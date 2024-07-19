/* *********************************COPYRIGHT**********************************/
/* (C) Crown copyright Met Office. All rights reserved.                       */
/* For further details please refer to the file LICENCE.txt                   */
/* which you should have received as part of this distribution.               */
/* *********************************COPYRIGHT**********************************/
/*                                                                            */
/* This file is part of the UM Shared Library project.                        */
/*                                                                            */
/* The UM Shared Library is free software: you can redistribute it            */
/* and/or modify it under the terms of the Modified BSD License, as           */
/* published by the Open Source Initiative.                                   */
/*                                                                            */
/* The UM Shared Library is distributed in the hope that it will be           */
/* useful, but WITHOUT ANY WARRANTY; without even the implied warranty        */
/* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* Modified BSD License for more details.                                     */
/*                                                                            */
/* You should have received a copy of the Modified BSD License                */
/* along with the UM Shared Library.                                          */
/* If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 */
/******************************************************************************/

#if !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200112L
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include "c_shum_thread_utils.h"
#include "c_fruit_test_shum_thread_utils.h"

#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
#include "omp.h"
#endif

#define tuft_signum(x) ((x > 0) - (x < 0))

/******************************************************************************/
/* prototypes                                                                 */
/******************************************************************************/

static void OMPparallel_dispatch(void **const);

static void OMPparallelfor_dispatch(void **const,
                                    const int64_t *const restrict,
                                    const int64_t *const restrict,
                                    const int64_t *const restrict);

typedef struct DISPATCH_PACK {
  bool test_ret;
  int64_t istart;
  int64_t iend;
  int64_t incr;
  int64_t threads;
  size_t exp_iter;
  size_t *act_iter;
  size_t tot_act_iter;
  int64_t **local_iters;
} dispatch_pack;

/******************************************************************************/
/* tests                                                                      */
/******************************************************************************/

void c_test_returns_valid_lock(bool *test_ret)
{
  int64_t lock = -1;
  *test_ret = false;

  lock = f_shum_newLock();

  *test_ret = ( (lock>0)
                && (lock<INT64_MAX)
              );

}

/******************************************************************************/

void c_test_invalid_lock_release(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;
  *test_ret = false;

  res = f_shum_releaseLock(&lock);

  *test_ret = (res!=0);

}

/******************************************************************************/

void c_test_lockqueue_invalid(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;
  *test_ret = false;

  res = f_shum_LockQueue(&lock);

  *test_ret = (res==0);

}

/******************************************************************************/

void c_test_unlock_foreign(bool *test_ret, int64_t *lock)
{
  int64_t res = 0;
  *test_ret = false;

  int64_t tid = f_shum_threadID();

  /* trigger valiables are used to provide (weak) synchronisation */
  static volatile sig_atomic_t trigger1 = 0;
  static volatile sig_atomic_t trigger2 = 0;

  if (tid==0)
  {
    res = f_shum_Lock(lock);
    trigger1 = 1;
    f_shum_threadFlush();
  }

  while(trigger1!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==1)
  {
    res = f_shum_unLock(lock);
    trigger2 = 1;
    f_shum_threadFlush();
  }

  while(trigger2!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==0)
  {
    res = f_shum_unLock(lock);
  }


  switch(tid)
  {
    case 0:
      *test_ret = (res==0);
      break;

    case 1:
      *test_ret = (res!=0);
      break;

    default:
      *test_ret = false;
  }

}

/******************************************************************************/

void c_test_lock_blocking(bool *test_ret, int64_t *lock)
{
  int64_t res = 0;
  *test_ret = false;

  int64_t tid = f_shum_threadID();

  /* trigger valiables are used to provide (weak) synchronisation */
  static volatile sig_atomic_t trigger1 = 0;
  static volatile sig_atomic_t trigger2 = 0;
  static volatile sig_atomic_t trigger3 = 0;

  if (tid==0)
  {
    res += f_shum_Lock(lock);
    trigger1 = 1;
    f_shum_threadFlush();
  }

  while(trigger1!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==1)
  {
    trigger3 = 1;
    f_shum_threadFlush();
    res += f_shum_Lock(lock);
    f_shum_threadFlush();
    /* Test if trigger2 has not been set...
     * if it has not, we have got there too fast,
     * and so mustn't have blocked at the lock.
     */
    if (trigger2==0)
    {
      res += 1;
    }
  }

  while(trigger2!=1 && tid!=2)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==2)
  {
    while(trigger3!=1)
    {
      /* spin */
      f_shum_threadFlush();
    }
    f_shum_threadFlush();
    sleep(1);
    trigger2 = 1;
    f_shum_threadFlush();
  }
  else
  {
    res += f_shum_unLock(lock);
  }

  switch(tid)
  {
    case 0:
    case 1:
    case 2:
      *test_ret = (res==0);
      break;

    default:
      *test_ret = false;
  }

}

/******************************************************************************/

void c_test_lockqueue_multi(bool *test_ret, int64_t *lock)
{
  int64_t res = 0;
  int64_t resx = 0;
  uint64_t breaker =0x0000000000000000;
  *test_ret = false;

  int64_t tid = f_shum_threadID();

  /* trigger valiables are used to provide (weak) synchronisation */
  static volatile sig_atomic_t trigger1 = 0;
  static volatile sig_atomic_t trigger2 = 0;
  static volatile sig_atomic_t trigger3 = 0;
  static volatile sig_atomic_t trigger4 = 0;

  /* this should add zero, as there are no locks */
  res += f_shum_LockQueue(lock);

  if (tid==1)
  {
    trigger1 = 1;
    f_shum_threadFlush();
  }

  if (tid==2)
  {
    trigger2 = 1;
    f_shum_threadFlush();
  }

  if (tid==0)
  {
    while(trigger1!=1)
    {
      /* spin */
      f_shum_threadFlush();
    }
    f_shum_threadFlush();
    while(trigger2!=1)
    {
      /* spin */
      f_shum_threadFlush();
    }
    f_shum_threadFlush();
    res += f_shum_Lock(lock);
    trigger3 = 1;
    f_shum_threadFlush();
  }

  while(trigger3!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }

  f_shum_threadFlush();

  if (tid==1)
  {
    res += f_shum_Lock(lock);
    f_shum_threadFlush();
  }

  if (tid==2)
  {
    while(f_shum_LockQueue(lock)!=1)
    {
      /* spin */
      f_shum_threadFlush();
    }
    f_shum_threadFlush();
    trigger4 = 1;
    f_shum_threadFlush();
    res += f_shum_Lock(lock);
  }

  while(trigger4!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  /* there is a race here between thread 0 and 2. */
  while(tid==0 && f_shum_LockQueue(lock)!=2)
  {
    /* spin */
    breaker++;
    if(breaker==0xFFFFFFFFFFFFFFFF)
    {
      /* prevent a deadlock, but if we get here the test will fail */
      resx = 1;
      break;
    }
  }

  /* there is a race here between thread 1 and 2. */
  while(tid==1 && f_shum_LockQueue(lock)!=1)
  {
    /* spin */
    breaker++;
    if(breaker==0xFFFFFFFFFFFFFFFF)
    {
      /* prevent a deadlock, but if we get here the test will fail */
      resx = 1;
      break;
    }
  }

  /* there is a race here between thread 1 and 2. */
  if ( tid==2 )
  {
    while(f_shum_LockQueue(lock)==1)
    {
      /* ensure lock release is ordered */
      resx += f_shum_unLock(lock);
      resx += f_shum_Lock(lock);
    }
  }

  res += f_shum_LockQueue(lock);

  res += f_shum_unLock(lock);

  switch(tid)
  {
    case 0:
      *test_ret = (res==2) && (resx==0);
      break;

    case 1:
      *test_ret = (res==1) && (resx==0);
      break;

    case 2:
      *test_ret = (res==0) && (resx==0);
      break;

    default:
      *test_ret = false;
  }

}

/******************************************************************************/

void c_test_testlock_nonblocking(bool *test_ret, int64_t *lock)
{
  int64_t res = 0;
  *test_ret = false;

  int64_t tid = f_shum_threadID();

  /* trigger valiables are used to provide (weak) synchronisation */
  static volatile sig_atomic_t trigger1 = 0;
  static volatile sig_atomic_t trigger2 = 0;
  static volatile sig_atomic_t trigger3 = 0;

  if (tid==0)
  {
    res += f_shum_Lock(lock);
    trigger1 = 1;
    f_shum_threadFlush();
  }

  while(trigger1!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==1)
  {
    int64_t res_capture =0;
    trigger3 = 1;
    f_shum_threadFlush();
    res_capture = f_shum_TestLock(lock);
    f_shum_threadFlush();
    /* Test if trigger2 has not been set...
     * if it has, we have got there too slowly,
     * and so probably have blocked at the lock.
     */
    if (trigger2!=0)
    {
      res += 1;
    }

    f_shum_threadFlush();

    if (res_capture==-1)
    {
      res += res_capture;
    }
  }

  while(trigger2!=1 && tid!=2)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==2)
  {
    while(trigger3!=1)
    {
      /* spin */
      f_shum_threadFlush();
    }
    f_shum_threadFlush();
    sleep(1);
    trigger2 = 1;
    f_shum_threadFlush();
  }
  else
  {
    res += f_shum_unLock(lock);
  }

  switch(tid)
  {
    case 0:
    case 1:
    case 2:
      *test_ret = (res==0);
      break;

    default:
      *test_ret = false;
  }

}

/******************************************************************************/

void c_test_create_and_release_lock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_releaseLock(&lock);
  res2 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0);

}

/******************************************************************************/

void c_test_create_single_lock(bool *test_ret, int64_t *lock)
{
  int64_t res1 = 0;

  *test_ret = false;

  *lock = f_shum_newLock();

  *test_ret = (res1==0);
}

/******************************************************************************/

void c_test_create_many_locks(bool *test_ret)
{
  int64_t lock = -1;
  int i;

  *test_ret = true;

  for (i=0;i<1000;i++)
  {
    lock = f_shum_newLock();

    *test_ret = ( *test_ret
                  && (lock>0)
                  && (lock<INT64_MAX)
                );

    if (!*test_ret) break;
  }
}

/******************************************************************************/

void c_test_backfill_locks(bool *test_ret)
{
  int64_t lock[4] = {-1,-1,-1,-1};
  int64_t prev_lock = -1;
  int64_t res=0;
  int i;

  *test_ret = true;

  lock[0] = f_shum_newLock();

  for (i=1;i<4;i++)
  {
    lock[i] = f_shum_newLock();

    *test_ret = ( *test_ret
                  && (lock[i]>lock[i-1])
                );

    if (!*test_ret) break;
  }

  prev_lock = lock[2];

  res = f_shum_releaseLock(&lock[2]);

  *test_ret = *test_ret && (res==0);

  lock[2] = f_shum_newLock();

  *test_ret = *test_ret && (lock[2]==prev_lock);
}

/******************************************************************************/

void c_test_sweep_release_locks(bool *test_ret)
{
  int64_t res=0;
  int64_t i;
  int64_t max;

  *test_ret = true;

  max = f_shum_newLock();

  if (max<2500)
  {
    max=2500;
  }

  for (i=0;i<max;i++)
  {
    res = f_shum_releaseLock(&i);

    *test_ret = ( *test_ret
                  && (res==1||res==0)
                );

    if (!*test_ret) break;
  }

}

/******************************************************************************/

void c_test_sweep_unlock_locks(bool *test_ret)
{
  int64_t res=0;
  int64_t i;
  int64_t max;

  *test_ret = true;

  max = f_shum_newLock();

  if (max<2500)
  {
    max=2500;
  }

  for (i=0;i<max;i++)
  {
    res = f_shum_unLock(&i);

    *test_ret = ( *test_ret
                  && (res==1||res==0)
                );

    if (!*test_ret) break;
  }

}

/******************************************************************************/

void c_test_lock(bool *test_ret, int64_t *lock)
{
  int64_t res = 0;

  *test_ret = false;

  *lock = f_shum_newLock();

  res = f_shum_Lock(lock);

  *test_ret = (res==0);
}

/******************************************************************************/

void c_test_release_pending_locks(bool *test_ret, int64_t *lock)
{
  /* trigger valiables are used to provide (weak) synchronisation */
  static volatile sig_atomic_t trigger1 = 0;
  static volatile sig_atomic_t trigger2 = 0;
  static volatile sig_atomic_t trigger3 = 0;
  static volatile sig_atomic_t trigger4 = 0;
  static volatile sig_atomic_t trigger5 = 0;

  /* result comparison variables */
  static volatile sig_atomic_t res3t1 = 0;
  static volatile sig_atomic_t res3t2 = 0;

  int64_t res1 = 0;
  int64_t res2 = 0;
  int64_t res3 = 0;
  bool res4 = false;

  int64_t tid = f_shum_threadID();

  *test_ret = false;

  if (tid==0)
  {
    res1 = f_shum_Lock(lock);
    trigger1 = 1;
    f_shum_threadFlush();
  }

  while(trigger1!=1)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  if (tid==1)
  {
    res1 = f_shum_Lock(lock);
  }

  if (tid==2)
  {
    while(f_shum_LockQueue(lock)!=1)
    {
      /* spin */
    }
    f_shum_threadFlush();
    res1 = f_shum_Lock(lock);
  }

  while(trigger2!=1 && tid!=0)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  while(f_shum_LockQueue(lock)!=2-tid && f_shum_inPar())
  {
    /* spin */
    if (f_shum_LockQueue(lock)==1 && tid==2)
    {
      /* ensure lock release is ordered */
      res2 = f_shum_unLock(lock);
      res1 = f_shum_Lock(lock);
    }
  }
  f_shum_threadFlush();

  while(trigger3!=1 && tid==2)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  res2 = f_shum_unLock(lock);
  /* there is a potential race to this point, so threads 1 and 2
   * could both have unlocked by the time we get to the release.
   * This means the succesfull thread could be 1 or 2 (but not both).
   */
  res3 = f_shum_releaseLock(lock);

  if (tid==0)
  {
    trigger2 = 1;
    f_shum_threadFlush();
  }

  if (tid==1)
  {
    trigger3 = 1;
    trigger4 = 1;
    res3t1 = (sig_atomic_t)res3;
    f_shum_threadFlush();
  }

  if (tid==2)
  {
    res3t2 = (sig_atomic_t)res3;
    trigger5 = 1;
    f_shum_threadFlush();
  }

  while(trigger4!=1 && tid!=0)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  while(trigger5!=1 && tid!=0)
  {
    /* spin */
    f_shum_threadFlush();
  }
  f_shum_threadFlush();

  f_shum_threadFlush();

  /* check the results were what we expected */
  if (!f_shum_inPar())
  {
    res4=true;
  }
  else
  {
    switch(tid)
    {
      case 0:
        res4=(res3!=0);
        break;

      case 1:
      case 2:
      {
        int64_t ires3t1, ires3t2;

        ires3t1 = (int)res3t1;
        ires3t2 = (int)res3t2;

        res4 = (res3==0) || (res3==1);
        res4 = res4 && (ires3t1!=ires3t2);
        break;
      }

      default:
        res4=false;
        break;
    }
  }

  *test_ret = (res1==0 && res2==0 && res4==true);
}

/******************************************************************************/

void c_test_lock_testlock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res = f_shum_TestLock(&lock);

  *test_ret = (res==0);
}


/******************************************************************************/

void c_test_unlock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_Lock(&lock);
  res2 = f_shum_unLock(&lock);

  *test_ret = (res1==0 && res2==0);
}

/******************************************************************************/

void c_test_unlock_invalid(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;

  *test_ret = false;

  res = f_shum_unLock(&lock);

  *test_ret = (res!=0);
}

/******************************************************************************/

void c_test_lock_invalid_lock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res = f_shum_Lock(&lock);

  *test_ret = (res==0);
}

/******************************************************************************/

void c_test_lock_invalid_testlock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res = f_shum_TestLock(&lock);

  *test_ret = (res==0);
}

/******************************************************************************/

void c_test_unlock_already_unlocked(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;
  int64_t res3 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_Lock(&lock);
  res2 = f_shum_unLock(&lock);
  res3 = f_shum_unLock(&lock);

  *test_ret = (res1==0 && res2==0 && res3!=0);
}

/******************************************************************************/

void c_test_release_locked_lock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_Lock(&lock);
  res2 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0);
}

/******************************************************************************/

void c_test_release_locked_testlock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_TestLock(&lock);
  res2 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0);
}

/******************************************************************************/

void c_test_lock_self_owned_lock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;
  int64_t res3 = 0;
  int64_t res4 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_Lock(&lock);
  res2 = f_shum_Lock(&lock);

  res3 = f_shum_unLock(&lock);

  res4 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0 && res3==0 && res4==0);
}


/******************************************************************************/

void c_test_lockqueue_single(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;
  int64_t res3 = 0;
  int64_t res4 = 0;
  int64_t res5 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_LockQueue(&lock);

  res2 = f_shum_Lock(&lock);

  res3 = f_shum_LockQueue(&lock);

  res4 = f_shum_unLock(&lock);

  res5 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2==0 && res3==0 && res4==0 && res5==0);
}

/******************************************************************************/

void c_test_lock_self_owned_testlock(bool *test_ret)
{
  int64_t lock = -1;
  int64_t res1 = 0;
  int64_t res2 = 0;
  int64_t res3 = 0;
  int64_t res4 = 0;

  *test_ret = false;

  lock = f_shum_newLock();

  res1 = f_shum_TestLock(&lock);
  res2 = f_shum_TestLock(&lock);

  res3 = f_shum_unLock(&lock);

  res4 = f_shum_releaseLock(&lock);

  *test_ret = (res1==0 && res2!=0 && res3==0 && res4==0);
}


/******************************************************************************/

void c_test_inpar(bool *test_ret, int64_t *par_res)
{
  *par_res = f_shum_inPar();
#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
  *test_ret = (*par_res == omp_in_parallel());
#else
  *test_ret = (*par_res==1 || *par_res==0);
#endif
}

/******************************************************************************/

void c_test_threadid(bool *test_ret, int64_t *tid)
{
  *tid = f_shum_threadID();
#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
  *test_ret = (*tid == omp_get_thread_num());
#else
  *test_ret = (*tid >= 0);
#endif
}

/******************************************************************************/

void c_test_numthreads(bool *test_ret, int64_t *numthreads)
{
  *numthreads = f_shum_numThreads();
#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
  *test_ret = (*numthreads == omp_get_num_threads());
#else
  *test_ret = (*numthreads >= 1);
#endif
}

/******************************************************************************/

void c_test_threadflush(bool *test_ret, volatile int64_t *shared1)
{
  /* this is a dummy test for now */

  *test_ret = (*shared1==0);
}

/******************************************************************************/

void c_test_barrier(bool *test_ret, int64_t *shared1)
{
  int64_t tid = f_shum_threadID();

  struct timespec sleep_one = { 1, 0 };

  const int64_t inpar = f_shum_inPar();

  *test_ret = (inpar==f_shum_barrier());

  if (tid==0)
  {
    shared1[0] = 1;
  }
  else
  {
    while (nanosleep(&sleep_one, NULL))
    {
      /* repeat until we sleep uninterupted */
    }
  }

  *test_ret &= (inpar==f_shum_barrier());

  *test_ret &= (shared1[0] + shared1[1] + shared1[2] == 1);

  *test_ret &= (inpar==f_shum_barrier());

  shared1[tid] = 2;

  if (tid!=0)
  {
    while (nanosleep(&sleep_one, NULL))
    {
      /* repeat until we sleep uninterupted */
    }
  }

  if (tid==2)
  {
    while (nanosleep(&sleep_one, NULL))
    {
      /* repeat until we sleep uninterupted */
    }
  }

  *test_ret &= (inpar==f_shum_barrier());

  *test_ret &= (shared1[0] + shared1[1] + shared1[2] == 2*f_shum_numThreads());

}

/******************************************************************************/

void c_test_startOMPparallel(bool *test_ret, int64_t *threads)
{
  dispatch_pack pack;
  dispatch_pack *pack_ptr = &pack;

  pack.test_ret = 0;
  pack.threads = *threads;

  f_shum_startOMPparallel((void **)&pack_ptr,
                          &OMPparallel_dispatch);

  *test_ret = pack.test_ret;
}


/******************************************************************************/

void c_test_startOMPparallelfor(bool *test_ret, int64_t *in_istart,
                                int64_t *in_iend, int64_t *in_incr,
                                int64_t *threads)
{
  dispatch_pack pack;
  dispatch_pack *pack_ptr = &pack;
  int64_t i;
  int64_t *itr_ptr;
  size_t itr_ptr_thread;

  /* Initialise variables */

  pack.test_ret = 0;
  pack.istart = *in_istart;
  pack.iend = *in_iend;
  pack.incr = *in_incr;
  pack.threads = *threads;

  pack.exp_iter = 0;
  pack.tot_act_iter = 0;
  pack.act_iter = calloc((size_t)*threads,sizeof(*(pack.act_iter)));
  pack.local_iters = calloc((size_t)*threads,sizeof(*(pack.local_iters)));

  /* Check the total iteration count */

  for(i = *in_istart;
      tuft_signum(*in_incr)*i <= tuft_signum(*in_incr)*(*in_iend);
      i = i+*in_incr)
  {
    pack.exp_iter += 1;
  }

  /* Execute the parallelfor */

  f_shum_startOMPparallelfor((void **)&pack_ptr,
                             &OMPparallelfor_dispatch,
                             in_istart,
                             in_iend,
                             in_incr);

  /* Check the thread division was balanced */
  for(i = 1; i < *threads; i++)
  {
    if (pack.act_iter[0] - pack.act_iter[i] > 1)
    {
      pack.test_ret = 0;
    }
    if (pack.act_iter[i] != 0 && pack.act_iter[i-1] == 0)
    {
      pack.test_ret = 0;
    }
  }

  /* Check the sum of local iteration counts is the total count */

  for(i = 0; i < *threads; i++)
  {
    pack.tot_act_iter += pack.act_iter[i];
  }

  if (pack.exp_iter != pack.tot_act_iter)
  {
    pack.test_ret = 0;
  }

  /* the next test assumes the iteration bounds are correct, so abort early if
   * we have already failed
   */
  *test_ret = pack.test_ret;
  if (!*test_ret)
  {
    return;
  }

  /* Check the actual iterations were correct */

  itr_ptr_thread=0;

  itr_ptr = pack.local_iters[itr_ptr_thread];

  for(i = *in_istart;
      tuft_signum(*in_incr)*i <= tuft_signum(*in_incr)*(*in_iend);
      i = i+*in_incr)
  {

    if (*itr_ptr != i)
    {
      pack.test_ret = 0;
      break;
    }
    itr_ptr++;

    pack.act_iter[itr_ptr_thread]--;

    if(pack.act_iter[itr_ptr_thread]==0)
    {
      itr_ptr_thread++;
      itr_ptr = pack.local_iters[itr_ptr_thread];
    }

  }

  *test_ret = pack.test_ret;
}

/******************************************************************************/

static void OMPparallel_dispatch(void **const shareptr)
{
  dispatch_pack *pack = (dispatch_pack *)*shareptr;

  /* Check we have a parallel region */

  pack->test_ret = f_shum_inPar() || (pack->threads == 1);
}

/******************************************************************************/

static void OMPparallelfor_dispatch(void **const shareptr,
                                    const int64_t *const restrict istart,
                                    const int64_t *const restrict iend,
                                    const int64_t *const restrict incr)
{
  dispatch_pack *pack = (dispatch_pack *)*shareptr;
  int64_t i;
  int64_t *itr_ptr;

  size_t tid = (size_t)f_shum_threadID();

  /* Check we have a parallel region */

  OMPparallel_dispatch(shareptr);

  /* Check the iteration limits */

  if (*incr>0)
  {
    if (*istart>*iend)
    {
      /* increment is +ve, but istart is larger than iend.
       * This may be on purpose (to cause zero iterations on this thread) if
       * there are more threads than iterations.
       */
      if ((*iend-*istart) > pack->threads && pack->exp_iter != 0)
      {
        pack->test_ret = 0;
      }
    }
  }
  else if (*incr<0)
  {
    if (*istart<*iend)
    {
      /* increment is -ve, but iend is larger than istart.
       * This may be on purpose (to cause zero iterations on this thread) if
       * there are more threads than iterations.
       */
      if ((*iend-*istart) > pack->threads && pack->exp_iter != 0)
      {
       pack->test_ret = 0;
      }
    }
  }

  /* Check the local iteration count */

  for(i = *istart;
      tuft_signum(*incr)*i <= tuft_signum(*incr)*(*iend);
      i = i+*incr)
  {
    pack->act_iter[tid] += 1;
  }

  /* Check the actual iterations */

  pack->local_iters[tid] = calloc(pack->act_iter[tid],
                                  sizeof(*(pack->local_iters[tid])));

  itr_ptr =  pack->local_iters[tid];

  for(i = *istart;
      tuft_signum(*incr)*i <= tuft_signum(*incr)*(*iend);
      i = i+*incr)
  {
    *itr_ptr = i;
    itr_ptr++;
  }

}
