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

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "c_shum_thread_utils.h"
#include "c_fruit_test_shum_thread_utils.h"

#if defined(_OPENMP)
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

void c_test_inpar(bool *test_ret, int64_t *par_res)
{
  *par_res = f_shum_inPar();
#if defined(_OPENMP)
  *test_ret = (*par_res == omp_in_parallel());
#else
  *test_ret = (*par_res==1 || *par_res==0);
#endif
}

/******************************************************************************/

void c_test_threadid(bool *test_ret, int64_t *tid)
{
  *tid = f_shum_threadID();
#if defined(_OPENMP)
  *test_ret = (*tid == omp_get_thread_num());
#else
  *test_ret = (*tid >= 0);
#endif
}

/******************************************************************************/

void c_test_numthreads(bool *test_ret, int64_t *numthreads)
{
  *numthreads = f_shum_numThreads();
#if defined(_OPENMP)
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
