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

#if !defined(C_FRUIT_TEST_SHUM_THREAD_UTILS_H)

#define C_FRUIT_TEST_SHUM_THREAD_UTILS_H

/******************************************************************************/
/* Prototypes                                                                 */
/******************************************************************************/

extern void c_test_returns_valid_lock       (bool *);
extern void c_test_invalid_lock_release     (bool *);
extern void c_test_create_and_release_lock  (bool *);
extern void c_test_lock                     (bool *, int64_t *);
extern void c_test_release_locked_lock      (bool *);
extern void c_test_lock_invalid_lock        (bool *);
extern void c_test_create_many_locks        (bool *);
extern void c_test_backfill_locks           (bool *);
extern void c_test_unlock_already_unlocked  (bool *);
extern void c_test_sweep_release_locks      (bool *);
extern void c_test_lock_self_owned_lock     (bool *);
extern void c_test_unlock                   (bool *);
extern void c_test_unlock_invalid           (bool *);
extern void c_test_sweep_unlock_locks       (bool *);
extern void c_test_release_locked_testlock  (bool *);
extern void c_test_lock_self_owned_testlock (bool *);
extern void c_test_lock_invalid_testlock    (bool *);
extern void c_test_lock_testlock            (bool *);
extern void c_test_lockqueue_single         (bool *);
extern void c_test_lockqueue_invalid        (bool *);
extern void c_test_unlock_foreign           (bool *, int64_t *);
extern void c_test_lockqueue_multi          (bool *, int64_t *);
extern void c_test_lock_blocking            (bool *, int64_t *);
extern void c_test_testlock_nonblocking     (bool *, int64_t *);
extern void c_test_release_pending_locks    (bool *, int64_t *);
extern void c_test_create_single_lock       (bool *, int64_t *);
extern void c_test_inpar                    (bool *, int64_t *);
extern void c_test_threadid                 (bool *, int64_t *);
extern void c_test_threadflush              (bool *, volatile int64_t *);
extern void c_test_barrier                  (bool *, int64_t *);
extern void c_test_numthreads               (bool *, int64_t *);
extern void c_test_startOMPparallel         (bool *, int64_t *);
extern void c_test_startOMPparallelfor      (bool *,
                                             int64_t *,
                                             int64_t *,
                                             int64_t *,
                                             int64_t *);

#endif
