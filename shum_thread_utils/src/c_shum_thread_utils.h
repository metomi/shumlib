#ifndef THREAD_UTILS_INC

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

#define THREAD_UTILS_INC

#include <stdint.h>

extern int64_t f_shum_threadFlush         (void);

extern int64_t f_shum_taskYield           (void);

extern int64_t f_shum_newLock             (void);

extern int64_t f_shum_releaseLock         (int64_t *);

extern int64_t f_shum_Lock                (int64_t *);

extern int64_t f_shum_TestLock            (int64_t *);

extern int64_t f_shum_unLock              (int64_t *);

extern int64_t f_shum_threadID            (void);

extern int64_t f_shum_inPar               (void);

extern int64_t f_shum_numThreads          (void);

extern void    f_shum_startOMPparallel    (void **,
                                           void (*)(void **const));

extern void    f_shum_startOMPparallelfor (void **,
                                           void (*)(void **const,
                                                 const int64_t *const restrict,
                                                 const int64_t *const restrict,
                                                 const int64_t *const restrict),
                                           const int64_t *,
                                           const int64_t *,
                                           const int64_t *);

extern int64_t f_shum_LockQueue           (int64_t *);

extern int64_t f_shum_barrier             (void);

#endif
