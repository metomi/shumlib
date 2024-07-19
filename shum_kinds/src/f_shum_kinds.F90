! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This file is part of the UM Shared Library project.
!
! The UM Shared Library is free software: you can redistribute it
! and/or modify it under the terms of the Modified BSD License, as
! published by the Open Source Initiative.
!
! The UM Shared Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty
! of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! Modified BSD License for more details.
!
! You should have received a copy of the Modified BSD License
! along with the UM Shared Library.
! If not, see <http://opensource.org/licenses/BSD-3-Clause>.
!
!*******************************************************************************
!
! Description : Module to define kinds used in Shumlib
!
MODULE f_shum_kinds_mod

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY:                                       &
  LOGICAL_KINDS, REAL_KINDS, INTEGER_KINDS, REAL32, REAL64

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_BOOL, c_int64_t, c_int32_t, c_int8_t

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  shum_logical64,                                                              &
  shum_logical32,                                                              &
  shum_logical8,                                                               &
  shum_lkinds,                                                                 &
  shum_real64,                                                                 &
  shum_real32,                                                                 &
  shum_rkinds,                                                                 &
  shum_int64,                                                                  &
  shum_int32,                                                                  &
  shum_int8,                                                                   &
  shum_ikinds

!------------------------------------------------------------------------------!
! LOGICAL KINDS                                                                !
!------------------------------------------------------------------------------!

#if defined(FORCE_LOGICALS)

#if !defined(FORCE_LOGICAL_64)
#define FORCE_LOGICAL_64 8
#endif

#if !defined(FORCE_LOGICAL_32)
#define FORCE_LOGICAL_32 4
#endif

#if !defined(FORCE_LOGICAL_8)
#define FORCE_LOGICAL_8 1
#endif

INTEGER, PARAMETER :: shum_logical64 = FORCE_LOGICAL_64
INTEGER, PARAMETER :: shum_logical32 = FORCE_LOGICAL_32
INTEGER, PARAMETER :: shum_logical8  = FORCE_LOGICAL_8

INTEGER, PARAMETER ::                                                          &
  s_lks = SIZE(LOGICAL_KINDS)

INTEGER, PARAMETER ::                                                          &
  shum_lkinds(s_lks) = LOGICAL_KINDS(1:s_lks)

#else

! Available Logical Kinds

! The method to determine Logical Kinds is somewhat convoluted in order to be
! compatible with multiple compilers.
!
! We will create an array of storage sizes based on the LOGICAL_KINDS array.
! We will then manually test if the size of each element matches out size
! requirement. By fixing the size at 10 elements, then abstracting the array
! indices, we can cope with differing numbers of logical kinds. (If a particular
! array index is too large, it is re-mapped to the first element, which is
! guaranteed to exist.) The assumption has been made that 10 elements is large
! enough to always hold all elements of LOGICAL_KINDS. However, if a system is
! encountered that requires more elements, this method is easy to extend.
!
! The manual method is verbose, but has no performance impact, as all values are
! calculated at compile time.

INTEGER, PARAMETER ::                                                          &
  s_lks = MERGE( SIZE(LOGICAL_KINDS), 10, (SIZE(LOGICAL_KINDS) < 10) )

INTEGER, PARAMETER :: &
  shum_lkinds(s_lks) = LOGICAL_KINDS(1:s_lks)

! Index Parameters for Logical Kind selection

INTEGER, PARAMETER ::                                                          &
  l_idx_1  = 1,                                                                &
  l_idx_2  = MERGE( 2,  1, 2<=s_lks ),                                         &
  l_idx_3  = MERGE( 3,  1, 3<=s_lks ),                                         &
  l_idx_4  = MERGE( 4,  1, 4<=s_lks ),                                         &
  l_idx_5  = MERGE( 5,  1, 5<=s_lks ),                                         &
  l_idx_6  = MERGE( 6,  1, 6<=s_lks ),                                         &
  l_idx_7  = MERGE( 7,  1, 7<=s_lks ),                                         &
  l_idx_8  = MERGE( 8,  1, 8<=s_lks ),                                         &
  l_idx_9  = MERGE( 9,  1, 9<=s_lks ),                                         &
  l_idx_10 = MERGE( 10, 1, 10<=s_lks )

INTEGER, PARAMETER ::                                                          &
  l_idxk_1  = LOGICAL_KINDS(l_idx_1),                                          &
  l_idxk_2  = LOGICAL_KINDS(l_idx_2),                                          &
  l_idxk_3  = LOGICAL_KINDS(l_idx_3),                                          &
  l_idxk_4  = LOGICAL_KINDS(l_idx_4),                                          &
  l_idxk_5  = LOGICAL_KINDS(l_idx_5),                                          &
  l_idxk_6  = LOGICAL_KINDS(l_idx_6),                                          &
  l_idxk_7  = LOGICAL_KINDS(l_idx_7),                                          &
  l_idxk_8  = LOGICAL_KINDS(l_idx_8),                                          &
  l_idxk_9  = LOGICAL_KINDS(l_idx_9),                                          &
  l_idxk_10 = LOGICAL_KINDS(l_idx_10)

! Temporary Array of storage sizes

INTEGER, PARAMETER ::                                                          &
  lkinds_ss_tmp(10) = [                                                        &
                       STORAGE_SIZE(.TRUE._l_idxk_1),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_2),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_3),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_4),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_5),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_6),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_7),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_8),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_9),                          &
                       STORAGE_SIZE(.TRUE._l_idxk_10)                          &
                      ]

INTEGER, PARAMETER ::                                                          &
  lk_idx_l64_1  = 1,                                                           &
  lk_idx_l64_2  = MERGE( 2,  lk_idx_l64_1, lkinds_ss_tmp(2) == 64 ),           &
  lk_idx_l64_3  = MERGE( 3,  lk_idx_l64_2, lkinds_ss_tmp(3) == 64 ),           &
  lk_idx_l64_4  = MERGE( 4,  lk_idx_l64_3, lkinds_ss_tmp(4) == 64 ),           &
  lk_idx_l64_5  = MERGE( 5,  lk_idx_l64_4, lkinds_ss_tmp(5) == 64 ),           &
  lk_idx_l64_6  = MERGE( 6,  lk_idx_l64_5, lkinds_ss_tmp(6) == 64 ),           &
  lk_idx_l64_7  = MERGE( 7,  lk_idx_l64_6, lkinds_ss_tmp(7) == 64 ),           &
  lk_idx_l64_8  = MERGE( 8,  lk_idx_l64_7, lkinds_ss_tmp(8) == 64 ),           &
  lk_idx_l64_9  = MERGE( 9,  lk_idx_l64_8, lkinds_ss_tmp(9) == 64 ),           &
  lk_idx_l64_10 = MERGE( 10, lk_idx_l64_9, lkinds_ss_tmp(10) == 64 )


INTEGER, PARAMETER ::                                                          &
  lk_idx_l32_1  = 1,                                                           &
  lk_idx_l32_2  = MERGE( 2,  lk_idx_l32_1, lkinds_ss_tmp(2) == 32 ),           &
  lk_idx_l32_3  = MERGE( 3,  lk_idx_l32_2, lkinds_ss_tmp(3) == 32 ),           &
  lk_idx_l32_4  = MERGE( 4,  lk_idx_l32_3, lkinds_ss_tmp(4) == 32 ),           &
  lk_idx_l32_5  = MERGE( 5,  lk_idx_l32_4, lkinds_ss_tmp(5) == 32 ),           &
  lk_idx_l32_6  = MERGE( 6,  lk_idx_l32_5, lkinds_ss_tmp(6) == 32 ),           &
  lk_idx_l32_7  = MERGE( 7,  lk_idx_l32_6, lkinds_ss_tmp(7) == 32 ),           &
  lk_idx_l32_8  = MERGE( 8,  lk_idx_l32_7, lkinds_ss_tmp(8) == 32 ),           &
  lk_idx_l32_9  = MERGE( 9,  lk_idx_l32_8, lkinds_ss_tmp(9) == 32 ),           &
  lk_idx_l32_10 = MERGE( 10, lk_idx_l32_9, lkinds_ss_tmp(10) == 32 )

! Logical Kind selection indices

INTEGER, PARAMETER :: lk_idx_l64 = lk_idx_l64_10
INTEGER, PARAMETER :: lk_idx_l32 = lk_idx_l32_10

! Logical Kind selection

INTEGER, PARAMETER :: shum_logical64 = LOGICAL_KINDS(lk_idx_l64)
INTEGER, PARAMETER :: shum_logical32 = LOGICAL_KINDS(lk_idx_l32)
INTEGER, PARAMETER :: shum_logical8  = C_BOOL

#endif

!------------------------------------------------------------------------------!
! REAL KINDS                                                                   !
!------------------------------------------------------------------------------!

! Real Kind selection

INTEGER, PARAMETER :: shum_real64 = REAL64
INTEGER, PARAMETER :: shum_real32 = REAL32

INTEGER, PARAMETER ::                                                          &
  s_rks = SIZE(REAL_KINDS)

INTEGER, PARAMETER ::                                                          &
  shum_rkinds(s_rks) = REAL_KINDS(1:s_rks)

!------------------------------------------------------------------------------!
! INTEGER KINDS                                                                !
!------------------------------------------------------------------------------!

! Integer Kind selection

INTEGER, PARAMETER :: shum_int64 = c_int64_t
INTEGER, PARAMETER :: shum_int32 = c_int32_t
INTEGER, PARAMETER :: shum_int8  = c_int8_t

INTEGER, PARAMETER ::                                                          &
  s_iks = SIZE(INTEGER_KINDS)

INTEGER, PARAMETER ::                                                          &
  shum_ikinds(s_iks) = INTEGER_KINDS(1:s_iks)

END MODULE f_shum_kinds_mod
