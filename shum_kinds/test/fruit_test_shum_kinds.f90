! *********************************COPYRIGHT************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file LICENCE.txt
! which you should have received as part of this distribution.
! *********************************COPYRIGHT************************************
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
!*******************************************************************************
MODULE fruit_test_shum_kinds_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_INT, C_BOOL


USE f_shum_kinds_mod, ONLY:                                                    &
     shum_logical64, shum_logical32, shum_logical8,                            &
     shum_lkinds,                                                              &
     shum_real64, shum_real32,                                                 &
     shum_rkinds,                                                              &
     shum_int64, shum_int32, shum_int8,                                        &
     shum_ikinds

IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_kinds

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
INTEGER, PARAMETER :: INT64  = C_INT64_T
INTEGER, PARAMETER :: INT32  = C_INT32_T
INTEGER, PARAMETER :: REAL64 = C_DOUBLE
INTEGER, PARAMETER :: bool   = C_BOOL
!------------------------------------------------------------------------------!

CONTAINS

SUBROUTINE fruit_test_shum_kinds

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
USE f_shum_kinds_version_mod, ONLY: get_shum_kinds_version

IMPLICIT NONE

INTEGER(KIND=INT64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_kinds_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_kinds at Shumlib version: ", version

CALL run_test_case(                                                            &
  test_shum_kinds, "kinds")

END SUBROUTINE fruit_test_shum_kinds

!------------------------------------------------------------------------------!

SUBROUTINE test_shum_kinds


IMPLICIT NONE

CALL assert_equals(STORAGE_SIZE(.TRUE._shum_logical64),                        &
                   64, "shum_logical64 storage size is wrong" )


CALL assert_equals(STORAGE_SIZE(.TRUE._shum_logical32),                        &
                   32, "shum_logical32 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(.TRUE._shum_logical8),                         &
                   8, "shum_logical8 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(1.0_shum_real64),                              &
                   64, "shum_real64 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(1.0_shum_real32),                              &
                   32, "shum_real32 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(1_shum_int64),                                 &
                   64, "shum_int64 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(1_shum_int32),                                 &
                   32, "shum_int32 storage size is wrong" )

CALL assert_equals(STORAGE_SIZE(1_shum_int8),                                  &
                   8, "shum_int8 storage size is wrong" )

END SUBROUTINE test_shum_kinds

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_kinds_mod
