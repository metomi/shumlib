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
MODULE fruit_test_shum_number_tools_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_INT, C_BOOL

IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_number_tools

!------------------------------------------------------------------------------!
! Interfaces to call C routines                                                !
!------------------------------------------------------------------------------!

INTERFACE
  FUNCTION c_test_generate_finf()  BIND(c,NAME="c_test_generate_finf")
  IMPORT :: C_FLOAT
  IMPLICIT NONE
  REAL(KIND=C_FLOAT) :: c_test_generate_finf
  END FUNCTION
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
  FUNCTION c_test_generate_dinf()  BIND(c,NAME="c_test_generate_dinf")
  IMPORT :: C_DOUBLE
  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: c_test_generate_dinf
  END FUNCTION
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
  FUNCTION c_test_generate_fnan()  BIND(c,NAME="c_test_generate_fnan")
  IMPORT :: C_FLOAT
  IMPLICIT NONE
  REAL(KIND=C_FLOAT) :: c_test_generate_fnan
  END FUNCTION
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
  FUNCTION c_test_generate_dnan()  BIND(c,NAME="c_test_generate_dnan")
  IMPORT :: C_DOUBLE
  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: c_test_generate_dnan
  END FUNCTION
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
  SUBROUTINE c_test_generate_fdenormal(denormal_float)                         &
                                        BIND(c,NAME="c_test_generate_fdenormal")
  IMPORT :: C_FLOAT
  IMPLICIT NONE
  REAL(KIND=C_FLOAT) :: denormal_float
  END SUBROUTINE
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
  SUBROUTINE c_test_generate_ddenormal(denormal_double)                        &
                                        BIND(c,NAME="c_test_generate_ddenormal")
  IMPORT :: C_DOUBLE
  IMPLICIT NONE
  REAL(KIND=C_DOUBLE) :: denormal_double
  END SUBROUTINE
END INTERFACE

!------------------------------------------------------------------------------!
! We are going to use the types from the ISO_C_BINDING module, since although  !
! the REALs are not 100% guaranteed to correspond to the sizes we want to      !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
INTEGER, PARAMETER :: INT64  = C_INT64_T
INTEGER, PARAMETER :: INT32  = C_INT32_T
INTEGER, PARAMETER :: REAL32 = C_FLOAT
INTEGER, PARAMETER :: REAL64 = C_DOUBLE
INTEGER, PARAMETER :: bool   = C_BOOL
!------------------------------------------------------------------------------!

CONTAINS

SUBROUTINE fruit_test_shum_number_tools

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_number_tools_version_mod, ONLY: get_shum_number_tools_version

IMPLICIT NONE

INTEGER(KIND=INT64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway its
! sufficient to make sure it is callable; but lets print the version for info.
version = get_shum_number_tools_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing f_shum_number_tools at Shumlib version: ", version

CALL run_test_case(test_nan32, "test_nan32")

CALL run_test_case(test_nan64, "test_nan64")

CALL run_test_case(test_inf32, "test_inf32")

CALL run_test_case(test_inf64, "test_inf64")

CALL run_test_case(test_denormal32, "test_denormal32")

CALL run_test_case(test_denormal64, "test_denormal64")

#if defined(HAS_IEEE_ARITHMETIC)
CALL run_test_case(                                                            &
      test_ieee_arithmetic, "test_ieee_arithmetic")
#endif

END SUBROUTINE fruit_test_shum_number_tools

!------------------------------------------------------------------------------!

SUBROUTINE test_nan32

USE f_shum_is_nan_mod, ONLY: f_shum_is_nan, f_shum_has_nan

IMPLICIT NONE

REAL(KIND=REAL32) :: nan32, array32_1d(2), array32_2d(2,2), array32_3d(2,2,2), &
                     array32_4d(2,2,2,2), array32_5d(2,2,2,2,2)

REAL(KIND=C_FLOAT) :: denormal_float

LOGICAL(KIND=bool) :: equality_test

nan32 = c_test_generate_fnan()

equality_test = (nan32 == 0.0_real32)

CALL assert_false(equality_test,                                               &
                   "32-bit NaN is not supported on platform")

equality_test = (nan32 == c_test_generate_fnan())

CALL assert_false(equality_test,                                               &
                  "32-bit NaN is equal to itself")

equality_test = f_shum_is_nan(0.0_real32)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_nan incorrectly detected a NaN")

equality_test = f_shum_is_nan(c_test_generate_finf())

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_nan confused NaN and Inf")

denormal_float = 0.0
CALL c_test_generate_fdenormal(denormal_float)

equality_test = f_shum_is_nan(denormal_float)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_nan confused NaN and denormal number")

array32_1d = 0.0
equality_test = f_shum_has_nan(array32_1d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_nan incorrectly detected a 1D NaN")

array32_2d = 0.0
equality_test = f_shum_has_nan(array32_2d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_nan incorrectly detected a 2D NaN")

array32_3d = 0.0
equality_test = f_shum_has_nan(array32_3d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_nan incorrectly detected a 3D NaN")

array32_4d = 0.0
equality_test = f_shum_has_nan(array32_4d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_nan incorrectly detected a 4D NaN")

array32_5d = 0.0
equality_test = f_shum_has_nan(array32_5d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_nan incorrectly detected a 5D NaN")

equality_test = f_shum_is_nan(nan32)

CALL assert_true(equality_test,                                                &
                  "32-bit f_shum_is_nan failed to detect a NaN")

array32_1d(2) = nan32
equality_test = f_shum_has_nan(array32_1d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 1D NaN")

array32_2d(2,2) = nan32
equality_test = f_shum_has_nan(array32_2d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 2D NaN")

array32_3d(2,2,2) = nan32
equality_test = f_shum_has_nan(array32_3d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 3D NaN")

array32_4d(2,2,2,2) = nan32
equality_test = f_shum_has_nan(array32_4d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 4D NaN")

array32_5d(2,2,2,2,2) = nan32
equality_test = f_shum_has_nan(array32_5d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 5D NaN")

END SUBROUTINE test_nan32

!------------------------------------------------------------------------------!

SUBROUTINE test_nan64

USE f_shum_is_nan_mod, ONLY: f_shum_is_nan, f_shum_has_nan

IMPLICIT NONE

REAL(KIND=REAL64) :: nan64, array64_1d(2), array64_2d(2,2), array64_3d(2,2,2), &
                     array64_4d(2,2,2,2), array64_5d(2,2,2,2,2)

REAL(KIND=C_DOUBLE) :: denormal_double

LOGICAL(KIND=bool) :: equality_test

nan64 = c_test_generate_dnan()

equality_test = (nan64 == 0.0_real64)

CALL assert_false(equality_test,                                               &
                   "64-bit NaN is not supported on platform")

equality_test = (nan64 == c_test_generate_dnan())

CALL assert_false(equality_test,                                               &
                  "64-bit NaN is equal to itself")

equality_test = f_shum_is_nan(0.0_real64)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_nan incorrectly detected a NaN")

equality_test = f_shum_is_nan(c_test_generate_dinf())

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_nan confused NaN and Inf")

denormal_double = 0.0
CALL c_test_generate_ddenormal(denormal_double)

equality_test = f_shum_is_nan(denormal_double)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_nan confused NaN and denormal number")

array64_1d = 0.0
equality_test = f_shum_has_nan(array64_1d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_nan incorrectly detected a 1D NaN")

array64_2d = 0.0
equality_test = f_shum_has_nan(array64_2d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_nan incorrectly detected a 2D NaN")

array64_3d = 0.0
equality_test = f_shum_has_nan(array64_3d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_nan incorrectly detected a 3D NaN")

array64_4d = 0.0
equality_test = f_shum_has_nan(array64_4d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_nan incorrectly detected a 4D NaN")

array64_5d = 0.0
equality_test = f_shum_has_nan(array64_5d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_nan incorrectly detected a 5D NaN")

equality_test = f_shum_is_nan(nan64)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_is_nan failed to detect a NaN")

array64_1d(2) = nan64
equality_test = f_shum_has_nan(array64_1d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_nan failed to detect a 1D NaN")

array64_2d(2,2) = nan64
equality_test = f_shum_has_nan(array64_2d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_nan failed to detect a 2D NaN")

array64_3d(2,2,2) = nan64
equality_test = f_shum_has_nan(array64_3d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_nan failed to detect a 3D NaN")

array64_4d(2,2,2,2) = nan64
equality_test = f_shum_has_nan(array64_4d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_nan failed to detect a 4D NaN")

array64_5d(2,2,2,2,2) = nan64
equality_test = f_shum_has_nan(array64_5d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_nan failed to detect a 5D NaN")

END SUBROUTINE test_nan64

!------------------------------------------------------------------------------!

SUBROUTINE test_inf32

USE f_shum_is_inf_mod, ONLY: f_shum_is_inf, f_shum_has_inf

IMPLICIT NONE

REAL(KIND=REAL32) :: inf32, array32_1d(2), array32_2d(2,2), array32_3d(2,2,2), &
                     array32_4d(2,2,2,2), array32_5d(2,2,2,2,2)

REAL(KIND=C_FLOAT) :: denormal_float

LOGICAL(KIND=bool) :: equality_test

inf32 = c_test_generate_finf()

CALL assert_not_equals(0.0_real32, inf32,                                      &
                       "32-bit Inf is not supported on platform")

equality_test = f_shum_is_inf(0.0_real32)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_inf incorrectly detected an Inf")

equality_test = f_shum_is_inf(c_test_generate_fnan())

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_inf confused Inf and NaN")

denormal_float = 0.0
CALL c_test_generate_fdenormal(denormal_float)

equality_test = f_shum_is_inf(denormal_float)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_inf confused Inf and denormal number")

array32_1d = 0.0
equality_test = f_shum_has_inf(array32_1d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_inf incorrectly detected a 1D Inf")

array32_2d = 0.0
equality_test = f_shum_has_inf(array32_2d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_inf incorrectly detected a 2D Inf")

array32_3d = 0.0
equality_test = f_shum_has_inf(array32_3d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_inf incorrectly detected a 3D Inf")

array32_4d = 0.0
equality_test = f_shum_has_inf(array32_4d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_inf incorrectly detected a 4D Inf")

array32_5d = 0.0
equality_test = f_shum_has_inf(array32_5d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_inf incorrectly detected a 5D Inf")

equality_test = f_shum_is_inf(inf32)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect an Inf")


array32_1d(2) = inf32
equality_test = f_shum_has_inf(array32_1d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect a 1D Inf")

array32_2d(2,2) = inf32
equality_test = f_shum_has_inf(array32_2d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect a 2D Inf")

array32_3d(2,2,2) = inf32
equality_test = f_shum_has_inf(array32_3d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect a 3D Inf")

array32_4d(2,2,2,2) = inf32
equality_test = f_shum_has_inf(array32_4d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect a 4D Inf")

array32_5d(2,2,2,2,2) = inf32
equality_test = f_shum_has_inf(array32_5d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_inf failed to detect a 5D Inf")

END SUBROUTINE test_inf32

!------------------------------------------------------------------------------!

SUBROUTINE test_inf64

USE f_shum_is_inf_mod, ONLY: f_shum_is_inf, f_shum_has_inf

IMPLICIT NONE

REAL(KIND=REAL64) :: inf64, array64_1d(2), array64_2d(2,2), array64_3d(2,2,2), &
                     array64_4d(2,2,2,2), array64_5d(2,2,2,2,2)

REAL(KIND=C_DOUBLE) :: denormal_double

LOGICAL(KIND=bool) :: equality_test

inf64 = c_test_generate_dinf()

CALL assert_not_equals(0.0_real64, inf64,                                      &
                       "64-bit Inf is not supported on platform")

equality_test = f_shum_is_inf(0.0_real64)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_inf incorrectly detected an Inf")

equality_test = f_shum_is_inf(c_test_generate_dnan())

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_inf confused Inf and NaN")

denormal_double = 0.0
CALL c_test_generate_ddenormal(denormal_double)

equality_test = f_shum_is_inf(denormal_double)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_inf confused Inf and denormal number")

array64_1d = 0.0
equality_test = f_shum_has_inf(array64_1d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_inf incorrectly detected a 1D Inf")

array64_2d = 0.0
equality_test = f_shum_has_inf(array64_2d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_inf incorrectly detected a 2D Inf")

array64_3d = 0.0
equality_test = f_shum_has_inf(array64_3d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_inf incorrectly detected a 3D Inf")

array64_4d = 0.0
equality_test = f_shum_has_inf(array64_4d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_inf incorrectly detected a 4D Inf")

array64_5d = 0.0
equality_test = f_shum_has_inf(array64_5d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_inf incorrectly detected a 5D Inf")

equality_test = f_shum_is_inf(inf64)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect an Inf")

array64_1d(2) = inf64
equality_test = f_shum_has_inf(array64_1d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect a 1D Inf")

array64_2d(2,2) = inf64
equality_test = f_shum_has_inf(array64_2d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect a 2D Inf")

array64_3d(2,2,2) = inf64
equality_test = f_shum_has_inf(array64_3d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect a 3D Inf")

array64_4d(2,2,2,2) = inf64
equality_test = f_shum_has_inf(array64_4d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect a 4D Inf")

array64_5d(2,2,2,2,2) = inf64
equality_test = f_shum_has_inf(array64_5d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_inf failed to detect a 5D Inf")

END SUBROUTINE test_inf64

!------------------------------------------------------------------------------!

SUBROUTINE test_denormal32

USE f_shum_is_denormal_mod, ONLY: f_shum_is_denormal, f_shum_has_denormal

IMPLICIT NONE

REAL(KIND=REAL32) :: array32_1d(2), array32_2d(2,2), array32_3d(2,2,2),        &
                     array32_4d(2,2,2,2), array32_5d(2,2,2,2,2)

REAL(KIND=C_FLOAT) :: denormal_float

LOGICAL(KIND=bool) :: equality_test

denormal_float = 0.0
CALL c_test_generate_fdenormal(denormal_float)

equality_test = (TRANSFER(denormal_float,0_int32) /= 0_int32)

CALL assert_true(equality_test,                                                &
                       "32-bit denormal is not supported on platform")

equality_test = f_shum_is_denormal(0.0_real32)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_denormal incorrectly detected a denormal")

equality_test = f_shum_is_denormal(c_test_generate_fnan())

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_denormal confused denormal and NaN")

equality_test = f_shum_is_denormal(c_test_generate_fnan())

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_is_denormal confused denormal and Inf")

array32_1d = 0.0
equality_test = f_shum_has_denormal(array32_1d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 1D denormal")

array32_2d = 0.0
equality_test = f_shum_has_denormal(array32_2d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 2D denormal")

array32_3d = 0.0
equality_test = f_shum_has_denormal(array32_3d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 3D denormal")

array32_4d = 0.0
equality_test = f_shum_has_denormal(array32_4d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 4D denormal")

array32_5d = 0.0
equality_test = f_shum_has_denormal(array32_5d)

CALL assert_false(equality_test,                                               &
                  "32-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 5D denormal")

equality_test = f_shum_is_denormal(denormal_float)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a denormal")


CALL c_test_generate_fdenormal(array32_1d(2))
equality_test = f_shum_has_denormal(array32_1d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a 1D denormal")

CALL c_test_generate_fdenormal(array32_2d(2,2))
equality_test = f_shum_has_denormal(array32_2d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a 2D denormal")

CALL c_test_generate_fdenormal(array32_3d(2,2,2))
equality_test = f_shum_has_denormal(array32_3d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a 3D denormal")

CALL c_test_generate_fdenormal(array32_4d(2,2,2,2))
equality_test = f_shum_has_denormal(array32_4d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a 4D denormal")

CALL c_test_generate_fdenormal(array32_5d(2,2,2,2,2))
equality_test = f_shum_has_denormal(array32_5d)

CALL assert_true(equality_test,                                                &
                 "32-bit f_shum_has_denormal failed to detect a 5D denormal")

END SUBROUTINE test_denormal32

!------------------------------------------------------------------------------!

SUBROUTINE test_denormal64

USE f_shum_is_denormal_mod, ONLY: f_shum_is_denormal, f_shum_has_denormal

IMPLICIT NONE

REAL(KIND=REAL64) :: array64_1d(2), array64_2d(2,2), array64_3d(2,2,2),        &
                     array64_4d(2,2,2,2), array64_5d(2,2,2,2,2)

REAL(KIND=C_DOUBLE) :: denormal_double

LOGICAL(KIND=bool) :: equality_test

denormal_double = 0.0
CALL c_test_generate_ddenormal(denormal_double)

equality_test = (TRANSFER(denormal_double,0_int64) /= 0_int64)

CALL assert_true(equality_test,                                                &
                       "64-bit denormal is not supported on platform")

equality_test = f_shum_is_denormal(0.0_real64)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_denormal incorrectly detected a denormal")

equality_test = f_shum_is_denormal(c_test_generate_dnan())

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_denormal confused denormal and NaN")

equality_test = f_shum_is_denormal(c_test_generate_dinf())

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_is_denormal confused denormal and Inf")

array64_1d = 0.0
equality_test = f_shum_has_denormal(array64_1d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 1D denormal")

array64_2d = 0.0
equality_test = f_shum_has_denormal(array64_2d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 2D denormal")

array64_3d = 0.0
equality_test = f_shum_has_denormal(array64_3d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 3D denormal")

array64_4d = 0.0
equality_test = f_shum_has_denormal(array64_4d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 4D denormal")

array64_5d = 0.0
equality_test = f_shum_has_denormal(array64_5d)

CALL assert_false(equality_test,                                               &
                  "64-bit f_shum_has_denormal incorrectly detected " //        &
                  "a 5D denormal")

equality_test = f_shum_is_denormal(denormal_double)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a denormal")

CALL c_test_generate_ddenormal(array64_1d(2))
equality_test = f_shum_has_denormal(array64_1d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a 1D denormal")

CALL c_test_generate_ddenormal(array64_2d(2,2))
equality_test = f_shum_has_denormal(array64_2d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a 2D denormal")

CALL c_test_generate_ddenormal(array64_3d(2,2,2))
equality_test = f_shum_has_denormal(array64_3d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a 3D denormal")

CALL c_test_generate_ddenormal(array64_4d(2,2,2,2))
equality_test = f_shum_has_denormal(array64_4d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a 4D denormal")

CALL c_test_generate_ddenormal(array64_5d(2,2,2,2,2))
equality_test = f_shum_has_denormal(array64_5d)

CALL assert_true(equality_test,                                                &
                 "64-bit f_shum_has_denormal failed to detect a 5D denormal")

END SUBROUTINE test_denormal64

!------------------------------------------------------------------------------!

#if defined(HAS_IEEE_ARITHMETIC)
SUBROUTINE test_ieee_arithmetic

USE, INTRINSIC :: ieee_arithmetic, ONLY:                                       &
  IEEE_QUIET_NAN, IEEE_SIGNALING_NAN, IEEE_POSITIVE_INF, IEEE_NEGATIVE_INF,    &
  IEEE_CLASS, IEEE_POSITIVE_DENORMAL, IEEE_NEGATIVE_DENORMAL, OPERATOR(==),    &
  IEEE_SUPPORT_NAN, IEEE_SUPPORT_INF, IEEE_SUPPORT_DENORMAL

IMPLICIT NONE

REAL(KIND=REAL32) :: nan32, inf32
REAL(KIND=REAL64) :: nan64, inf64
REAL(KIND=C_FLOAT) :: denormal_float
REAL(KIND=C_DOUBLE) :: denormal_double
LOGICAL(KIND=bool) :: equality_test

nan32 = c_test_generate_fnan()
IF (IEEE_SUPPORT_NAN(nan32)) THEN


  equality_test = (IEEE_CLASS(nan32) == IEEE_QUIET_NAN) .OR.                   &
                  (IEEE_CLASS(nan32) == IEEE_SIGNALING_NAN)

  CALL assert_true(equality_test,                                              &
                   "32-bit NaN is not validated by ieee_class()")
END IF

nan64 = c_test_generate_dnan()
IF (IEEE_SUPPORT_NAN(nan64)) THEN
  equality_test = (IEEE_CLASS(nan64) == IEEE_QUIET_NAN) .OR.                   &
                  (IEEE_CLASS(nan64) == IEEE_SIGNALING_NAN)

  CALL assert_true(equality_test,                                              &
                   "64-bit NaN is not validated by ieee_class()")
END IF

inf32 = c_test_generate_finf()
IF (IEEE_SUPPORT_INF(inf32)) THEN
  equality_test = (IEEE_CLASS(inf32) == IEEE_POSITIVE_INF) .OR.                &
                  (IEEE_CLASS(inf32) == IEEE_NEGATIVE_INF)

  CALL assert_true(equality_test,                                              &
                   "32-bit Inf is not validated by ieee_class()")
END IF

inf64 = c_test_generate_dinf()
IF (IEEE_SUPPORT_INF(inf64)) THEN
  equality_test = (IEEE_CLASS(inf64) == IEEE_POSITIVE_INF) .OR.                &
                  (IEEE_CLASS(inf64) == IEEE_NEGATIVE_INF)

  CALL assert_true(equality_test,                                              &
                   "64-bit Inf is not validated by ieee_class()")
END IF

denormal_float = 0.0
IF (IEEE_SUPPORT_DENORMAL(denormal_float)) THEN
  CALL c_test_generate_fdenormal(denormal_float)


  equality_test = (IEEE_CLASS(denormal_float) == IEEE_POSITIVE_DENORMAL) .OR.  &
                  (IEEE_CLASS(denormal_float) == IEEE_NEGATIVE_DENORMAL)

  CALL assert_true(equality_test,                                              &
                   "32-bit denormal number is not validated by ieee_class()")
END IF

denormal_double = 0.0
IF (IEEE_SUPPORT_DENORMAL(denormal_double)) THEN
  CALL c_test_generate_ddenormal(denormal_double)

  equality_test = (IEEE_CLASS(denormal_double) == IEEE_POSITIVE_DENORMAL) .OR. &
                  (IEEE_CLASS(denormal_double) == IEEE_NEGATIVE_DENORMAL)

  CALL assert_true(equality_test,                                              &
                   "64-bit denormal number is not validated by ieee_class()")
END IF

END SUBROUTINE test_ieee_arithmetic
#endif

END MODULE fruit_test_shum_number_tools_mod
