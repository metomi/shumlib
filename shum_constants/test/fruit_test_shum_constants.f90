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
MODULE fruit_test_shum_constants_mod

USE fruit, ONLY: assert_equals, run_test_case, assert_true
USE fruit_util, ONLY: equals
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_INT, C_BOOL

IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_constants

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
  INTEGER, PARAMETER :: int64  = C_INT64_T
  INTEGER, PARAMETER :: int32  = C_INT32_T
  INTEGER, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PARAMETER :: real32 = C_FLOAT
  INTEGER, PARAMETER :: bool   = C_BOOL
!------------------------------------------------------------------------------!

CONTAINS

SUBROUTINE fruit_test_shum_constants

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_constants_version_mod, ONLY: get_shum_constants_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_constants_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_constants at Shumlib version: ", version

CALL run_test_case(test_conversions_constant_prec_values,                      &
                   "test_conversions_constant_prec_values")

CALL run_test_case(test_pi_relationships, "test_pi_relationships")

CALL run_test_case(test_time_relationships, "test_time_relationships")

END SUBROUTINE fruit_test_shum_constants

!------------------------------------------------------------------------------!

SUBROUTINE test_conversions_constant_prec_values

USE f_shum_conversions_mod, ONLY:                                              &

          ! Time Constants
          shum_rsec_per_day_const,           shum_rsec_per_day_const_32,       &
          shum_isec_per_day_const,           shum_isec_per_day_const_32,       &
          shum_rhour_per_day_const,          shum_rhour_per_day_const_32,      &
          shum_ihour_per_day_const,          shum_ihour_per_day_const_32,      &
          shum_isec_per_hour_const,          shum_isec_per_hour_const_32,      &
          shum_rsec_per_hour_const,          shum_rsec_per_hour_const_32,      &
          shum_isec_per_min_const,           shum_isec_per_min_const_32,       &
          shum_rsec_per_min_const,           shum_rsec_per_min_const_32,       &
          shum_rhour_per_sec_const,          shum_rhour_per_sec_const_32,      &
          shum_rday_per_hour_const,          shum_rday_per_hour_const_32,      &

          ! Pi Constants
          shum_pi_const,                     shum_pi_const_32,                 &
          shum_pi_over_180_const,            shum_pi_over_180_const_32,        &
          shum_180_over_pi_const,            shum_180_over_pi_const_32,        &

          ! Other Constants
          shum_zerodegc_const,               shum_zerodegc_const_32,           &
          shum_ft2m_const,                   shum_ft2m_const_32,               &
          shum_kt2ms_const,                  shum_kt2ms_const_32

IMPLICIT NONE

REAL(KIND=real64), PARAMETER :: r64eps32 = REAL(EPSILON(0.0_real32),KIND=real64)
LOGICAL(KIND=bool) :: l_is_equal

! Integer Comparisons

CALL assert_equals(INT(shum_isec_per_day_const_32,KIND=int64),                 &
                   shum_isec_per_day_const, "shum_isec_per_day_const")

CALL assert_equals(INT(shum_isec_per_hour_const_32,KIND=int64),                &
                   shum_isec_per_hour_const, "shum_isec_per_hour_const")

CALL assert_equals(INT(shum_isec_per_min_const_32,KIND=int64),                 &
                   shum_isec_per_min_const, "shum_isec_per_min_const")

CALL assert_equals(INT(shum_ihour_per_day_const_32,KIND=int64),                &
                   shum_ihour_per_day_const, "shum_ihour_per_day_const")

! Exactly representable Real comparisons

CALL assert_equals(REAL(shum_rhour_per_sec_const_32,KIND=real64),              &
                   shum_rhour_per_sec_const, r64eps32,                         &
                   "shum_rhour_per_sec_const")

CALL assert_equals(REAL(shum_rsec_per_day_const,KIND=real32),                  &
                   shum_rsec_per_day_const_32, "shum_rsec_per_day_const_32")

CALL assert_equals(REAL(shum_rsec_per_hour_const_32,KIND=real64),              &
                   shum_rsec_per_hour_const, "shum_rsec_per_hour_const")

CALL assert_equals(REAL(shum_rhour_per_day_const_32,KIND=real64),              &
                   shum_rhour_per_day_const, "shum_rhour_per_day_const")

CALL assert_equals(REAL(shum_rsec_per_min_const_32,KIND=real64),               &
                   shum_rsec_per_min_const, "shum_rhour_per_day_const")

! Approximate Real comparisons

l_is_equal = equals(REAL(shum_pi_const_32,KIND=real64),                        &
                    shum_pi_const, r64eps32)

CALL assert_true(l_is_equal, "shum_pi_const")

l_is_equal = equals(REAL(shum_kt2ms_const_32,KIND=real64),                     &
                    shum_kt2ms_const, r64eps32)

CALL assert_true(l_is_equal, "shum_kt2ms_const")

l_is_equal = equals(REAL(shum_pi_over_180_const_32,KIND=real64),               &
                    shum_pi_over_180_const, r64eps32)

CALL assert_true(l_is_equal, "shum_pi_over_180_const")

l_is_equal = equals(REAL(shum_ft2m_const_32,KIND=real64),                      &
                    shum_ft2m_const, r64eps32)

CALL assert_true(l_is_equal, "shum_ft2m_const")

l_is_equal = equals(REAL(shum_rday_per_hour_const_32,KIND=real64),             &
                    shum_rday_per_hour_const, r64eps32)

CALL assert_true(l_is_equal, "shum_rday_per_hour_const")

l_is_equal = equals(REAL(shum_180_over_pi_const_32,KIND=real64),               &
                    shum_180_over_pi_const, r64eps32)

CALL assert_true(l_is_equal, "shum_180_over_pi_const")

l_is_equal = equals(REAL(shum_zerodegc_const_32,KIND=real64),                  &
                    shum_zerodegc_const, r64eps32)

CALL assert_true(l_is_equal, "shum_zerodegc_const")


END SUBROUTINE test_conversions_constant_prec_values

!------------------------------------------------------------------------------!

SUBROUTINE test_pi_relationships

USE f_shum_conversions_mod, ONLY:                                              &
          shum_pi_const,                     shum_pi_const_32,                 &
          shum_pi_over_180_const,             shum_pi_over_180_const_32,       &
          shum_180_over_pi_const,            shum_180_over_pi_const_32

IMPLICIT NONE

CALL assert_equals(180.0_real32/shum_pi_const_32,                              &
                   shum_180_over_pi_const_32, "shum_180_over_pi_const_32")

CALL assert_equals(shum_pi_const_32/180.0_real32,                              &
                   shum_pi_over_180_const_32, "shum_pi_over_180_const_32")

CALL assert_equals(180.0_real64/shum_pi_const,                                 &
                   shum_180_over_pi_const, "shum_180_over_pi_const")

CALL assert_equals(shum_pi_const/180.0_real64,                                 &
                   shum_pi_over_180_const, "shum_pi_over_180_const")


END SUBROUTINE test_pi_relationships

!------------------------------------------------------------------------------!

SUBROUTINE test_time_relationships

USE f_shum_conversions_mod, ONLY:                                              &
          shum_rsec_per_day_const,           shum_rsec_per_day_const_32,       &
          shum_isec_per_day_const,           shum_isec_per_day_const_32,       &
          shum_rhour_per_day_const,          shum_rhour_per_day_const_32,      &
          shum_ihour_per_day_const,          shum_ihour_per_day_const_32,      &
          shum_isec_per_hour_const,          shum_isec_per_hour_const_32,      &
          shum_rsec_per_hour_const,          shum_rsec_per_hour_const_32,      &
          shum_isec_per_min_const,           shum_isec_per_min_const_32,       &
          shum_rsec_per_min_const,           shum_rsec_per_min_const_32,       &
          shum_rhour_per_sec_const,          shum_rhour_per_sec_const_32,      &
          shum_rday_per_hour_const,          shum_rday_per_hour_const_32

IMPLICIT NONE

! Integer vs Real representations

CALL assert_equals(shum_isec_per_day_const,                                    &
                   INT(shum_rsec_per_day_const, KIND=int64),                   &
                   "shum_rsec_per_day_const")

CALL assert_equals(shum_isec_per_hour_const,                                   &
                   INT(shum_rsec_per_hour_const, KIND=int64),                  &
                   "shum_rsec_per_hour_const")

CALL assert_equals(shum_isec_per_min_const,                                    &
                   INT(shum_rsec_per_min_const, KIND=int64),                   &
                   "shum_rsec_per_min_const")

CALL assert_equals(shum_ihour_per_day_const,                                   &
                   INT(shum_rhour_per_day_const, KIND=int64),                  &
                   "shum_rhour_per_day_const")

CALL assert_equals(shum_isec_per_day_const_32,                                 &
                   INT(shum_rsec_per_day_const_32, KIND=int32),                &
                   "shum_rsec_per_day_const_32")

CALL assert_equals(shum_isec_per_hour_const_32,                                &
                   INT(shum_rsec_per_hour_const_32, KIND=int32),               &
                   "shum_rsec_per_hour_const_32")

CALL assert_equals(shum_isec_per_min_const_32,                                 &
                   INT(shum_rsec_per_min_const_32, KIND=int32),                &
                   "shum_rsec_per_min_const_32")

CALL assert_equals(shum_ihour_per_day_const_32,                                &
                   INT(shum_rhour_per_day_const_32, KIND=int32),               &
                   "shum_rhour_per_day_const_32")

! Time relations

CALL assert_equals(shum_ihour_per_day_const*shum_isec_per_hour_const,          &
                   shum_isec_per_day_const,                                    &
                   "shum_isec_per_day_const derived")

CALL assert_equals(shum_ihour_per_day_const_32*shum_isec_per_hour_const_32,    &
                   shum_isec_per_day_const_32,                                 &
                   "shum_isec_per_day_const_32 derived")

CALL assert_equals(shum_isec_per_min_const,                                    &
                   60_int64, "shum_isec_per_min_const")

CALL assert_equals(shum_isec_per_min_const_32,                                 &
                   60_int32, "shum_isec_per_min_const_32")

CALL assert_equals(shum_isec_per_min_const*60_int64,                           &
                   shum_isec_per_hour_const, "shum_isec_per_hour_const")

CALL assert_equals(shum_isec_per_min_const_32*60_int32,                        &
                   shum_isec_per_hour_const_32, "shum_isec_per_hour_const_32")

CALL assert_equals(shum_isec_per_day_const,                                    &
                   86400_int64, "shum_isec_per_day_const exact")

CALL assert_equals(shum_isec_per_day_const_32,                                 &
                   86400_int32, "shum_isec_per_day_const_32 exact")

CALL assert_equals(shum_rday_per_hour_const*shum_rhour_per_day_const,          &
                   1.0_real64, "reciprocal rday and rhour")

CALL assert_equals(shum_rday_per_hour_const_32*shum_rhour_per_day_const_32,    &
                   1.0_real32, "reciprocal rday_32 and rhour_32")

CALL assert_equals(shum_rhour_per_sec_const*shum_rsec_per_hour_const,          &
                   1.0_real64, "reciprocal rhour and rsecond")

CALL assert_equals(shum_rhour_per_sec_const_32*shum_rsec_per_hour_const_32,    &
                   1.0_real32, "reciprocal rhour_32 and rsecond_32")

END SUBROUTINE test_time_relationships

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_constants_mod
