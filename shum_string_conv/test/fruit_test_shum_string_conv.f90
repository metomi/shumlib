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
MODULE fruit_test_shum_string_conv_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_CHAR, C_NULL_CHAR, C_PTR, C_LOC

IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_string_conv

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
!------------------------------------------------------------------------------!

CONTAINS

SUBROUTINE fruit_test_shum_string_conv

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_string_conv_version_mod, ONLY: get_shum_string_conv_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_string_conv_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_string_conv at Shumlib version: ", version

CALL run_test_case(test_c2f_string_cstr, "c2f_string_cstr")
CALL run_test_case(test_c2f_string_ptr, "c2f_string_ptr")
CALL run_test_case(test_c2f_string_nolen, "c2f_string_nolen")
CALL run_test_case(test_c2f_string_len1, "c2f_string_len1")
CALL run_test_case(test_strlen, "strlen")
CALL run_test_case(test_f2c_string, "f2c_string")

END SUBROUTINE fruit_test_shum_string_conv

!------------------------------------------------------------------------------!

SUBROUTINE test_c2f_string_cstr

USE f_shum_string_conv_mod, ONLY: f_shum_f2c_string, f_shum_c2f_string

IMPLICIT NONE

CHARACTER(KIND=C_CHAR, LEN=1) :: c_string(50)
CHARACTER(LEN=14) :: f_string

CALL set_case_name("test_c2f_string_cstr")

c_string(1:14) = ["t", "e", "s", "t", " ",                                     &
                  "C", " ",                                                    &
                  "s", "t", "r", "i", "n", "g", C_NULL_CHAR ]

f_string = f_shum_c2f_string(c_string, INT(13, KIND=int64))

CALL assert_equals(13, LEN(f_shum_c2f_string(c_string, INT(13, KIND=int64))),  &
    "Returned Fortran string not expected length")

CALL assert_equals("test C string", f_string,                                  &
    "Contents of string are different after conversion")

END SUBROUTINE test_c2f_string_cstr

!------------------------------------------------------------------------------!

SUBROUTINE test_c2f_string_ptr

USE f_shum_string_conv_mod, ONLY: f_shum_f2c_string, f_shum_c2f_string

IMPLICIT NONE

CHARACTER(KIND=C_CHAR, LEN=1), TARGET :: c_string(50)
CHARACTER(LEN=14) :: f_string
TYPE(C_PTR) :: cptr

CALL set_case_name("test_c2f_string_ptr")

c_string(1:14) = ["t", "e", "s", "t", " ",                                     &
                  "C", " ",                                                    &
                  "s", "t", "r", "i", "n", "g", C_NULL_CHAR ]

cptr = C_LOC(c_string)

f_string = f_shum_c2f_string(cptr, INT(13, KIND=int64))

CALL assert_equals(13, LEN(f_shum_c2f_string(cptr, INT(13, KIND=int64))),      &
    "Returned Fortran string not expected length")

CALL assert_equals("test C string", f_string,                                  &
    "Contents of string are different after conversion")

END SUBROUTINE test_c2f_string_ptr

!------------------------------------------------------------------------------!

SUBROUTINE test_c2f_string_nolen

USE f_shum_string_conv_mod, ONLY: f_shum_f2c_string, f_shum_c2f_string

IMPLICIT NONE

CHARACTER(KIND=C_CHAR, LEN=1) :: c_string(50)
CHARACTER(LEN=14), ALLOCATABLE :: f_string

CALL set_case_name("test_c2f_string_nolen")

c_string(1:14) = ["t", "e", "s", "t", " ",                                     &
                  "C", " ",                                                    &
                  "s", "t", "r", "i", "n", "g", C_NULL_CHAR ]

f_string = f_shum_c2f_string(c_string)

CALL assert_equals(13, LEN(f_shum_c2f_string(c_string)),                       &
    "Returned Fortran string not expected length")

CALL assert_equals("test C string", f_string,                                  &
    "Contents of string are different after conversion")

END SUBROUTINE test_c2f_string_nolen

!------------------------------------------------------------------------------!

SUBROUTINE test_c2f_string_len1

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string

IMPLICIT NONE

CHARACTER(KIND=C_CHAR, LEN=1) :: c_string(50)
CHARACTER(LEN=2) :: f_string

CALL set_case_name("test_c2f_string_nolen")

c_string(1:14) = ["t", "e", "s", "t", " ",                                     &
                  "C", " ",                                                    &
                  "s", "t", "r", "i", "n", "g", C_NULL_CHAR ]

f_string = f_shum_c2f_string(c_string, 1_int64)

CALL assert_equals(1, LEN(f_shum_c2f_string(c_string, 1_int64)),               &
    "Returned Fortran string not expected length")

CALL assert_equals("t", f_string,                                              &
    "Contents of string are different after conversion")

END SUBROUTINE test_c2f_string_len1

!------------------------------------------------------------------------------!

SUBROUTINE test_strlen

USE f_shum_string_conv_mod, ONLY: f_shum_strlen

IMPLICIT NONE

INTEGER(KIND=int64) :: c_strlen
CHARACTER(KIND=C_CHAR, LEN=1) :: c_string(50)

INTEGER(KIND=int64), PARAMETER :: len1 = 30
INTEGER(KIND=int64), PARAMETER :: len2 = 0
INTEGER(KIND=int64), PARAMETER :: len3 = 50

CALL set_case_name("test_strlen")

c_string(:) = ""
c_string(len1 + 1) = C_NULL_CHAR

c_strlen = f_shum_strlen(c_string)

CALL assert_equals(len1, c_strlen,                                             &
    "String length with null character in middle of string is wrong")

c_string(len1 + 1) = ""
c_string(len2 + 1) = C_NULL_CHAR

c_strlen = f_shum_strlen(c_string)

CALL assert_equals(len2, c_strlen,                                             &
    "String length with null character at start of string is wrong")

c_string(len2 + 1) = ""
c_string(len3) = C_NULL_CHAR

c_strlen = f_shum_strlen(c_string)

CALL assert_equals(len3 - 1, c_strlen,                                         &
    "String length with null character at end of string is wrong")

END SUBROUTINE test_strlen

!------------------------------------------------------------------------------!

SUBROUTINE test_f2c_string

USE f_shum_string_conv_mod, ONLY: f_shum_f2c_string, f_shum_c2f_string

IMPLICIT NONE

CHARACTER(LEN=50) :: f_string
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: c_string(:)

CALL set_case_name("test_f2c_string")

f_string = ""
f_string = "test fortran string"
c_string = f_shum_f2c_string(f_string)

CALL assert_equals(LEN(f_string) + 1, SIZE(c_string),                          &
    "Returned C string not 1 character longer than input string")

CALL assert_equals(C_NULL_CHAR, c_string(SIZE(c_string)),                      &
    "C string does not end with the null character")

CALL assert_equals(f_string // C_NULL_CHAR,                                    &
    f_shum_c2f_string(c_string, SIZE(c_string,KIND=int64)),                    &
    "Contents of string are different after conversion")

END SUBROUTINE test_f2c_string

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_string_conv_mod
