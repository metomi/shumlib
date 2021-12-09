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
MODULE fruit_test_shum_byteswap_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL
USE f_shum_ztables_mod

IMPLICIT NONE
PRIVATE

PUBLIC :: fruit_test_shum_byteswap

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

SUBROUTINE fruit_test_shum_byteswap

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_byteswap_version_mod, ONLY: get_shum_byteswap_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_byteswap_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_byteswap at Shumlib version: ", version

CALL run_test_case(                                                            &
         test_returns_valid_endian, "returns_valid_endian")
CALL run_test_case(                                                            &
         test_1d_64bit_int_data_8_word_size, "1d_64bit_int_data_8_word_size")
CALL run_test_case(                                                            &
         test_2d_64bit_int_data_8_word_size, "2d_64bit_int_data_8_word_size")
CALL run_test_case(                                                            &
         test_1d_64bit_int_data_4_word_size, "1d_64bit_int_data_4_word_size")
CALL run_test_case(                                                            &
         test_2d_64bit_int_data_4_word_size, "2d_64bit_int_data_4_word_size")
CALL run_test_case(                                                            &
         test_1d_32bit_int_data_8_word_size, "1d_32bit_int_data_8_word_size")
CALL run_test_case(                                                            &
         test_2d_32bit_int_data_8_word_size, "2d_32bit_int_data_8_word_size")
CALL run_test_case(                                                            &
         test_1d_32bit_int_data_4_word_size, "1d_32bit_int_data_4_word_size")
CALL run_test_case(                                                            &
         test_2d_32bit_int_data_4_word_size, "2d_32bit_int_data_4_word_size")
CALL run_test_case(                                                            &
         test_1d_64bit_data_8_word_size, "1d_64bit_data_8_word_size")
CALL run_test_case(                                                            &
         test_2d_64bit_data_8_word_size, "2d_64bit_data_8_word_size")
CALL run_test_case(                                                            &
         test_1d_64bit_data_4_word_size, "1d_64bit_data_4_word_size")
CALL run_test_case(                                                            &
         test_2d_64bit_data_4_word_size, "2d_64bit_data_4_word_size")
CALL run_test_case(                                                            &
         test_1d_32bit_data_8_word_size, "1d_32bit_data_8_word_size")
CALL run_test_case(                                                            &
         test_2d_32bit_data_8_word_size, "2d_32bit_data_8_word_size")
CALL run_test_case(                                                            &
         test_1d_32bit_data_4_word_size, "1d_32bit_data_4_word_size")
CALL run_test_case(                                                            &
         test_2d_32bit_data_4_word_size, "2d_32bit_data_4_word_size")

END SUBROUTINE fruit_test_shum_byteswap

!------------------------------------------------------------------------------!

SUBROUTINE test_returns_valid_endian

USE f_shum_byteswap_mod, ONLY: f_shum_get_machine_endianism,                   &
                               f_shum_littleendian, f_shum_bigendian
IMPLICIT NONE

INTEGER(KIND=int64) :: endian
LOGICAL(KIND=bool) :: check
CALL set_case_name("test_byteswap_returns_valid_endian")
endian = f_shum_get_machine_endianism()
check = ((endian == f_shum_littleendian) .OR. (endian == f_shum_bigendian))
CALL assert_true(check, "Returned value is not a valid endian enum value")

END SUBROUTINE test_returns_valid_endian

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_64bit_int_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 8

INTEGER(KIND=int64) :: data_in(len_data)
INTEGER(KIND=int64) :: data_swapped_expected(len_data)
INTEGER(KIND=int64) :: data_in_copy(len_data)

INTEGER(KIND=int64) :: status
INTEGER :: i
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = i
  data_in_copy(i) = i
END DO

data_swapped_expected(1)  = z0100000000000000
data_swapped_expected(2)  = z0200000000000000
data_swapped_expected(3)  = z0300000000000000
data_swapped_expected(4)  = z0400000000000000
data_swapped_expected(5)  = z0500000000000000
data_swapped_expected(6)  = z0600000000000000
data_swapped_expected(7)  = z0700000000000000
data_swapped_expected(8)  = z0800000000000000
data_swapped_expected(9)  = z0900000000000000
data_swapped_expected(10) = z0A00000000000000

status = f_shum_byteswap(data_in,                                              &
                         INT(len_data, KIND=int64),                            &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected,                                &
                         INT(len_data, KIND=int64),                            &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_64bit_int_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_64bit_int_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 8

INTEGER(KIND=int64) :: data_in(len1, len2)
INTEGER(KIND=int64) :: data_swapped_expected(len1, len2)
INTEGER(KIND=int64) :: data_in_copy(len1, len2)

INTEGER(KIND=int64) :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = k
    data_in_copy(j, i) = k
  END DO
END DO

data_swapped_expected(1, 1) = z0100000000000000
data_swapped_expected(2, 1) = z0200000000000000
data_swapped_expected(3, 1) = z0300000000000000
data_swapped_expected(4, 1) = z0400000000000000
data_swapped_expected(5, 1) = z0500000000000000
data_swapped_expected(1, 2) = z0600000000000000
data_swapped_expected(2, 2) = z0700000000000000
data_swapped_expected(3, 2) = z0800000000000000
data_swapped_expected(4, 2) = z0900000000000000
data_swapped_expected(5, 2) = z0A00000000000000

status = f_shum_byteswap(data_in,                                              &
                         INT(len1*len2, KIND=int64),                           &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected,                                &
                         INT(len1*len2, KIND=int64),                           &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_64bit_int_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_64bit_int_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 4

INTEGER(KIND=int64) :: data_in(len_data)
INTEGER(KIND=int64) :: data_swapped_expected(len_data)
INTEGER(KIND=int64) :: data_in_copy(len_data)

INTEGER(KIND=int64) :: status
INTEGER :: i
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = i
  data_in_copy(i) = i
END DO

data_swapped_expected(1)  = z0000000001000000
data_swapped_expected(2)  = z0000000002000000
data_swapped_expected(3)  = z0000000003000000
data_swapped_expected(4)  = z0000000004000000
data_swapped_expected(5)  = z0000000005000000
data_swapped_expected(6)  = z0000000006000000
data_swapped_expected(7)  = z0000000007000000
data_swapped_expected(8)  = z0000000008000000
data_swapped_expected(9)  = z0000000009000000
data_swapped_expected(10) = z000000000A000000

status = f_shum_byteswap(data_in,                                              &
                         INT(len_data*2, KIND=int64),                          &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected,                                &
                         INT(len_data*2, KIND=int64),                          &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_64bit_int_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_64bit_int_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 4

INTEGER(KIND=int64) :: data_in(len1, len2)
INTEGER(KIND=int64) :: data_swapped_expected(len1, len2)
INTEGER(KIND=int64) :: data_in_copy(len1, len2)

INTEGER(KIND=int64) :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = k
    data_in_copy(j, i) = k
  END DO
END DO

data_swapped_expected(1, 1) = z0000000001000000
data_swapped_expected(2, 1) = z0000000002000000
data_swapped_expected(3, 1) = z0000000003000000
data_swapped_expected(4, 1) = z0000000004000000
data_swapped_expected(5, 1) = z0000000005000000
data_swapped_expected(1, 2) = z0000000006000000
data_swapped_expected(2, 2) = z0000000007000000
data_swapped_expected(3, 2) = z0000000008000000
data_swapped_expected(4, 2) = z0000000009000000
data_swapped_expected(5, 2) = z000000000A000000

status = f_shum_byteswap(data_in,                                              &
                         INT(len1*len2*2, KIND=int64),                         &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected,                                &
                         INT(len1*len2*2, KIND=int64),                         &
                         INT(word_size, KIND=int64), message)
CALL assert_equals(0_int64, status,                                            &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_64bit_int_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_32bit_int_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 8

INTEGER(KIND=int32) :: data_in(len_data)
INTEGER(KIND=int32) :: data_swapped_expected(len_data)
INTEGER(KIND=int32) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = i
  data_in_copy(i) = i
END DO

data_swapped_expected(1)  = z02000000
data_swapped_expected(2)  = z01000000
data_swapped_expected(3)  = z04000000
data_swapped_expected(4)  = z03000000
data_swapped_expected(5)  = z06000000
data_swapped_expected(6)  = z05000000
data_swapped_expected(7)  = z08000000
data_swapped_expected(8)  = z07000000
data_swapped_expected(9)  = z0A000000
data_swapped_expected(10) = z09000000

status = f_shum_byteswap(data_in, len_data/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_32bit_int_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_32bit_int_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 8

INTEGER(KIND=int32) :: data_in(len1, len2)
INTEGER(KIND=int32) :: data_swapped_expected(len1, len2)
INTEGER(KIND=int32) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = k
    data_in_copy(j, i) = k
  END DO
END DO

data_swapped_expected(1, 1) = z02000000
data_swapped_expected(2, 1) = z01000000
data_swapped_expected(3, 1) = z04000000
data_swapped_expected(4, 1) = z03000000
data_swapped_expected(5, 1) = z06000000
data_swapped_expected(1, 2) = z05000000
data_swapped_expected(2, 2) = z08000000
data_swapped_expected(3, 2) = z07000000
data_swapped_expected(4, 2) = z0A000000
data_swapped_expected(5, 2) = z09000000

status = f_shum_byteswap(data_in, len1*len2/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_32bit_int_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_32bit_int_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 4

INTEGER(KIND=int32) :: data_in(len_data)
INTEGER(KIND=int32) :: data_swapped_expected(len_data)
INTEGER(KIND=int32) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = i
  data_in_copy(i) = i
END DO

data_swapped_expected(1)  = z01000000
data_swapped_expected(2)  = z02000000
data_swapped_expected(3)  = z03000000
data_swapped_expected(4)  = z04000000
data_swapped_expected(5)  = z05000000
data_swapped_expected(6)  = z06000000
data_swapped_expected(7)  = z07000000
data_swapped_expected(8)  = z08000000
data_swapped_expected(9)  = z09000000
data_swapped_expected(10) = z0A000000

status = f_shum_byteswap(data_in, len_data, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_32bit_int_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_32bit_int_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 4

INTEGER(KIND=int32) :: data_in(len1, len2)
INTEGER(KIND=int32) :: data_swapped_expected(len1, len2)
INTEGER(KIND=int32) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = k
    data_in_copy(j, i) = k
  END DO
END DO

data_swapped_expected(1, 1) = z01000000
data_swapped_expected(2, 1) = z02000000
data_swapped_expected(3, 1) = z03000000
data_swapped_expected(4, 1) = z04000000
data_swapped_expected(5, 1) = z05000000
data_swapped_expected(1, 2) = z06000000
data_swapped_expected(2, 2) = z07000000
data_swapped_expected(3, 2) = z08000000
data_swapped_expected(4, 2) = z09000000
data_swapped_expected(5, 2) = z0A000000

status = f_shum_byteswap(data_in, len1*len2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Integer64 byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_32bit_int_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_64bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real64) :: data_in(len_data)
REAL(KIND=real64) :: data_swapped_expected(len_data)
REAL(KIND=real64) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = REAL(i, KIND=real64)
  data_in_copy(i) = REAL(i, KIND=real64)
END DO

data_swapped_expected(1)  = TRANSFER(z000000000000F03F, o64)
data_swapped_expected(2)  = TRANSFER(z0000000000000040, o64)
data_swapped_expected(3)  = TRANSFER(z0000000000000840, o64)
data_swapped_expected(4)  = TRANSFER(z0000000000001040, o64)
data_swapped_expected(5)  = TRANSFER(z0000000000001440, o64)
data_swapped_expected(6)  = TRANSFER(z0000000000001840, o64)
data_swapped_expected(7)  = TRANSFER(z0000000000001C40, o64)
data_swapped_expected(8)  = TRANSFER(z0000000000002040, o64)
data_swapped_expected(9)  = TRANSFER(z0000000000002240, o64)
data_swapped_expected(10) = TRANSFER(z0000000000002440, o64)

status = f_shum_byteswap(data_in, len_data, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_64bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_64bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real64) :: data_in(len1, len2)
REAL(KIND=real64) :: data_swapped_expected(len1, len2)
REAL(KIND=real64) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = REAL(k, KIND=real64)
    data_in_copy(j, i) = REAL(k, KIND=real64)
  END DO
END DO

data_swapped_expected(1, 1) = TRANSFER(z000000000000F03F, o64)
data_swapped_expected(2, 1) = TRANSFER(z0000000000000040, o64)
data_swapped_expected(3, 1) = TRANSFER(z0000000000000840, o64)
data_swapped_expected(4, 1) = TRANSFER(z0000000000001040, o64)
data_swapped_expected(5, 1) = TRANSFER(z0000000000001440, o64)
data_swapped_expected(1, 2) = TRANSFER(z0000000000001840, o64)
data_swapped_expected(2, 2) = TRANSFER(z0000000000001C40, o64)
data_swapped_expected(3, 2) = TRANSFER(z0000000000002040, o64)
data_swapped_expected(4, 2) = TRANSFER(z0000000000002240, o64)
data_swapped_expected(5, 2) = TRANSFER(z0000000000002440, o64)

status = f_shum_byteswap(data_in, len1*len2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_64bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_64bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real64) :: data_in(len_data)
REAL(KIND=real64) :: data_swapped_expected(len_data)
REAL(KIND=real64) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = REAL(i, KIND=real64)
  data_in_copy(i) = REAL(i, KIND=real64)
END DO

data_swapped_expected(1)  = TRANSFER(z0000F03F00000000, o64)
data_swapped_expected(2)  = TRANSFER(z0000004000000000, o64)
data_swapped_expected(3)  = TRANSFER(z0000084000000000, o64)
data_swapped_expected(4)  = TRANSFER(z0000104000000000, o64)
data_swapped_expected(5)  = TRANSFER(z0000144000000000, o64)
data_swapped_expected(6)  = TRANSFER(z0000184000000000, o64)
data_swapped_expected(7)  = TRANSFER(z00001C4000000000, o64)
data_swapped_expected(8)  = TRANSFER(z0000204000000000, o64)
data_swapped_expected(9)  = TRANSFER(z0000224000000000, o64)
data_swapped_expected(10) = TRANSFER(z0000244000000000, o64)

status = f_shum_byteswap(data_in, len_data*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_64bit_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_64bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real64) :: data_in(len1, len2)
REAL(KIND=real64) :: data_swapped_expected(len1, len2)
REAL(KIND=real64) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = REAL(k, KIND=real64)
    data_in_copy(j, i) = REAL(k, KIND=real64)
  END DO
END DO

data_swapped_expected(1, 1) = TRANSFER(z0000F03F00000000, o64)
data_swapped_expected(2, 1) = TRANSFER(z0000004000000000, o64)
data_swapped_expected(3, 1) = TRANSFER(z0000084000000000, o64)
data_swapped_expected(4, 1) = TRANSFER(z0000104000000000, o64)
data_swapped_expected(5, 1) = TRANSFER(z0000144000000000, o64)
data_swapped_expected(1, 2) = TRANSFER(z0000184000000000, o64)
data_swapped_expected(2, 2) = TRANSFER(z00001C4000000000, o64)
data_swapped_expected(3, 2) = TRANSFER(z0000204000000000, o64)
data_swapped_expected(4, 2) = TRANSFER(z0000224000000000, o64)
data_swapped_expected(5, 2) = TRANSFER(z0000244000000000, o64)

status = f_shum_byteswap(data_in, len1*len2*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_64bit_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_32bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real32) :: data_in(len_data)
REAL(KIND=real32) :: data_swapped_expected(len_data)
REAL(KIND=real32) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = REAL(i, KIND=real32)
  data_in_copy(i) = REAL(i, KIND=real32)
END DO

data_swapped_expected(1)  = TRANSFER(z00000040, o32)
data_swapped_expected(2)  = TRANSFER(z0000803F, o32)
data_swapped_expected(3)  = TRANSFER(z00008040, o32)
data_swapped_expected(4)  = TRANSFER(z00004040, o32)
data_swapped_expected(5)  = TRANSFER(z0000C040, o32)
data_swapped_expected(6)  = TRANSFER(z0000A040, o32)
data_swapped_expected(7)  = TRANSFER(z00000041, o32)
data_swapped_expected(8)  = TRANSFER(z0000E040, o32)
data_swapped_expected(9)  = TRANSFER(z00002041, o32)
data_swapped_expected(10) = TRANSFER(z00001041, o32)

status = f_shum_byteswap(data_in, len_data/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len_data,              &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_1d_32bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_32bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real32) :: data_in(len1, len2)
REAL(KIND=real32) :: data_swapped_expected(len1, len2)
REAL(KIND=real32) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = REAL(k, KIND=real32)
    data_in_copy(j, i) = REAL(k, KIND=real32)
  END DO
END DO

data_swapped_expected(1, 1) = TRANSFER(z00000040, o32)
data_swapped_expected(2, 1) = TRANSFER(z0000803F, o32)
data_swapped_expected(3, 1) = TRANSFER(z00008040, o32)
data_swapped_expected(4, 1) = TRANSFER(z00004040, o32)
data_swapped_expected(5, 1) = TRANSFER(z0000C040, o32)
data_swapped_expected(1, 2) = TRANSFER(z0000A040, o32)
data_swapped_expected(2, 2) = TRANSFER(z00000041, o32)
data_swapped_expected(3, 2) = TRANSFER(z0000E040, o32)
data_swapped_expected(4, 2) = TRANSFER(z00002041, o32)
data_swapped_expected(5, 2) = TRANSFER(z00001041, o32)

status = f_shum_byteswap(data_in, len1*len2/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len1, len2,            &
    "Double byteswapped array does not agree with initial array")


END SUBROUTINE test_2d_32bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_1d_32bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len_data = 10
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real32) :: data_in(len_data)
REAL(KIND=real32) :: data_swapped_expected(len_data)
REAL(KIND=real32) :: data_in_copy(len_data)

INTEGER :: status
INTEGER :: i
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len_data
  data_in(i) = REAL(i, KIND=real32)
  data_in_copy(i) = REAL(i, KIND=real32)
END DO

data_swapped_expected(1)  = TRANSFER(z0000803F, o32)
data_swapped_expected(2)  = TRANSFER(z00000040, o32)
data_swapped_expected(3)  = TRANSFER(z00004040, o32)
data_swapped_expected(4)  = TRANSFER(z00008040, o32)
data_swapped_expected(5)  = TRANSFER(z0000A040, o32)
data_swapped_expected(6)  = TRANSFER(z0000C040, o32)
data_swapped_expected(7)  = TRANSFER(z0000E040, o32)
data_swapped_expected(8)  = TRANSFER(z00000041, o32)
data_swapped_expected(9)  = TRANSFER(z00001041, o32)
data_swapped_expected(10) = TRANSFER(z00002041, o32)

status = f_shum_byteswap(data_in, len_data, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_in, data_swapped_expected, len_data,                   &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len_data, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in_copy, len_data,              &
    "Double byteswapped array does not agree with initial array")

END SUBROUTINE test_1d_32bit_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_2d_32bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE

INTEGER, PARAMETER :: len1 = 5
INTEGER, PARAMETER :: len2 = 2
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real32) :: data_in(len1, len2)
REAL(KIND=real32) :: data_swapped_expected(len1, len2)
REAL(KIND=real32) :: data_in_copy(len1, len2)

INTEGER :: status
INTEGER :: i
INTEGER :: j
INTEGER :: k
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

k = 0
DO i = 1, len2
  DO j = 1, len1
    k = k + 1
    data_in(j, i) = REAL(k, KIND=real32)
    data_in_copy(j, i) = REAL(k, KIND=real32)
  END DO
END DO

data_swapped_expected(1, 1) = TRANSFER(z0000803F, o32)
data_swapped_expected(2, 1) = TRANSFER(z00000040, o32)
data_swapped_expected(3, 1) = TRANSFER(z00004040, o32)
data_swapped_expected(4, 1) = TRANSFER(z00008040, o32)
data_swapped_expected(5, 1) = TRANSFER(z0000A040, o32)
data_swapped_expected(1, 2) = TRANSFER(z0000C040, o32)
data_swapped_expected(2, 2) = TRANSFER(z0000E040, o32)
data_swapped_expected(3, 2) = TRANSFER(z00000041, o32)
data_swapped_expected(4, 2) = TRANSFER(z00001041, o32)
data_swapped_expected(5, 2) = TRANSFER(z00002041, o32)

status = f_shum_byteswap(data_in, len1*len2, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_in, data_swapped_expected, len1, len2,                 &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len1*len2, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in_copy, len1, len2,            &
    "Double byteswapped array does not agree with initial array")

END SUBROUTINE test_2d_32bit_data_4_word_size

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_byteswap_mod
