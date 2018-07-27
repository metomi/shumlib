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
MODULE fruit_test_shum_wgdos_packing_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

PRIVATE

PUBLIC :: fruit_test_shum_wgdos_packing

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

INTERFACE sample_starting_data
MODULE PROCEDURE sample_starting_data_2d, sample_starting_data_1d
END INTERFACE

INTERFACE sample_unpacked_data
MODULE PROCEDURE sample_unpacked_data_2d, sample_unpacked_data_1d
END INTERFACE

CONTAINS

SUBROUTINE fruit_test_shum_wgdos_packing

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_wgdos_packing_version_mod, ONLY: get_shum_wgdos_packing_version

IMPLICIT NONE 

INTEGER(KIND=int64) :: version

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's 
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_wgdos_packing_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_wgdos_packing at Shumlib version: ", version

CALL run_test_case(                                                            &
    test_pack_simple_field_1d_arg64, "pack_simple_field_1d_arg64")
CALL run_test_case(                                                            &
    test_pack_simple_field_1d_alloc_arg64, "pack_simple_field_1d_alloc_arg64")
CALL run_test_case(                                                            &
    test_pack_simple_field_2d_arg64, "pack_simple_field_2d_arg64")
CALL run_test_case(                                                            &
    test_pack_simple_field_2d_alloc_arg64, "pack_simple_field_2d_alloc_arg64")
CALL run_test_case(                                                            &
    test_pack_simple_field_1d_arg32, "pack_simple_field_1d_arg32")
CALL run_test_case(                                                            &
    test_pack_simple_field_1d_alloc_arg32, "pack_simple_field_1d_alloc_arg32")
CALL run_test_case(                                                            &
    test_pack_simple_field_2d_arg32, "pack_simple_field_2d_arg32")
CALL run_test_case(                                                            &
    test_pack_simple_field_2d_alloc_arg32, "pack_simple_field_2d_alloc_arg32")
CALL run_test_case(                                                            &
    test_unpack_simple_field_1d_arg64, "unpack_simple_field_1d_arg64")
CALL run_test_case(                                                            &
    test_unpack_simple_field_2d_arg64, "unpack_simple_field_2d_arg64")
CALL run_test_case(                                                            &
    test_unpack_simple_field_1d_arg32, "unpack_simple_field_1d_arg32")
CALL run_test_case(                                                            &
    test_unpack_simple_field_2d_arg32, "unpack_simple_field_2d_arg32")
CALL run_test_case(                                                            &
    test_packing_field_with_zeros, "packing_field_with_zeros")
CALL run_test_case(                                                            &
    test_packing_field_with_mdi, "packing_field_with_mdi")
CALL run_test_case(                                                            &
    test_packing_field_with_zeros_and_mdi, "packing_field_with_zeros_and_mdi")
CALL run_test_case(                                                            &
    test_read_simple_header_arg64, "read_simple_header_arg64")
CALL run_test_case(                                                            &
    test_read_simple_header_arg32, "read_simple_header_arg32")
CALL run_test_case(                                                            &
    test_fail_packing_accuracy, "fail_packing_accuracy")
CALL run_test_case(                                                            &
    test_fail_packing_return_array_size, "fail_packing_return_array_size")
CALL run_test_case(                                                            &
    test_fail_pack_stride_arg64, "fail_pack_stride_arg64")
CALL run_test_case(                                                            &
    test_fail_pack_stride_arg32, "fail_pack_stride_arg32")
CALL run_test_case(                                                            &
    test_fail_unpack_stride_arg64, "fail_unpack_stride_arg64")
CALL run_test_case(                                                            &
    test_fail_unpack_stride_arg32, "fail_unpack_stride_arg32")
CALL run_test_case(                                                            &
    test_fail_unpack_too_many_elements, "fail_unpack_too_many_elements")
CALL run_test_case(                                                            &
    test_fail_unpack_inconsistent, "fail_unpack_inconsistent")

END SUBROUTINE fruit_test_shum_wgdos_packing

! Functions used to return sample dataset of unpacked data - the goal here 
! isn't for a numerical workout but rather an easy to identify and work with
! array - a simple range from 1 - 30 split across 6 rows.  This data can be
! returned as either 1d or 2d data to handle both types of interface.
!------------------------------------------------------------------------------!

SUBROUTINE sample_starting_data_2d(sample) 
IMPLICIT NONE 
REAL(KIND=real64), INTENT(OUT) :: sample(5, 6)
sample(:,1) = [  1.0,  2.0,  3.0,  4.0,  5.0 ]
sample(:,2) = [  6.0,  7.0,  8.0,  9.0, 10.0 ]
sample(:,3) = [ 11.0, 12.0, 13.0, 14.0, 15.0 ]
sample(:,4) = [ 16.0, 17.0, 18.0, 19.0, 20.0 ]
sample(:,5) = [ 21.0, 22.0, 23.0, 24.0, 25.0 ]
sample(:,6) = [ 26.0, 27.0, 28.0, 29.0, 30.0 ]
END SUBROUTINE sample_starting_data_2d

SUBROUTINE sample_starting_data_1d(sample)
IMPLICIT NONE 
REAL(KIND=real64), INTENT(OUT) :: sample(30)
sample(1:5)   = [  1.0,  2.0,  3.0,  4.0,  5.0 ]
sample(6:10)  = [  6.0,  7.0,  8.0,  9.0, 10.0 ]
sample(11:15) = [ 11.0, 12.0, 13.0, 14.0, 15.0 ]
sample(16:20) = [ 16.0, 17.0, 18.0, 19.0, 20.0 ]
sample(21:25) = [ 21.0, 22.0, 23.0, 24.0, 25.0 ]
sample(26:30) = [ 26.0, 27.0, 28.0, 29.0, 30.0 ]
END SUBROUTINE sample_starting_data_1d

! As above, only these arrays store the expected result of packing the above
! data with an accuracy of "1" and then unpacking it again.  The packing will
! always push data onto a 2-power value based on the accuracy; in this simple
! case that is 2**1 == 2; so all odd values are shunted upwards to the next
! multiple of 2; again this is simple easy to test against and visualise.
!------------------------------------------------------------------------------!

SUBROUTINE sample_unpacked_data_2d(sample) 
IMPLICIT NONE 
REAL(KIND=real64), INTENT(OUT) :: sample(5, 6)
sample(:,1) = [  2.0,  2.0,  4.0,  4.0,  6.0 ]
sample(:,2) = [  6.0,  8.0,  8.0, 10.0, 10.0 ]
sample(:,3) = [ 12.0, 12.0, 14.0, 14.0, 16.0 ]
sample(:,4) = [ 16.0, 18.0, 18.0, 20.0, 20.0 ]
sample(:,5) = [ 22.0, 22.0, 24.0, 24.0, 26.0 ]
sample(:,6) = [ 26.0, 28.0, 28.0, 30.0, 30.0 ]
END SUBROUTINE sample_unpacked_data_2d

SUBROUTINE sample_unpacked_data_1d(sample)
IMPLICIT NONE 
REAL(KIND=real64), INTENT(OUT) :: sample(30)
sample(1:5)   = [  2.0,  2.0,  4.0,  4.0,  6.0 ]
sample(6:10)  = [  6.0,  8.0,  8.0, 10.0, 10.0 ]
sample(11:15) = [ 12.0, 12.0, 14.0, 14.0, 16.0 ]
sample(16:20) = [ 16.0, 18.0, 18.0, 20.0, 20.0 ]
sample(21:25) = [ 22.0, 22.0, 24.0, 24.0, 26.0 ]
sample(26:30) = [ 26.0, 28.0, 28.0, 30.0, 30.0 ]
END SUBROUTINE sample_unpacked_data_1d

! Function which returns the intermediate stage between the two pairs of sample
! data above - this array is less intuitive since the data is compressed, but
! some header values can be observed (particularly the array length in the 
! first element and the accuracy of the packing in the second).
!------------------------------------------------------------------------------!

SUBROUTINE sample_packed_data(sample)
IMPLICIT NONE 
INTEGER(KIND=int32) :: sample(21)

sample = [          21_int32,                                                  &
                     1_int32,                                                  &
                327686_int32,                                                  &
            1092616192_int32,                                                  &
                131073_int32,                                                  &
              92274688_int32,                                                  &
            1096810496_int32,                                                  &
                131073_int32,                                                  &
             377487360_int32,                                                  &
            1103101952_int32,                                                  &
                131073_int32,                                                  &
              92274688_int32,                                                  &
            1108344832_int32,                                                  &
                131073_int32,                                                  &
             377487360_int32,                                                  &
            1108738048_int32,                                                  &
                131073_int32,                                                  &
              92274688_int32,                                                  &
            1109000192_int32,                                                  &
                131073_int32,                                                  &
             377487360_int32   ]

END SUBROUTINE sample_packed_data

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_1d_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked, accuracy, mdi,        &
                           packed_data, num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_1d_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_1d_alloc_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32), ALLOCATABLE :: packed_data(:)

INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked, accuracy, mdi,        &
                           packed_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, SIZE(packed_data, KIND=int64),                  &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_1d_alloc_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_2d_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_2d_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_2d_alloc_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32), ALLOCATABLE :: packed_data(:)

INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, SIZE(packed_data, KIND=int64),                  &
    "Size of packed data is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_2d_alloc_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_1d_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked, accuracy, mdi,        &
                           packed_data, num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_1d_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_1d_alloc_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32), ALLOCATABLE :: packed_data(:)

INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked, accuracy, mdi,        &
                           packed_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, SIZE(packed_data, KIND=int32),                  &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_1d_alloc_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_2d_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_2d_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field_2d_alloc_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: expected_data(len_packed)

INTEGER(KIND=int32), ALLOCATABLE :: packed_data(:)

INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

CALL sample_packed_data(expected_data)

CALL assert_equals(len_packed, SIZE(packed_data, KIND=int32),                  &
    "Number of packed words is incorrect")

CALL assert_equals(expected_data, packed_data, INT(len_packed),                &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field_2d_alloc_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field_1d_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
REAL(KIND=real64)   :: expected_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data, len1_unpacked,   &
                             message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

CALL sample_unpacked_data(expected_data)

CALL assert_equals(expected_data, unpacked_data, INT(len1_unpacked),           &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field_1d_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field_2d_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
REAL(KIND=real64)   :: expected_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

CALL sample_unpacked_data(expected_data)

CALL assert_equals(expected_data, unpacked_data,                               &
    INT(len1_unpacked), INT(len2_unpacked),                                    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field_2d_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field_1d_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
REAL(KIND=real64)   :: expected_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(                                                  &
                packed_data, mdi, unpacked_data, len1_unpacked,                &
                message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

CALL sample_unpacked_data(expected_data)

CALL assert_equals(expected_data, unpacked_data, INT(len1_unpacked),           &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field_1d_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field_2d_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
REAL(KIND=real64)   :: expected_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data, message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

CALL sample_unpacked_data(expected_data)

CALL assert_equals(                                                            &
     expected_data, unpacked_data, INT(len1_unpacked), INT(len2_unpacked),     &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field_2d_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_zeros

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 24

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: expected_packed_data(len_packed)
REAL(KIND=real64)   :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

! Row starting with zeros
unpacked_data(:,1) = [  0.0,  0.0,  3.0,  4.0,  5.0 ]
! Row ending with zeros
unpacked_data(:,2) = [  6.0,  7.0,  8.0,  0.0,  0.0 ]
! Row with middle group of zeros
unpacked_data(:,3) = [ 11.0, 12.0,  0.0,  0.0, 15.0 ]
! Row of all zeros
unpacked_data(:,4) = [  0.0,  0.0,  0.0,  0.0,  0.0 ]
! Rows with random grouped zeros
unpacked_data(:,5) = [  0.0, 22.0,  0.0,  0.0, 25.0 ]
unpacked_data(:,6) = [ 26.0,  0.0, 28.0, 29.0,  0.0 ]
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

expected_packed_data = [ 24_int32,                                             &
                          1_int32,                                             &
                     327686_int32,                                             &
                          0_int32,                                             &
                     131073_int32,                                             &
                  180355072_int32,                                             &
                          0_int32,                                             &
                    8585218_int32,                                             &
                 -402653185_int32,                                             &
                 1912602624_int32,                                             &
                          0_int32,                                             &
                    8650754_int32,                                             &
                 -805306369_int32,                                             &
                 1719664640_int32,                                             &
                          0_int32,                                             &
                          0_int32,                                             &
                          0_int32,                                             &
                    8650754_int32,                                             &
                 1342177279_int32,                                             &
                -1124073472_int32,                                             &
                          0_int32,                                             &
                    8650754_int32,                                             &
                -1207959553_int32,                                             &
                 -554696704_int32     ]

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:num_words),             &
    INT(len_packed),                                                           &
    "Packed array does not agree with expected result")

status = f_shum_wgdos_unpack(packed_data(1:num_words), mdi, unpacked_data,     &
                             message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

expected_unpacked_data(:,1) = [  0.0,  0.0,  4.0,  4.0,  6.0 ]
expected_unpacked_data(:,2) = [  6.0,  8.0,  8.0,  0.0,  0.0 ]
expected_unpacked_data(:,3) = [ 12.0, 12.0,  0.0,  0.0, 16.0 ]
expected_unpacked_data(:,4) = [  0.0,  0.0,  0.0,  0.0,  0.0 ]
expected_unpacked_data(:,5) = [  0.0, 22.0,  0.0,  0.0, 26.0 ]
expected_unpacked_data(:,6) = [ 26.0,  0.0, 28.0, 30.0,  0.0 ]

CALL assert_equals(expected_unpacked_data, unpacked_data,                      &
    INT(len1_unpacked), INT(len2_unpacked),                                    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_zeros

!------------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_mdi

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 26

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: expected_packed_data(len_packed)
REAL(KIND=real64)   :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

accuracy = 1
mdi      = -99.0

! Row starting with mdi
unpacked_data(:,1) = [ -99.0, -99.0,   3.0,   4.0,   5.0 ]
! Row ending with mdi
unpacked_data(:,2) = [   6.0,   7.0,   8.0, -99.0, -99.0 ]
! Row with middle group of mdi
unpacked_data(:,3) = [  11.0,  12.0, -99.0, -99.0,  15.0 ]
! Row of all mdi
unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
! Rows with random grouped mdi
unpacked_data(:,5) = [ -99.0,  22.0, -99.0, -99.0,  25.0 ]
unpacked_data(:,6) = [  26.0, -99.0,  28.0,  29.0, -99.0 ]
  
status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

expected_packed_data = [ 26_int32,                                             &
                          1_int32,                                             &
                     327686_int32,                                             &
                 1094713344_int32,                                             &
                    2162690_int32,                                             &
                 -939524097_int32,                                             &
                  536870912_int32,                                             &
                 1096810496_int32,                                             &
                    2162690_int32,                                             &
                  536870911_int32,                                             &
                 1610612736_int32,                                             &
                 1103101952_int32,                                             &
                    2228226_int32,                                             &
                  939524095_int32,                                             &
                  134217728_int32,                                             &
                -1055916032_int32,                                             &
                    2097153_int32,                                             &
                         -1_int32,                                             &
                 1108738048_int32,                                             &
                    2228226_int32,                                             &
                -1207959553_int32,                                             &
                  536870912_int32,                                             &
                 1109000192_int32,                                             &
                    2228226_int32,                                             &
                 1342177279_int32,                                             &
                  402653184_int32  ]

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:num_words),             &
    INT(len_packed),                                                           &
    "Packed array does not agree with expected result")

status = f_shum_wgdos_unpack(packed_data(1:num_words), mdi, unpacked_data,     &
                             message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

expected_unpacked_data(:,1) = [ -99.0, -99.0,  4.0,    4.0,   6.0 ]
expected_unpacked_data(:,2) = [   6.0,   8.0,  8.0,  -99.0, -99.0 ]
expected_unpacked_data(:,3) = [  12.0,  12.0, -99.0, -99.0,  16.0 ]
expected_unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
expected_unpacked_data(:,5) = [ -99.0,  22.0, -99.0, -99.0,  26.0 ]
expected_unpacked_data(:,6) = [  26.0, -99.0,  28.0,  30.0, -99.0 ]

CALL assert_equals(expected_unpacked_data, unpacked_data,                      &
    INT(len1_unpacked), INT(len2_unpacked),                                    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_mdi

!------------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_zeros_and_mdi

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 22

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: expected_packed_data(len_packed)
REAL(KIND=real64)   :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

accuracy = 1
mdi      = -99.0

! Row with mdi before zeros
unpacked_data(:,1) = [ -99.0, -99.0,   0.0,   0.0,   5.0 ]
! Row with zeros before mdis
unpacked_data(:,2) = [   0.0,   0.0,   8.0, -99.0, -99.0 ]
! Row with alternating zeros and mdis
unpacked_data(:,3) = [   0.0, -99.0,   0.0, -99.0,   0.0 ]
! Row of all mdi
unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
! Rows of all zeros
unpacked_data(:,5) = [   0.0,   0.0,   0.0,   0.0,   0.0 ]
! Row with neither zero nor mdi
unpacked_data(:,6) = [  26.0,  27.0,  28.0,  29.0,  30.0 ]
  
status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(0_int32, status,                                            &
    "Packing of array returned non-zero exit status")

expected_packed_data = [ 22_int32,                                             &
                          1_int32,                                             &
                     327686_int32,                                             &
                          0_int32,                                             &
                    2228226_int32,                                             &
                 -939524097_int32,                                             &
                  201326592_int32,                                             &
                          0_int32,                                             &
                   10682370_int32,                                             &
                  436207615_int32,                                             &
                -2147483648_int32,                                             &
                          0_int32,                                             &
                    2097153_int32,                                             &
                 1476395007_int32,                                             &
                -1055916032_int32,                                             &
                    2097153_int32,                                             &
                         -1_int32,                                             &
                          0_int32,                                             &
                          0_int32,                                             &
                 1109000192_int32,                                             &
                     131073_int32,                                             &
                  377487360_int32  ]

CALL assert_equals(len_packed, num_words,                                      &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:num_words),             &
    INT(len_packed),                                                           &
    "Packed array does not agree with expected result")

status = f_shum_wgdos_unpack(packed_data(1:num_words), mdi, unpacked_data,     &
                             message)

CALL assert_equals(0_int32, status,                                            &
    "Unpacking of array returned non-zero exit status")

expected_unpacked_data(:,1) = [ -99.0, -99.0,   0.0,  0.0,   6.0 ]
expected_unpacked_data(:,2) = [   0.0,   0.0,   8.0, -99.0, -99.0 ]
expected_unpacked_data(:,3) = [   0.0, -99.0,   0.0, -99.0,   0.0 ]
expected_unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
expected_unpacked_data(:,5) = [   0.0,   0.0,   0.0,   0.0,   0.0 ]
expected_unpacked_data(:,6) = [  26.0,  28.0,  28.0,  30.0,  30.0 ]

CALL assert_equals(expected_unpacked_data, unpacked_data,                      &
    INT(len1_unpacked), INT(len2_unpacked),                                    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_zeros_and_mdi

!------------------------------------------------------------------------------!

SUBROUTINE test_read_simple_header_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_read_wgdos_header

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)

INTEGER(KIND=int64) :: words
INTEGER(KIND=int64) :: rows
INTEGER(KIND=int64) :: cols
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

status = f_shum_read_wgdos_header(packed_data, words, accuracy, cols, rows,    &
                                  message)

CALL assert_equals(0_int32, status,                                            &
    "Reading WGDOS header returned non-zero exit status")

CALL assert_equals(len_packed, words,                                          &
    "Packed array word count does not agree with expected result")

CALL assert_equals(1_int64, accuracy,                                          &
    "Packed array accuracy does not agree with expected result")

CALL assert_equals(6_int64, rows,                                              &
    "Packed array row count does not agree with expected result")

CALL assert_equals(5_int64, cols,                                              &
    "Packed array column count does not agree with expected result")

END SUBROUTINE test_read_simple_header_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_read_simple_header_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_read_wgdos_header

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)

INTEGER(KIND=int32) :: words
INTEGER(KIND=int32) :: rows
INTEGER(KIND=int32) :: cols
INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

status = f_shum_read_wgdos_header(packed_data, words, accuracy, cols, rows,    &
                                  message)

CALL assert_equals(0_int32, status,                                            &
    "Reading WGDOS header returned non-zero exit status")

CALL assert_equals(len_packed, words,                                          &
    "Packed array word count does not agree with expected result")

CALL assert_equals(1_int32, accuracy,                                          &
    "Packed array accuracy does not agree with expected result")

CALL assert_equals(6_int32, rows,                                              &
    "Packed array row count does not agree with expected result")

CALL assert_equals(5_int32, cols,                                              &
    "Packed array column count does not agree with expected result")

END SUBROUTINE test_read_simple_header_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_packing_accuracy

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
unpacked_data(3,3) = 999999999999999.9_real64

accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(2_int32, status,                                            &
    "Packing of array with unpackable value returned unexpected exit status")


CALL assert_equals("Unable to WGDOS pack to this accuracy", TRIM(message),     &                   
    "Error message issued different than expected")

END SUBROUTINE test_fail_packing_accuracy

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_packing_return_array_size

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 8

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len_packed)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, accuracy, mdi, packed_data,          &
                           num_words, message)

CALL assert_equals(2_int32, status,                                            &
    "Packing array with too small return array returned unexpected exit status")

CALL assert_equals("Provided array for returning packed data is too small; "// &
    "found 8 words, but requires 21 words", TRIM(message),                     &
    "Error message issued different than expected")

END SUBROUTINE test_fail_packing_return_array_size

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_pack_stride_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len_packed)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked-1, accuracy, mdi,      &
                           packed_data, num_words, message)

CALL assert_equals(1_int32, status,                                            &
    "Passing array with indivisible stride returned unexpected exit status")

CALL assert_equals("1d Field length not divisible by given stride",            &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_pack_stride_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_pack_stride_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int32) :: packed_data(len_packed)

INTEGER(KIND=int32) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int32) :: accuracy
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_starting_data(unpacked_data)
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(unpacked_data, len1_unpacked - 1_int32, accuracy,   &
                           mdi, packed_data, num_words, message)

CALL assert_equals(1_int32, status,                                            &
    "Passing array with indivisible stride returned unexpected exit status")

CALL assert_equals("1d Field length not divisible by given stride",            &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_pack_stride_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_unpack_stride_arg64

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data,                  &
                             len1_unpacked - 1_int32, message)

CALL assert_equals(1_int32, status,                                            &
    "Passing array with indivisible stride returned unexpected exit status")

CALL assert_equals("1d Field length not divisible by given stride",            &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_unpack_stride_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_unpack_stride_arg32

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int32), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int32), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int32), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real32)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data,                  &
                             len1_unpacked - 1_int32, message)

CALL assert_equals(1_int32, status,                                            &
    "Passing array with indivisible stride returned unexpected exit status")

CALL assert_equals("1d Field length not divisible by given stride",            &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_unpack_stride_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_unpack_too_many_elements

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 1
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 1
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

CALL sample_packed_data(packed_data)

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data(1:4), mdi, unpacked_data, message)

CALL assert_equals(2_int32, status,                                            &
    "Passing array with too many elements returned unexpected exit status")

CALL assert_equals("Packed field header reports 21 words, but provided "     //&
    "field is 4 words in length",                                              &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_unpack_too_many_elements

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_unpack_inconsistent

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT16_T

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 21

INTEGER(KIND=int32) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int32) :: status
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

! Additional variables and parameters used to reverse-engineer a specific
! packed value which will force failure of the consistency check
INTEGER(KIND=int32) :: bad_word_p1
INTEGER(KIND=int32) :: bad_word_p2
INTEGER, PARAMETER  :: int16  = C_INT16_T
INTEGER(KIND=int32), PARAMETER ::                                              &
                     mask16  = INT(HUGE(0_int16), KIND=int64)*2 + 1

CALL sample_packed_data(packed_data)

! The consistency check is making sure the total size of data stored in each
! of the packed rows once added together doesn't exceed the total size to be
! unpacked... so we want to increase the first row-size value to cause the
! check to fail

! However, the sample array used here is a fairly simple case; each row is 
! actually packed into a single value - so the row size increase has to be
! by a specific amount to work correctly (3 points)

! Now split the first row-header's 2nd word into two 16-bit words - the 
! second of these is the word-count for the row
bad_word_p1 = ISHFT(packed_data(5), -16_int32)
bad_word_p2 = IAND(packed_data(5), mask16)

! We need to increase this by 3 to cause the failure; increasing it by less
! than that doesn't trigger the error due to an edge case specific to this
! simple example
bad_word_p2 = bad_word_p2 + 3

! Now reconstruct and replace the original value
packed_data(5) = IOR(bad_word_p2, ISHFT(bad_word_p1, 16_int32))

mdi = -99.0

status = f_shum_wgdos_unpack(packed_data, mdi, unpacked_data, message)

CALL assert_equals(2_int32, status,                                            &
    "Passing array with inconsistent data returned unexpected exit status")

CALL assert_equals("Compressed data inconsistent",                             &
    TRIM(message), "Error message issued different than expected")

END SUBROUTINE test_fail_unpack_inconsistent

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_wgdos_packing_mod
