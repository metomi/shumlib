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
MODULE fruit_test_shum_fieldsfile_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_INT, C_BOOL

IMPLICIT NONE 

PRIVATE

PUBLIC :: fruit_test_shum_fieldsfile

! Interface to call exit with status
!------------------------------------------------------------------------------!
INTERFACE
SUBROUTINE c_exit(status)  BIND(c,NAME="exit")
  IMPORT :: C_INT
  IMPLICIT NONE
  INTEGER(KIND=C_INT), VALUE, INTENT(IN) :: status
END SUBROUTINE
END INTERFACE

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

! The unit, filename, and id of the temporary file used by these tests
INTEGER(KIND=int64), PARAMETER :: scratch_test_unit = 20
INTEGER                        :: shum_tmpdir_len
CHARACTER(LEN=:), ALLOCATABLE  :: shum_tmpdir

CONTAINS

SUBROUTINE fruit_test_shum_fieldsfile

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
USE f_shum_fieldsfile_version_mod, ONLY: get_shum_fieldsfile_version

IMPLICIT NONE 

INTEGER(KIND=int64) :: version
INTEGER             :: get_env_status

! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's 
! sufficient to make sure it is callable; but let's print the version for info.
version = get_shum_fieldsfile_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_fieldsfile at Shumlib version: ", version

! Retrieve a suitable directory to use as a scratch area from the environment
! variable.  Call once to retreive the length of the variable
CALL GET_ENVIRONMENT_VARIABLE("SHUM_TMPDIR", LENGTH=shum_tmpdir_len,           &
                              STATUS=get_env_status)

! If the variable exists call again to read it in
IF (get_env_status == 0) THEN
  ALLOCATE(CHARACTER(shum_tmpdir_len) :: shum_tmpdir)
  CALL GET_ENVIRONMENT_VARIABLE("SHUM_TMPDIR",                                 &
                                VALUE=shum_tmpdir,                             &
                                STATUS=get_env_status)
END IF
! Now check the status (not an ELSE IF, because that way we can catch the 
! failed status of either the first or second call
IF (get_env_status /= 0) THEN
  WRITE(ERROR_UNIT, "(A)") "Unable to access SHUM_TMPDIR environment variable"
  CALL c_exit(INT(get_env_status, KIND=C_INT))
END IF

CALL run_test_case(                                                            &
  test_end_to_end_direct_write_file, "end_to_end_direct_write_file")
CALL run_test_case(                                                            &
  test_end_to_end_sequential_write_file, "end_to_end_sequential_write_file")
CALL run_test_case(                                                            &
  test_stashmaster_read, "stashmaster_read")

END SUBROUTINE fruit_test_shum_fieldsfile

!------------------------------------------------------------------------------!

SUBROUTINE test_end_to_end_direct_write_file

USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_create_file, f_shum_write_fixed_length_header,                        &
  f_shum_fixed_length_header_len, f_shum_write_integer_constants,              &
  f_shum_write_real_constants, f_shum_write_level_dependent_constants,         &
  f_shum_write_row_dependent_constants,                                        &
  f_shum_write_column_dependent_constants,                                     &
  f_shum_write_additional_parameters, f_shum_write_extra_constants,            &
  f_shum_write_temp_histfile, f_shum_write_compressed_index,                   &
  f_shum_write_lookup, f_shum_lookup_dim1_len,                                 &
  f_shum_precalc_data_positions, f_shum_write_field_data,                      &
  f_shum_close_file, f_shum_open_file,                                         &
  f_shum_read_fixed_length_header, f_shum_fixed_length_header_len,             &
  f_shum_lookup_dim1_len, f_shum_read_integer_constants,                       &
  f_shum_read_real_constants, f_shum_read_level_dependent_constants,           &
  f_shum_read_row_dependent_constants, f_shum_read_column_dependent_constants, &
  f_shum_read_additional_parameters, f_shum_read_extra_constants,              &
  f_shum_read_temp_histfile, f_shum_read_compressed_index,                     &
  f_shum_read_lookup, f_shum_lookup_dim1_len, f_shum_read_field_data

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lbuser1, lbpack, lbrel, lblrec, lbnrec, lbegin, lbuser2

USE f_shum_fixed_length_header_indices_mod, ONLY:                              &
  int_const_start, int_const_dim, real_const_start, real_const_dim,            &
  lev_dep_const_start, lev_dep_const_dim1, lev_dep_const_dim2,                 &
  row_dep_const_start, row_dep_const_dim1, row_dep_const_dim2,                 &  
  col_dep_const_start, col_dep_const_dim1, col_dep_const_dim2,                 &
  additional_const_start, additional_const_dim1, additional_const_dim2,        &
  extra_const_start, extra_const_dim, temp_histfile_start, temp_histfile_dim,  &
  comp_field_index1_start, comp_field_index1_dim,                              &
  comp_field_index2_start, comp_field_index2_dim,                              &
  comp_field_index3_start, comp_field_index3_dim,                              &
  lookup_start, lookup_dim1, lookup_dim2, data_start, data_dim,                &
  num_prognostic_fields

IMPLICIT NONE 

INTEGER(KIND=int64) :: status
CHARACTER(LEN=500)  :: message = ""
INTEGER(KIND=int64) :: ff_id

CHARACTER(LEN=*), PARAMETER   :: tempfile="fruit_test_fieldsfile_direct.ff"
CHARACTER(LEN=:), ALLOCATABLE :: scratch_filename

! Sizes of test arrays used for this test
INTEGER(KIND=int64), PARAMETER :: int_const_dim_test = 46
INTEGER(KIND=int64), PARAMETER :: real_const_dim_test = 38
INTEGER(KIND=int64), PARAMETER :: lev_const_dim1_test = 20
INTEGER(KIND=int64), PARAMETER :: lev_const_dim2_test = 4
INTEGER(KIND=int64), PARAMETER :: row_const_dim1_test = 50
INTEGER(KIND=int64), PARAMETER :: row_const_dim2_test = 2
INTEGER(KIND=int64), PARAMETER :: col_const_dim1_test = 30
INTEGER(KIND=int64), PARAMETER :: col_const_dim2_test = 2
INTEGER(KIND=int64), PARAMETER :: addl_param_dim1_test = 10
INTEGER(KIND=int64), PARAMETER :: addl_param_dim2_test = 3
INTEGER(KIND=int64), PARAMETER :: extra_const_dim_test = 12
INTEGER(KIND=int64), PARAMETER :: temp_hist_dim_test = 15
INTEGER(KIND=int64), PARAMETER :: comp_ind_1_dim_test = 20
INTEGER(KIND=int64), PARAMETER :: comp_ind_2_dim_test = 30
INTEGER(KIND=int64), PARAMETER :: comp_ind_3_dim_test = 40
INTEGER(KIND=int64), PARAMETER :: n_fields_test = 13
INTEGER(KIND=int64), PARAMETER :: rows = 30
INTEGER(KIND=int64), PARAMETER :: columns = 20

! Variables used for writing and storing expected values
INTEGER(KIND=int64) :: fixed_length_header(f_shum_fixed_length_header_len)
INTEGER(KIND=int64) :: integer_constants(int_const_dim_test)
REAL(KIND=real64)   :: real_constants(real_const_dim_test)
REAL(KIND=real64)   :: level_constants(lev_const_dim1_test, lev_const_dim2_test)
REAL(KIND=real64)   :: row_constants(row_const_dim1_test, row_const_dim2_test)
REAL(KIND=real64)   :: col_constants(col_const_dim1_test, col_const_dim2_test)
REAL(KIND=real64)   :: addl_params(addl_param_dim1_test, addl_param_dim2_test)
REAL(KIND=real64)   :: extra_constants(extra_const_dim_test)
REAL(KIND=real64)   :: temp_histfile(temp_hist_dim_test)
REAL(KIND=real64)   :: comp_index_1(comp_ind_1_dim_test)
REAL(KIND=real64)   :: comp_index_2(comp_ind_2_dim_test)
REAL(KIND=real64)   :: comp_index_3(comp_ind_3_dim_test)
INTEGER(KIND=int64) :: lookup(f_shum_lookup_dim1_len, n_fields_test)
REAL(KIND=real64)   :: r64_field_data(rows*columns)
INTEGER(KIND=int64) :: i64_field_data(rows*columns)
REAL(KIND=real32)   :: r32_field_data(rows*columns)
INTEGER(KIND=int32) :: i32_field_data(rows*columns)

! Variables used for re-reading the values
INTEGER(KIND=int64) :: fixed_length_header_r(f_shum_fixed_length_header_len)
INTEGER(KIND=int64), ALLOCATABLE :: integer_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: real_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: level_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: row_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: col_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: addl_params_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: extra_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: temp_histfile_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_1_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_2_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_3_r(:)
INTEGER(KIND=int64), ALLOCATABLE :: lookup_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: r64_field_data_r(:)
INTEGER(KIND=int64), ALLOCATABLE :: i64_field_data_r(:)
REAL(KIND=real32),   ALLOCATABLE :: r32_field_data_r(:)
INTEGER(KIND=int32), ALLOCATABLE :: i32_field_data_r(:)

INTEGER :: i, j, k
INTEGER :: failures_at_entry
INTEGER :: failures_at_exit

LOGICAL(KIND=bool) :: check

! Get the number of failed tests prior to this test starting
CALL get_failed_count(failures_at_entry)

! Create a temporary file to use for testing 
ALLOCATE(CHARACTER(shum_tmpdir_len + LEN(tempfile)) :: scratch_filename)
scratch_filename = shum_tmpdir // '/' // tempfile

status = f_shum_create_file(scratch_filename, n_fields_test + 2, ff_id, message)

CALL assert_equals(0_int64, status,                                            & 
                                   "Failed to create new file: "//TRIM(message))

DO i = 1, f_shum_fixed_length_header_len
  fixed_length_header(i) = INT(i, KIND=int64)
END DO

! Make sure first word is valid
fixed_length_header(1) = 20_int64

status = f_shum_write_fixed_length_header(ff_id, fixed_length_header, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write fixed_length_header: "//TRIM(message))

DO i = 1, int_const_dim_test
  integer_constants(i) = INT(i + 100, KIND=int64)
END DO

status = f_shum_write_integer_constants(ff_id, integer_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write integer_constants: "//TRIM(message))

DO i = 1, real_const_dim_test
  real_constants(i) = REAL(i + 200, KIND=real64)
END DO

status = f_shum_write_real_constants(ff_id, real_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write real_constants: "//TRIM(message))

DO i = 1, lev_const_dim1_test
  DO j = 1, lev_const_dim2_test
    level_constants(i, j) =                                                    &
                     REAL(i + (j-1)*lev_const_dim1_test + 300, KIND=real64)
  END DO
END DO

status = f_shum_write_level_dependent_constants(ff_id, level_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                   "Failed to write level_dependent_constants: "//TRIM(message))

DO i = 1, row_const_dim1_test
  DO j = 1, row_const_dim2_test
    row_constants(i, j) =                                                      &
                       REAL(i + (j-1)*row_const_dim1_test + 400, KIND=real64)
  END DO
END DO

status = f_shum_write_row_dependent_constants(ff_id, row_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                     "Failed to write row_dependent_constants: "//TRIM(message))

DO i = 1, col_const_dim1_test
  DO j = 1, col_const_dim2_test
    col_constants(i, j) =                                                      &
                       REAL(i + (j-1)*col_const_dim1_test + 500, KIND=real64)
  END DO
END DO

status = f_shum_write_column_dependent_constants(ff_id, col_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                  "Failed to write column_dependent_constants: "//TRIM(message))

DO i = 1, addl_param_dim1_test
  DO j = 1, addl_param_dim2_test
    addl_params(i, j) =                                                        &
                       REAL(i + (j-1)*addl_param_dim1_test + 600, KIND=real64)
  END DO
END DO

status = f_shum_write_additional_parameters(ff_id, addl_params, message)

CALL assert_equals(0_int64, status,                                            & 
                       "Failed to write additional_parameters: "//TRIM(message))

DO i = 1, extra_const_dim_test
  extra_constants(i) = REAL(i + 700, KIND=real64)
END DO

status = f_shum_write_extra_constants(ff_id, extra_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                             "Failed to write extra_constants: "//TRIM(message))

DO i = 1, temp_hist_dim_test
  temp_histfile(i) = REAL(i + 800, KIND=real64)
END DO

status = f_shum_write_temp_histfile(ff_id, temp_histfile, message)

CALL assert_equals(0_int64, status,                                            & 
                               "Failed to write temp_histfile: "//TRIM(message))

DO i = 1, comp_ind_1_dim_test
  comp_index_1(i) = REAL(i + 900, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_1, 1_int64, message)

CALL assert_equals(0_int64, status,                                            & 
                          "Failed to write compressed_index 1: "//TRIM(message))

DO i = 1, comp_ind_2_dim_test
  comp_index_2(i) = REAL(i + 1000, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_2, 2_int64, message)

CALL assert_equals(0_int64, status,                                            & 
                          "Failed to write compressed_index 2: "//TRIM(message))


DO i = 1, comp_ind_3_dim_test
  comp_index_3(i) = REAL(i + 1100, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_3, 3_int64, message)

CALL assert_equals(0_int64, status,                                            & 
                          "Failed to write compressed_index 3: "//TRIM(message))

DO i = 1, f_shum_lookup_dim1_len
  DO j = 1, n_fields_test
    lookup(i, j) = INT(i + (j-1)*f_shum_lookup_dim1_len, KIND=int64)
  END DO
END DO

lookup(lbrel,   1:5)   = [   3,   3,   3,   3,   3 ]
lookup(lbuser1, 1:5)   = [   1,   2,   1,   1,   1 ]
lookup(lbpack,  1:5)   = [   0,   0,   2,   1,   0 ]

lookup(lbrel,   6:10)  = [   3,   3,   3,   3,   3 ]
lookup(lbuser1, 6:10)  = [   1,   2,   2,   2,   1 ]
lookup(lbpack,  6:10)  = [   2,   0,   2,   0,   0 ]

lookup(lbrel,   11:13) = [   3,   3,   3 ]
lookup(lbuser1, 11:13) = [   1,   1,   1 ]
lookup(lbpack,  11:13) = [   1,   2,   0 ]

status = f_shum_write_lookup(ff_id, lookup, 1_int64, message)

CALL assert_equals(0_int64, status,                                            & 
                                "Failed to write lookup table: "//TRIM(message))

status = f_shum_precalc_data_positions(ff_id, rows*columns, message)

CALL assert_equals(0_int64, status,                                            & 
                       "Failed to precalculate data positions: "//TRIM(message))

DO k = 1, n_fields_test
  SELECT CASE (k)
    ! Real 64-bit data fields
    CASE(1,5,10,13)
      DO i = 1, rows
        DO j = 1, columns
          r64_field_data(i + (j-1)*rows) = REAL(i + (j-1)*rows, KIND=real64)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                             ff_id, INT(k, KIND=int64), r64_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write real 64-bit data: "//TRIM(message))
    ! Integer 64-bit data fields
    CASE(2,7,9)
      DO i = 1, rows
        DO j = 1, columns
          i64_field_data(i + (j-1)*rows) = INT(i + (j-1)*rows, KIND=int64)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                             ff_id, INT(k, KIND=int64), i64_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write integer 64-bit data: "//TRIM(message))
    ! Real 32-bit data fields
    CASE(3,6,12)
      DO i = 1, rows
        DO j = 1, columns
          r32_field_data(i + (j-1)*rows) = REAL(i + (j-1)*rows, KIND=real32)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                             ff_id, INT(k, KIND=int64), r32_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write real 32-bit data: "//TRIM(message))
    ! Integer 32-bit data fields
    CASE(4,8,11)
      DO i = 1, rows
        DO j = 1, columns
          i32_field_data(i + (j-1)*rows) = INT(i + (j-1)*rows, KIND=int32)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                             ff_id, INT(k, KIND=int64), i32_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write integer 32-bit data: "//TRIM(message))
  END SELECT
END DO

status = f_shum_close_file(ff_id, message)

CALL assert_equals(0_int64, status, "Failed to close file: "//TRIM(message))

status = f_shum_open_file(scratch_filename, ff_id, message)

CALL assert_equals(0_int64, status, "Failed to re-open file: "//TRIM(message))

status = f_shum_read_fixed_length_header(ff_id, fixed_length_header_r, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to read fixed_length_header: "//TRIM(message))

DO i = int_const_dim, f_shum_fixed_length_header_len
  fixed_length_header(i) = -32768_int64
END DO
fixed_length_header(num_prognostic_fields) = 153_int64

fixed_length_header(int_const_start) = f_shum_fixed_length_header_len + 1
fixed_length_header(int_const_dim)   = int_const_dim_test

fixed_length_header(real_const_dim)   = real_const_dim_test
fixed_length_header(real_const_start) =                                        &
  fixed_length_header(int_const_start) + fixed_length_header(int_const_dim)

fixed_length_header(lev_dep_const_dim1)  = lev_const_dim1_test
fixed_length_header(lev_dep_const_dim2)  = lev_const_dim2_test
fixed_length_header(lev_dep_const_start) =                                     &
  fixed_length_header(real_const_start) + fixed_length_header(real_const_dim)

fixed_length_header(row_dep_const_dim1)  = row_const_dim1_test
fixed_length_header(row_dep_const_dim2)  = row_const_dim2_test
fixed_length_header(row_dep_const_start) =                                     &
  fixed_length_header(lev_dep_const_start) +                                   &
    fixed_length_header(lev_dep_const_dim1) *                                  &
    fixed_length_header(lev_dep_const_dim2)

fixed_length_header(col_dep_const_dim1)  = col_const_dim1_test
fixed_length_header(col_dep_const_dim2)  = col_const_dim2_test
fixed_length_header(col_dep_const_start) =                                     &
  fixed_length_header(row_dep_const_start) +                                   &
    fixed_length_header(row_dep_const_dim1)  *                                 &
    fixed_length_header(row_dep_const_dim2)

fixed_length_header(additional_const_dim1) = addl_param_dim1_test
fixed_length_header(additional_const_dim2) = addl_param_dim2_test
fixed_length_header(additional_const_start) =                                  &
  fixed_length_header(col_dep_const_start) +                                   &
    fixed_length_header(col_dep_const_dim1)  *                                 &
    fixed_length_header(col_dep_const_dim2)

fixed_length_header(extra_const_dim) = extra_const_dim_test
fixed_length_header(extra_const_start) =                                       &
  fixed_length_header(additional_const_start) +                                &
    fixed_length_header(additional_const_dim1)  *                              &
    fixed_length_header(additional_const_dim2)

fixed_length_header(temp_histfile_dim) = temp_hist_dim_test
fixed_length_header(temp_histfile_start) =                                     &
  fixed_length_header(extra_const_start) + fixed_length_header(extra_const_dim)  

fixed_length_header(comp_field_index1_dim) = comp_ind_1_dim_test
fixed_length_header(comp_field_index1_start) =                                 &
  fixed_length_header(temp_histfile_start) +                                   &
  fixed_length_header(temp_histfile_dim)  

fixed_length_header(comp_field_index2_dim) = comp_ind_2_dim_test
fixed_length_header(comp_field_index2_start) =                                 &
  fixed_length_header(comp_field_index1_start) +                               &
  fixed_length_header(comp_field_index1_dim)  

fixed_length_header(comp_field_index3_dim) = comp_ind_3_dim_test
fixed_length_header(comp_field_index3_start) =                                 &
  fixed_length_header(comp_field_index2_start) +                               &
  fixed_length_header(comp_field_index2_dim)  

fixed_length_header(lookup_dim1) = f_shum_lookup_dim1_len
fixed_length_header(lookup_dim2) = n_fields_test
fixed_length_header(lookup_start) =                                            &
  fixed_length_header(comp_field_index3_start) +                               &
  fixed_length_header(comp_field_index3_dim)  

fixed_length_header(data_start) = 524289_int64
fixed_length_header(data_dim)   = 535552_int64

CALL assert_equals(fixed_length_header, fixed_length_header_r,                 &
  INT(f_shum_fixed_length_header_len, KIND=int32),                             &
  "Re-read fixed_length_header does not contain expected values")

status = f_shum_read_integer_constants(ff_id, integer_constants_r, message)

CALL assert_equals(0_int64, status,                                            & 
                   "Failed to read integer_constants: "//TRIM(message))

check = ALLOCATED(integer_constants_r)
CALL assert_true(check, "Re-read integer_contants not allocated by read call")

CALL assert_equals(int_const_dim_test, SIZE(integer_constants_r, KIND=int64),  &
                   "Re-read integer_constants incorrect size")

CALL assert_equals(integer_constants, integer_constants_r,                     &
                   INT(int_const_dim_test, KIND=int32),                        &
                   "Re-read integer_constants contain incorrect values")

status = f_shum_read_real_constants(ff_id, real_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read real_constants: "//TRIM(message))

check = ALLOCATED(real_constants_r)
CALL assert_true(check, "Re-read real_contants not allocated by read call")

CALL assert_equals(real_const_dim_test, SIZE(real_constants_r, KIND=int64),    &
                   "Re-read real_constants incorrect size")

CALL assert_equals(real_constants, real_constants_r,                           &
                   INT(real_const_dim_test, KIND=int32),                       &
                   "Re-read real_constants contain incorrect values")

status = f_shum_read_level_dependent_constants(                                &
                                              ff_id, level_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read level_dependent_constants: "//TRIM(message))

check = ALLOCATED(level_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read level_dependent_contants not allocated by read call")

CALL assert_equals(lev_const_dim1_test,                                        &
                   SIZE(level_constants_r, 1, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 1st dimension")

CALL assert_equals(lev_const_dim2_test,                                        &
                   SIZE(level_constants_r, 2, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 2nd dimension")

CALL assert_equals(level_constants, level_constants_r,                         &
                   INT(lev_const_dim1_test, KIND=int32),                       &
                   INT(lev_const_dim2_test, KIND=int32),                       &
                   "Re-read level_dependent_constants contain incorrect values")

status = f_shum_read_row_dependent_constants(ff_id, row_constants_r, message)
                                                
CALL assert_equals(0_int64, status,                                            &
                   "Failed to read row_dependent_constants: "//TRIM(message))

check = ALLOCATED(row_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read row_dependent_contants not allocated by read call")

CALL assert_equals(row_const_dim1_test,                                        &
                   SIZE(row_constants_r, 1, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 1st dimension")

CALL assert_equals(row_const_dim2_test,                                        &
                   SIZE(row_constants_r, 2, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 2nd dimension")

CALL assert_equals(row_constants, row_constants_r,                             &
                   INT(row_const_dim1_test, KIND=int32),                       &
                   INT(row_const_dim2_test, KIND=int32),                       &
                   "Re-read row_dependent_constants contain incorrect values")

status = f_shum_read_column_dependent_constants(ff_id, col_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read column_dependent_constants: "//TRIM(message))

check = ALLOCATED(col_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read column_dependent_contants not allocated by read call")

CALL assert_equals(col_const_dim1_test,                                        &
                   SIZE(col_constants_r, 1, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 1st dimension")

CALL assert_equals(col_const_dim2_test,                                        &
                   SIZE(col_constants_r, 2, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 2nd dimension")

CALL assert_equals(col_constants, col_constants_r,                             &
                   INT(col_const_dim1_test, KIND=int32),                       &
                   INT(col_const_dim2_test, KIND=int32),                       &
                  "Re-read column_dependent_constants contain incorrect values")

status = f_shum_read_additional_parameters(ff_id, addl_params_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read additional_parameters: "//TRIM(message))

check = ALLOCATED(addl_params_r)
CALL assert_true(check,                                                        &
                 "Re-read additional_parameters not allocated by read call")

CALL assert_equals(addl_param_dim1_test,                                       &
                   SIZE(addl_params_r, 1, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 1st dimension")

CALL assert_equals(addl_param_dim2_test,                                       &
                   SIZE(addl_params_r, 2, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 2nd dimension")

CALL assert_equals(addl_params, addl_params_r,                                 &
                   INT(addl_param_dim1_test, KIND=int32),                      &
                   INT(addl_param_dim2_test, KIND=int32),                      &
                  "Re-read additional_parameters contain incorrect values")

status = f_shum_read_extra_constants(ff_id, extra_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read extra_constants: "//TRIM(message))

check = ALLOCATED(extra_constants_r)
CALL assert_true(check, "Re-read extra_constants not allocated by read call")

CALL assert_equals(extra_const_dim_test, SIZE(extra_constants_r, KIND=int64),  &
                   "Re-read extra_constants incorrect size")

CALL assert_equals(extra_constants, extra_constants_r,                         &
                   INT(extra_const_dim_test, KIND=int32),                      &
                   "Re-read extra_constants contain incorrect values")

status = f_shum_read_temp_histfile(ff_id, temp_histfile_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read temp_histfile: "//TRIM(message))

check = ALLOCATED(temp_histfile_r)
CALL assert_true(check, "Re-read temp_histfile not allocated by read call")

CALL assert_equals(temp_hist_dim_test, SIZE(temp_histfile_r, KIND=int64),      &
                   "Re-read temp_histfile incorrect size")

CALL assert_equals(temp_histfile, temp_histfile_r,                             &
                   INT(temp_hist_dim_test, KIND=int32),                        &
                   "Re-read temp_histfile contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_1_r, 1_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 1: "//TRIM(message))

check = ALLOCATED(comp_index_1_r)
CALL assert_true(check, "Re-read compressed_index 1 not allocated by read call")

CALL assert_equals(comp_ind_1_dim_test, SIZE(comp_index_1_r, KIND=int64),      &
                   "Re-read compressed_index 1 incorrect size")

CALL assert_equals(comp_index_1, comp_index_1_r,                               &
                   INT(comp_ind_1_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 1 contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_2_r, 2_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 2: "//TRIM(message))

check = ALLOCATED(comp_index_2_r)
CALL assert_true(check, "Re-read compressed_index 2 not allocated by read call")

CALL assert_equals(comp_ind_2_dim_test, SIZE(comp_index_2_r, KIND=int64),      &
                   "Re-read compressed_index 2 incorrect size")

CALL assert_equals(comp_index_2, comp_index_2_r,                               &
                   INT(comp_ind_2_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 2 contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_3_r, 3_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 3: "//TRIM(message))

check = ALLOCATED(comp_index_3_r)
CALL assert_true(check, "Re-read compressed_index 3 not allocated by read call")

CALL assert_equals(comp_ind_3_dim_test, SIZE(comp_index_3_r, KIND=int64),      &
                   "Re-read compressed_index 3 incorrect size")

CALL assert_equals(comp_index_3, comp_index_3_r,                               &
                   INT(comp_ind_3_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 3 contain incorrect values")

status = f_shum_read_lookup(ff_id, lookup_r, message)

CALL assert_equals(0_int64, status, "Failed to read lookup: "//TRIM(message))

check = ALLOCATED(lookup_r)
CALL assert_true(check, "Re-read lookup not allocated by read call")

CALL assert_equals(f_shum_lookup_dim1_len,                                     &
                   SIZE(lookup_r, 1, KIND=int64),                              &
                   "Re-read lookup incorrect 1st dimension")

CALL assert_equals(n_fields_test,                                              &
                   SIZE(lookup_r, 2, KIND=int64),                              &
                   "Re-read lookup incorrect 2nd dimension")

lookup(lblrec,  1:5)   = [ 600, 600, 600, 300, 600 ]
lookup(lbnrec,  1:5)   = [1024,1024, 512,1024,1024 ]

lookup(lblrec,  6:10)  = [ 600, 600, 600, 600, 600 ]
lookup(lbnrec,  6:10)  = [ 512,1024, 512,1024,1024 ]

lookup(lblrec,  11:13) = [ 300, 600, 600 ]
lookup(lbnrec,  11:13) = [1024, 512,1024 ]

lookup(lbegin, 1) = 524288_int64
lookup(lbuser2, 1) = 1_int64
DO j = 2, n_fields_test
  lookup(lbegin, j) = lookup(lbegin, j-1) + lookup(lbnrec, j-1)
  lookup(lbuser2, j) = lookup(lbuser2, j-1) + lookup(lbnrec, j-1)
END DO

CALL assert_equals(lookup, lookup_r,                                           &
                   INT(f_shum_lookup_dim1_len, KIND=int32),                    &
                   INT(n_fields_test, KIND=int32),                             &
                  "Re-read lookup contains incorrect values")

DO k = 1, n_fields_test
  SELECT CASE (k)
    ! Real 64-bit data fields
    CASE(1,5,10,13)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), r64_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read real 64-bit data: "//TRIM(message))
      check = ALLOCATED(r64_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read real 64-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(r64_field_data_r, 1, KIND=int64),                              &
           "Re-read real 64-bit data array incorrect length")
      CALL assert_equals(r64_field_data, r64_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read real 64-bit data array contains incorrect values")
    ! Integer 64-bit data fields
    CASE(2,7,9)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), i64_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read integer 64-bit data: "//TRIM(message))
      check = ALLOCATED(i64_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read integer 64-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(i64_field_data_r, 1, KIND=int64),                              &
           "Re-read integer 64-bit data array incorrect length")
      CALL assert_equals(i64_field_data, i64_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read integer 64-bit data array contains incorrect values")
    ! Real 32-bit data fields
    CASE(3,6,12)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), r32_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read real 32-bit data: "//TRIM(message))
      check = ALLOCATED(r32_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read real 32-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(r32_field_data, 1, KIND=int64),                                &
           "Re-read real 32-bit data array incorrect length")
      CALL assert_equals(r32_field_data, r32_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read real 32-bit data array contains incorrect values")
    ! Integer 32-bit data fields
    CASE(4,8,11)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), i32_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read integer 32-bit data: "//TRIM(message))
      check = ALLOCATED(i32_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read integer 32-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(i32_field_data, 1, KIND=int64),                                &
           "Re-read integer 32-bit data array incorrect length")
      CALL assert_equals(i32_field_data, i32_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read integer 32-bit data array contains incorrect values")
  END SELECT
END DO

status = f_shum_close_file(ff_id, message)

CALL assert_equals(0_int64, status, "Failed to close file: "//TRIM(message))

! Tidy up the temporary file assuming the tests completed okay
CALL get_failed_count(failures_at_exit)
IF (failures_at_exit - failures_at_entry == 0) THEN
  OPEN(scratch_test_unit, FILE=scratch_filename, STATUS="OLD")
  CLOSE(scratch_test_unit, STATUS="DELETE")
END IF

END SUBROUTINE test_end_to_end_direct_write_file

!------------------------------------------------------------------------------!

SUBROUTINE test_end_to_end_sequential_write_file

USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_create_file, f_shum_write_fixed_length_header,                        &
  f_shum_fixed_length_header_len, f_shum_write_integer_constants,              &
  f_shum_write_real_constants, f_shum_write_level_dependent_constants,         &
  f_shum_write_row_dependent_constants,                                        &
  f_shum_write_column_dependent_constants,                                     &
  f_shum_write_additional_parameters, f_shum_write_extra_constants,            &
  f_shum_write_temp_histfile, f_shum_write_compressed_index,                   &
  f_shum_write_lookup, f_shum_lookup_dim1_len,                                 &
  f_shum_write_field_data,                                                     &
  f_shum_close_file, f_shum_open_file,                                         &
  f_shum_read_fixed_length_header, f_shum_fixed_length_header_len,             &
  f_shum_lookup_dim1_len, f_shum_read_integer_constants,                       &
  f_shum_read_real_constants, f_shum_read_level_dependent_constants,           &
  f_shum_read_row_dependent_constants, f_shum_read_column_dependent_constants, &
  f_shum_read_additional_parameters, f_shum_read_extra_constants,              &
  f_shum_read_temp_histfile, f_shum_read_compressed_index,                     &
  f_shum_read_lookup, f_shum_lookup_dim1_len, f_shum_read_field_data

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lbuser1, lbpack, lbrel, lblrec, lbnrec, lbegin, lbuser2

USE f_shum_fixed_length_header_indices_mod, ONLY:                              &
  int_const_start, int_const_dim, real_const_start, real_const_dim,            &
  lev_dep_const_start, lev_dep_const_dim1, lev_dep_const_dim2,                 &
  row_dep_const_start, row_dep_const_dim1, row_dep_const_dim2,                 &  
  col_dep_const_start, col_dep_const_dim1, col_dep_const_dim2,                 &
  additional_const_start, additional_const_dim1, additional_const_dim2,        &
  extra_const_start, extra_const_dim, temp_histfile_start, temp_histfile_dim,  &
  comp_field_index1_start, comp_field_index1_dim,                              &
  comp_field_index2_start, comp_field_index2_dim,                              &
  comp_field_index3_start, comp_field_index3_dim,                              &
  lookup_start, lookup_dim1, lookup_dim2, data_start, data_dim,                &
  num_prognostic_fields

IMPLICIT NONE 

INTEGER(KIND=int64) :: status
CHARACTER(LEN=500)  :: message = ""
INTEGER(KIND=int64) :: ff_id

CHARACTER(LEN=*), PARAMETER   :: tempfile="fruit_test_fieldsfile_sequential.ff"
CHARACTER(LEN=:), ALLOCATABLE :: scratch_filename

! Sizes of test arrays used for this test
INTEGER(KIND=int64), PARAMETER :: int_const_dim_test = 46
INTEGER(KIND=int64), PARAMETER :: real_const_dim_test = 38
INTEGER(KIND=int64), PARAMETER :: lev_const_dim1_test = 20
INTEGER(KIND=int64), PARAMETER :: lev_const_dim2_test = 4
INTEGER(KIND=int64), PARAMETER :: row_const_dim1_test = 50
INTEGER(KIND=int64), PARAMETER :: row_const_dim2_test = 2
INTEGER(KIND=int64), PARAMETER :: col_const_dim1_test = 30
INTEGER(KIND=int64), PARAMETER :: col_const_dim2_test = 2
INTEGER(KIND=int64), PARAMETER :: addl_param_dim1_test = 10
INTEGER(KIND=int64), PARAMETER :: addl_param_dim2_test = 3
INTEGER(KIND=int64), PARAMETER :: extra_const_dim_test = 12
INTEGER(KIND=int64), PARAMETER :: temp_hist_dim_test = 15
INTEGER(KIND=int64), PARAMETER :: comp_ind_1_dim_test = 20
INTEGER(KIND=int64), PARAMETER :: comp_ind_2_dim_test = 30
INTEGER(KIND=int64), PARAMETER :: comp_ind_3_dim_test = 40
INTEGER(KIND=int64), PARAMETER :: n_fields_test = 13
INTEGER(KIND=int64), PARAMETER :: rows = 30
INTEGER(KIND=int64), PARAMETER :: columns = 20

! Variables used for writing and storing expected values
INTEGER(KIND=int64) :: fixed_length_header(f_shum_fixed_length_header_len)
INTEGER(KIND=int64) :: integer_constants(int_const_dim_test)
REAL(KIND=real64)   :: real_constants(real_const_dim_test)
REAL(KIND=real64)   :: level_constants(lev_const_dim1_test, lev_const_dim2_test)
REAL(KIND=real64)   :: row_constants(row_const_dim1_test, row_const_dim2_test)
REAL(KIND=real64)   :: col_constants(col_const_dim1_test, col_const_dim2_test)
REAL(KIND=real64)   :: addl_params(addl_param_dim1_test, addl_param_dim2_test)
REAL(KIND=real64)   :: extra_constants(extra_const_dim_test)
REAL(KIND=real64)   :: temp_histfile(temp_hist_dim_test)
REAL(KIND=real64)   :: comp_index_1(comp_ind_1_dim_test)
REAL(KIND=real64)   :: comp_index_2(comp_ind_2_dim_test)
REAL(KIND=real64)   :: comp_index_3(comp_ind_3_dim_test)
INTEGER(KIND=int64) :: lookup(f_shum_lookup_dim1_len, n_fields_test)
REAL(KIND=real64)   :: r64_field_data(rows*columns)
INTEGER(KIND=int64) :: i64_field_data(rows*columns)
REAL(KIND=real32)   :: r32_field_data(rows*columns)
INTEGER(KIND=int32) :: i32_field_data(rows*columns)

! Variables used for re-reading the values
INTEGER(KIND=int64) :: fixed_length_header_r(f_shum_fixed_length_header_len)
INTEGER(KIND=int64), ALLOCATABLE :: integer_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: real_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: level_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: row_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: col_constants_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: addl_params_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: extra_constants_r(:)
REAL(KIND=real64),   ALLOCATABLE :: temp_histfile_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_1_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_2_r(:)
REAL(KIND=real64),   ALLOCATABLE :: comp_index_3_r(:)
INTEGER(KIND=int64), ALLOCATABLE :: lookup_r(:, :)
REAL(KIND=real64),   ALLOCATABLE :: r64_field_data_r(:)
INTEGER(KIND=int64), ALLOCATABLE :: i64_field_data_r(:)
REAL(KIND=real32),   ALLOCATABLE :: r32_field_data_r(:)
INTEGER(KIND=int32), ALLOCATABLE :: i32_field_data_r(:)

INTEGER :: i, j, k
INTEGER :: failures_at_entry
INTEGER :: failures_at_exit

LOGICAL(KIND=bool) :: check

! Get the number of failed tests prior to this test starting
CALL get_failed_count(failures_at_entry)

! Create a temporary file to use for testing 
ALLOCATE(CHARACTER(shum_tmpdir_len + LEN(tempfile)) :: scratch_filename)
scratch_filename = shum_tmpdir // '/' // tempfile

status = f_shum_create_file(scratch_filename, n_fields_test + 2, ff_id, message)

CALL assert_equals(0_int64, status,                                            & 
                                   "Failed to create new file: "//TRIM(message))

DO i = 1, f_shum_fixed_length_header_len
  fixed_length_header(i) = INT(i, KIND=int64)
END DO

! Make sure first word is valid
fixed_length_header(1) = 20_int64

status = f_shum_write_fixed_length_header(ff_id, fixed_length_header, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write fixed_length_header: "//TRIM(message))

DO i = 1, int_const_dim_test
  integer_constants(i) = INT(i + 100, KIND=int64)
END DO

status = f_shum_write_integer_constants(ff_id, integer_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write integer_constants: "//TRIM(message))

DO i = 1, real_const_dim_test
  real_constants(i) = REAL(i + 200, KIND=real64)
END DO

status = f_shum_write_real_constants(ff_id, real_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to write real_constants: "//TRIM(message))

DO i = 1, lev_const_dim1_test
  DO j = 1, lev_const_dim2_test
    level_constants(i, j) =                                                    &
                     REAL(i + (j-1)*lev_const_dim1_test + 300, KIND=real64)
  END DO
END DO

status = f_shum_write_level_dependent_constants(ff_id, level_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                   "Failed to write level_dependent_constants: "//TRIM(message))

DO i = 1, row_const_dim1_test
  DO j = 1, row_const_dim2_test
    row_constants(i, j) =                                                      &
                       REAL(i + (j-1)*row_const_dim1_test + 400, KIND=real64)
  END DO
END DO

status = f_shum_write_row_dependent_constants(ff_id, row_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                     "Failed to write row_dependent_constants: "//TRIM(message))

DO i = 1, col_const_dim1_test
  DO j = 1, col_const_dim2_test
    col_constants(i, j) =                                                      &
                       REAL(i + (j-1)*col_const_dim1_test + 500, KIND=real64)
  END DO
END DO

status = f_shum_write_column_dependent_constants(ff_id, col_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                  "Failed to write column_dependent_constants: "//TRIM(message))

status = f_shum_write_additional_parameters(ff_id, addl_params, message)

CALL assert_equals(0_int64, status,                                            & 
                       "Failed to write additional_parameters: "//TRIM(message))

DO i = 1, extra_const_dim_test
  extra_constants(i) = REAL(i + 700, KIND=real64)
END DO

status = f_shum_write_extra_constants(ff_id, extra_constants, message)

CALL assert_equals(0_int64, status,                                            & 
                             "Failed to write extra_constants: "//TRIM(message))

DO i = 1, temp_hist_dim_test
  temp_histfile(i) = REAL(i + 800, KIND=real64)
END DO

status = f_shum_write_temp_histfile(ff_id, temp_histfile, message)

CALL assert_equals(0_int64, status,                                            & 
                               "Failed to write temp_histfile: "//TRIM(message))

DO i = 1, comp_ind_1_dim_test
  comp_index_1(i) = REAL(i + 900, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_1, 1_int64, message)

CALL assert_equals(0_int64, status,                                          & 
                          "Failed to write compressed_index 1: "//TRIM(message))

DO i = 1, comp_ind_1_dim_test
  comp_index_2(i) = REAL(i + 1000, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_2, 2_int64, message)

CALL assert_equals(0_int64, status,                                          & 
                          "Failed to write compressed_index 2: "//TRIM(message))


DO i = 1, comp_ind_1_dim_test
  comp_index_3(i) = REAL(i + 1100, KIND=real64)
END DO

status = f_shum_write_compressed_index(ff_id, comp_index_3, 3_int64, message)

CALL assert_equals(0_int64, status,                                          & 
                          "Failed to write compressed_index 3: "//TRIM(message))

DO i = 1, f_shum_lookup_dim1_len
  DO j = 1, n_fields_test
    lookup(i, j) = INT(i + (j-1)*f_shum_lookup_dim1_len, KIND=int64)
  END DO
END DO

lookup(lbrel,   1:5)   = [   3,   3,   3,   3,   3 ]
lookup(lbuser1, 1:5)   = [   1,   2,   1,   1,   1 ]
lookup(lbpack,  1:5)   = [   0,   0,   2,   1,   0 ]

lookup(lbrel,   6:10)  = [   3,   3,   3,   3,   3 ]
lookup(lbuser1, 6:10)  = [   1,   2,   2,   2,   1 ]
lookup(lbpack,  6:10)  = [   2,   0,   2,   0,   0 ]

lookup(lbrel,   11:13) = [   3,   3,   3  ]
lookup(lbuser1, 11:13) = [   1,   1,   1  ]
lookup(lbpack,  11:13) = [   1,   2,   0  ]

DO k = 1, n_fields_test
  SELECT CASE (k)
    ! Real 64-bit data fields
    CASE(1,5,10,13)
      DO i = 1, rows
        DO j = 1, columns
          r64_field_data(i + (j-1)*rows) = REAL(i + (j-1)*rows, KIND=real64)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                                   ff_id, lookup(:, k), r64_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write real 64-bit data: "//TRIM(message))
    ! Integer 64-bit data fields
    CASE(2,7,9)
      DO i = 1, rows
        DO j = 1, columns
          i64_field_data(i + (j-1)*rows) = INT(i + (j-1)*rows, KIND=int64)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                                   ff_id, lookup(:, k), i64_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write integer 64-bit data: "//TRIM(message))
    ! Real 32-bit data fields
    CASE(3,6,12)
      DO i = 1, rows
        DO j = 1, columns
          r32_field_data(i + (j-1)*rows) = REAL(i + (j-1)*rows, KIND=real32)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                                   ff_id, lookup(:, k), r32_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write real 32-bit data: "//TRIM(message))
    ! Integer 32-bit data fields
    CASE(4,8,11)
      DO i = 1, rows
        DO j = 1, columns
          i32_field_data(i + (j-1)*rows) = INT(i + (j-1)*rows, KIND=int32)
        END DO
      END DO
      status = f_shum_write_field_data(                                        &
                                   ff_id, lookup(:, k), i32_field_data, message)
      CALL assert_equals(0_int64, status,                                      &
                         "Failed to write integer 32-bit data: "//TRIM(message))
  END SELECT
END DO

status = f_shum_close_file(ff_id, message)

CALL assert_equals(0_int64, status, "Failed to close file: "//TRIM(message))

status = f_shum_open_file(scratch_filename, ff_id, message)

CALL assert_equals(0_int64, status, "Failed to re-open file: "//TRIM(message))


status = f_shum_read_fixed_length_header(ff_id, fixed_length_header_r, message)

CALL assert_equals(0_int64, status,                                            & 
                         "Failed to read fixed_length_header: "//TRIM(message))

DO i = int_const_dim, f_shum_fixed_length_header_len
  fixed_length_header(i) = -32768_int64
END DO
fixed_length_header(num_prognostic_fields) = 153_int64

fixed_length_header(int_const_start) = f_shum_fixed_length_header_len + 1
fixed_length_header(int_const_dim)   = int_const_dim_test

fixed_length_header(real_const_dim)   = real_const_dim_test
fixed_length_header(real_const_start) =                                        &
  fixed_length_header(int_const_start) + fixed_length_header(int_const_dim)

fixed_length_header(lev_dep_const_dim1)  = lev_const_dim1_test
fixed_length_header(lev_dep_const_dim2)  = lev_const_dim2_test
fixed_length_header(lev_dep_const_start) =                                     &
  fixed_length_header(real_const_start) + fixed_length_header(real_const_dim)

fixed_length_header(row_dep_const_dim1)  = row_const_dim1_test
fixed_length_header(row_dep_const_dim2)  = row_const_dim2_test
fixed_length_header(row_dep_const_start) =                                     &
  fixed_length_header(lev_dep_const_start) +                                   &
    fixed_length_header(lev_dep_const_dim1) *                                  &
    fixed_length_header(lev_dep_const_dim2)

fixed_length_header(col_dep_const_dim1)  = col_const_dim1_test
fixed_length_header(col_dep_const_dim2)  = col_const_dim2_test
fixed_length_header(col_dep_const_start) =                                     &
  fixed_length_header(row_dep_const_start) +                                   &
    fixed_length_header(row_dep_const_dim1)  *                                 &
    fixed_length_header(row_dep_const_dim2)

fixed_length_header(additional_const_dim1) = addl_param_dim1_test
fixed_length_header(additional_const_dim2) = addl_param_dim2_test
fixed_length_header(additional_const_start) =                                  &
  fixed_length_header(col_dep_const_start) +                                   &
    fixed_length_header(col_dep_const_dim1)  *                                 &
    fixed_length_header(col_dep_const_dim2)

fixed_length_header(extra_const_dim) = extra_const_dim_test
fixed_length_header(extra_const_start) =                                       &
  fixed_length_header(additional_const_start) +                                &
    fixed_length_header(additional_const_dim1)  *                              &
    fixed_length_header(additional_const_dim2)

fixed_length_header(temp_histfile_dim) = temp_hist_dim_test
fixed_length_header(temp_histfile_start) =                                     &
  fixed_length_header(extra_const_start) + fixed_length_header(extra_const_dim)  

fixed_length_header(comp_field_index1_dim) = comp_ind_1_dim_test
fixed_length_header(comp_field_index1_start) =                                 &
  fixed_length_header(temp_histfile_start) +                                   &
  fixed_length_header(temp_histfile_dim)  

fixed_length_header(comp_field_index2_dim) = comp_ind_2_dim_test
fixed_length_header(comp_field_index2_start) =                                 &
  fixed_length_header(comp_field_index1_start) +                               &
  fixed_length_header(comp_field_index1_dim)  

fixed_length_header(comp_field_index3_dim) = comp_ind_3_dim_test
fixed_length_header(comp_field_index3_start) =                                 &
  fixed_length_header(comp_field_index2_start) +                               &
  fixed_length_header(comp_field_index2_dim)  

fixed_length_header(lookup_dim1) = f_shum_lookup_dim1_len
fixed_length_header(lookup_dim2) = n_fields_test
fixed_length_header(lookup_start) =                                            &
  fixed_length_header(comp_field_index3_start) +                               &
  fixed_length_header(comp_field_index3_dim)  

fixed_length_header(data_start) = 524289_int64
fixed_length_header(data_dim)   = 534528_int64

CALL assert_equals(fixed_length_header, fixed_length_header_r,                 &
  INT(f_shum_fixed_length_header_len, KIND=int32),                             &
  "Re-read fixed_length_header does not contain expected values")

status = f_shum_read_integer_constants(ff_id, integer_constants_r, message)

CALL assert_equals(0_int64, status,                                            & 
                   "Failed to read integer_constants: "//TRIM(message))

check = ALLOCATED(integer_constants_r)
CALL assert_true(check, "Re-read integer_contants not allocated by read call")

CALL assert_equals(int_const_dim_test, SIZE(integer_constants_r, KIND=int64),  &
                   "Re-read integer_constants incorrect size")

CALL assert_equals(integer_constants, integer_constants_r,                     &
                   INT(int_const_dim_test, KIND=int32),                        &
                   "Re-read integer_constants contain incorrect values")

status = f_shum_read_real_constants(ff_id, real_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read real_constants: "//TRIM(message))

check = ALLOCATED(real_constants_r)
CALL assert_true(check, "Re-read real_contants not allocated by read call")

CALL assert_equals(real_const_dim_test, SIZE(real_constants_r, KIND=int64),    &
                   "Re-read real_constants incorrect size")

CALL assert_equals(real_constants, real_constants_r,                           &
                   INT(real_const_dim_test, KIND=int32),                       &
                   "Re-read real_constants contain incorrect values")

status = f_shum_read_level_dependent_constants(                                &
                                              ff_id, level_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read level_dependent_constants: "//TRIM(message))

check = ALLOCATED(level_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read level_dependent_contants not allocated by read call")

CALL assert_equals(lev_const_dim1_test,                                        &
                   SIZE(level_constants_r, 1, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 1st dimension")

CALL assert_equals(lev_const_dim2_test,                                        &
                   SIZE(level_constants_r, 2, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 2nd dimension")

CALL assert_equals(level_constants, level_constants_r,                         &
                   INT(lev_const_dim1_test, KIND=int32),                       &
                   INT(lev_const_dim2_test, KIND=int32),                       &
                   "Re-read level_dependent_constants contain incorrect values")

status = f_shum_read_row_dependent_constants(ff_id, row_constants_r, message)
                                                
CALL assert_equals(0_int64, status,                                            &
                   "Failed to read row_dependent_constants: "//TRIM(message))

check = ALLOCATED(row_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read row_dependent_contants not allocated by read call")

CALL assert_equals(row_const_dim1_test,                                        &
                   SIZE(row_constants_r, 1, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 1st dimension")

CALL assert_equals(row_const_dim2_test,                                        &
                   SIZE(row_constants_r, 2, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 2nd dimension")

CALL assert_equals(row_constants, row_constants_r,                             &
                   INT(row_const_dim1_test, KIND=int32),                       &
                   INT(row_const_dim2_test, KIND=int32),                       &
                   "Re-read row_dependent_constants contain incorrect values")

status = f_shum_read_column_dependent_constants(ff_id, col_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read column_dependent_constants: "//TRIM(message))

check = ALLOCATED(col_constants_r)
CALL assert_true(check,                                                        &
                 "Re-read column_dependent_contants not allocated by read call")

CALL assert_equals(col_const_dim1_test,                                        &
                   SIZE(col_constants_r, 1, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 1st dimension")

CALL assert_equals(col_const_dim2_test,                                        &
                   SIZE(col_constants_r, 2, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 2nd dimension")

CALL assert_equals(col_constants, col_constants_r,                             &
                   INT(col_const_dim1_test, KIND=int32),                       &
                   INT(col_const_dim2_test, KIND=int32),                       &
                  "Re-read column_dependent_constants contain incorrect values")

status = f_shum_read_additional_parameters(ff_id, addl_params_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read additional_parameters: "//TRIM(message))

check = ALLOCATED(addl_params_r)
CALL assert_true(check,                                                        &
                 "Re-read additional_parameters not allocated by read call")

CALL assert_equals(addl_param_dim1_test,                                       &
                   SIZE(addl_params_r, 1, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 1st dimension")

CALL assert_equals(addl_param_dim2_test,                                       &
                   SIZE(addl_params_r, 2, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 2nd dimension")

CALL assert_equals(addl_params, addl_params_r,                                 &
                   INT(addl_param_dim1_test, KIND=int32),                      &
                   INT(addl_param_dim2_test, KIND=int32),                      &
                  "Re-read additional_parameters contain incorrect values")

status = f_shum_read_extra_constants(ff_id, extra_constants_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read extra_constants: "//TRIM(message))

check = ALLOCATED(extra_constants_r)
CALL assert_true(check, "Re-read extra_constants not allocated by read call")

CALL assert_equals(extra_const_dim_test, SIZE(extra_constants_r, KIND=int64),  &
                   "Re-read extra_constants incorrect size")

CALL assert_equals(extra_constants, extra_constants_r,                         &
                   INT(extra_const_dim_test, KIND=int32),                      &
                   "Re-read extra_constants contain incorrect values")

status = f_shum_read_temp_histfile(ff_id, temp_histfile_r, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read temp_histfile: "//TRIM(message))

check = ALLOCATED(temp_histfile_r)
CALL assert_true(check, "Re-read temp_histfile not allocated by read call")

CALL assert_equals(temp_hist_dim_test, SIZE(temp_histfile_r, KIND=int64),      &
                   "Re-read temp_histfile incorrect size")

CALL assert_equals(temp_histfile, temp_histfile_r,                             &
                   INT(temp_hist_dim_test, KIND=int32),                        &
                   "Re-read temp_histfile contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_1_r, 1_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 1: "//TRIM(message))

check = ALLOCATED(comp_index_1_r)
CALL assert_true(check,                                                        &
                 "Re-read compressed_index 1 not allocated by read call")

CALL assert_equals(comp_ind_1_dim_test, SIZE(comp_index_1_r, KIND=int64),      &
                   "Re-read compressed_index 1 incorrect size")

CALL assert_equals(comp_index_1, comp_index_1_r,                               &
                   INT(comp_ind_1_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 1 contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_2_r, 2_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 2: "//TRIM(message))

check = ALLOCATED(comp_index_2_r)
CALL assert_true(check,                                                        &
                 "Re-read compressed_index 2 not allocated by read call")

CALL assert_equals(comp_ind_2_dim_test, SIZE(comp_index_2_r, KIND=int64),      &
                   "Re-read compressed_index 2 incorrect size")

CALL assert_equals(comp_index_2, comp_index_2_r,                               &
                   INT(comp_ind_2_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 2 contain incorrect values")

status = f_shum_read_compressed_index(ff_id, comp_index_3_r, 3_int64, message)

CALL assert_equals(0_int64, status,                                            &
                   "Failed to read compressed_index 3: "//TRIM(message))

check = ALLOCATED(comp_index_3_r)
CALL assert_true(check,                                                        &
                 "Re-read compressed_index 3 not allocated by read call")

CALL assert_equals(comp_ind_3_dim_test, SIZE(comp_index_3_r, KIND=int64),      &
                   "Re-read compressed_index 3 incorrect size")

CALL assert_equals(comp_index_3, comp_index_3_r,                               &
                   INT(comp_ind_3_dim_test, KIND=int32),                       &
                   "Re-read compressed_index 3 contain incorrect values")

status = f_shum_read_lookup(ff_id, lookup_r, message)

CALL assert_equals(0_int64, status, "Failed to read lookup: "//TRIM(message))

check = ALLOCATED(lookup_r)
CALL assert_true(check, "Re-read lookup not allocated by read call")

CALL assert_equals(f_shum_lookup_dim1_len,                                     &
                   SIZE(lookup_r, 1, KIND=int64),                              &
                   "Re-read lookup incorrect 1st dimension")

CALL assert_equals(n_fields_test,                                              &
                   SIZE(lookup_r, 2, KIND=int64),                              &
                   "Re-read lookup incorrect 2nd dimension")

lookup(lblrec,  1:5)   = [ 600, 600, 600, 300, 600 ]
lookup(lbnrec,  1:5)   = [1024,1024, 512, 512,1024 ]

lookup(lblrec,  6:10)  = [ 600, 600, 600, 600, 600 ]
lookup(lbnrec,  6:10)  = [ 512,1024, 512,1024,1024 ]

lookup(lblrec,  11:13) = [ 300, 600, 600 ]
lookup(lbnrec,  11:13) = [ 512, 512,1024 ]

lookup(lbegin, 1) = 524288_int64
lookup(lbuser2, 1) = 1_int64
DO j = 2, n_fields_test
  lookup(lbegin, j) = lookup(lbegin, j-1) + lookup(lbnrec, j-1)
  lookup(lbuser2, j) = lookup(lbuser2, j-1) + lookup(lbnrec, j-1)
END DO

CALL assert_equals(lookup, lookup_r,                                           &
                   INT(f_shum_lookup_dim1_len, KIND=int32),                    &
                   INT(n_fields_test, KIND=int32),                             &
                  "Re-read lookup contains incorrect values")

DO k = 1, n_fields_test
  SELECT CASE (k)
    ! Real 64-bit data fields
    CASE(1,5,10,13)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), r64_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read real 64-bit data: "//TRIM(message))
      check = ALLOCATED(r64_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read real 64-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(r64_field_data_r, 1, KIND=int64),                              &
           "Re-read real 64-bit data array incorrect length")
      CALL assert_equals(r64_field_data, r64_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read real 64-bit data array contains incorrect values")
    ! Integer 64-bit data fields
    CASE(2,7,9)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), i64_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read integer 64-bit data: "//TRIM(message))
      check = ALLOCATED(i64_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read integer 64-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(i64_field_data_r, 1, KIND=int64),                              &
           "Re-read integer 64-bit data array incorrect length")
      CALL assert_equals(i64_field_data, i64_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read integer 64-bit data array contains incorrect values")
    ! Real 32-bit data fields
    CASE(3,6,12)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), r32_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read real 32-bit data: "//TRIM(message))
      check = ALLOCATED(r32_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read real 32-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(r32_field_data, 1, KIND=int64),                                &
           "Re-read real 32-bit data array incorrect length")
      CALL assert_equals(r32_field_data, r32_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read real 32-bit data array contains incorrect values")
    ! Integer 32-bit data fields
    CASE(4,8,11)
      status = f_shum_read_field_data(                                         &
                           ff_id, INT(k, KIND=int64), i32_field_data_r, message)
      CALL assert_equals(0_int64, status,                                      &
           "Failed to read integer 32-bit data: "//TRIM(message))
      check = ALLOCATED(i32_field_data_r)
      CALL assert_true(check,                                                  &
           "Re-read integer 32-bit data array not allocated by read call")
      CALL assert_equals(rows*columns,                                         &
           SIZE(i32_field_data, 1, KIND=int64),                                &
           "Re-read integer 32-bit data array incorrect length")
      CALL assert_equals(i32_field_data, i32_field_data_r,                     &
           INT(rows*columns, KIND=int32),                                      &
           "Re-read integer 32-bit data array contains incorrect values")
  END SELECT
END DO

status = f_shum_close_file(ff_id, message)

CALL assert_equals(0_int64, status, "Failed to close file: "//TRIM(message))

! Tidy up the temporary file assuming the tests completed okay
CALL get_failed_count(failures_at_exit)
IF (failures_at_exit - failures_at_entry == 0) THEN
  OPEN(scratch_test_unit, FILE=scratch_filename, STATUS="OLD")
  CLOSE(scratch_test_unit, STATUS="DELETE")
END IF

END SUBROUTINE test_end_to_end_sequential_write_file

!------------------------------------------------------------------------------!

SUBROUTINE test_stashmaster_read

USE f_shum_stashmaster_mod, ONLY: shum_STASHmaster, f_shum_read_stashmaster

IMPLICIT NONE 

INTEGER(KIND=int64)    :: status
CHARACTER(LEN=500)     :: message = ""
CHARACTER(LEN=1)       :: newline
TYPE(shum_STASHmaster) :: STASHmaster(99999)

CHARACTER(LEN=*), PARAMETER   :: tempfile="fruit_test_fieldsfile_STASHmaster"
CHARACTER(LEN=:), ALLOCATABLE :: scratch_filename

INTEGER :: failures_at_entry
INTEGER :: failures_at_exit

INTEGER(KIND=int64) :: grid
CHARACTER(LEN=36)   :: name
INTEGER(KIND=int64) :: packing_codes(10)
LOGICAL(KIND=bool)  :: check

! Get the number of failed tests prior to this test starting
CALL get_failed_count(failures_at_entry)

! Create a temporary file to use for testing 
ALLOCATE(CHARACTER(shum_tmpdir_len + LEN(tempfile)) :: scratch_filename)
scratch_filename = shum_tmpdir // '/' // tempfile

OPEN(scratch_test_unit, FILE=scratch_filename, STATUS="REPLACE",               &
  IOSTAT=status, IOMSG=message)

CALL assert_equals(0_int64, status,                                            & 
                                "Failed to create STASHmaster: "//TRIM(message))

! Write a simple STASHmaster file
newline = NEW_LINE("X")
WRITE(scratch_test_unit, "(A)", IOSTAT=status, IOMSG=message)                  &
"H1| SUBMODEL_NUMBER=1"                                                            //newline//&
"H2| SUBMODEL_NAME=ATMOS"                                                          //newline//&
"H3| UM_VERSION=99.9"                                                              //newline//&
"#"                                                                                //newline//&
"#|Model |Sectn | Item |Name                                |"                     //newline//&
"#|Space |Point | Time | Grid |LevelT|LevelF|LevelL|PseudT|PseudF|PseudL|LevCom|"  //newline//&
"#| Option Codes                   | Version Mask         | Halo |"                //newline//&
"#|DataT |DumpP | PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9  PCA |"              //newline//&
"#|Rotate| PPF  | USER | LBVC | BLEV | TLEV |RBLEVV| CFLL | CFFF |"                //newline//&
"#"                                                                                //newline//&
"#===============================================================================" //newline//&
"#"                                                                                //newline//&
"1|    1 |    0 |    2 |Test STASH record name 1            |"                     //newline//&
"2|    2 |    0 |    1 |   18 |    1 |    1 |    2 |    0 |    0 |    0 |    0 |"  //newline//&
"3| 000000000000010000000000000000 | 00000000000000000001 |    1 |"                //newline//&
"4|    1 |    2 |  -3   -3   -3   -3  -12   20   -3  -99  -99  -99 |"              //newline//&
"5|    0 |   56 |    0 |   65 |    0 |    0 |    0 |    0 |    5 |"                //newline//&
"#"                                                                                //newline//&
"1|    1 |   16 |    4 |Test STASH record name 2            |"                     //newline//&
"2|    0 |    0 |    1 |    1 |    2 |    1 |    2 |    0 |    0 |    0 |    1 |"  //newline//&
"3| 000000000000000000000000000000 | 00000000000000000001 |    3 |"                //newline//&
"4|    1 |    2 |  -3  -10   -3   -3  -14   21   -3  -99  -99  -99 |"              //newline//&
"5|    0 |   16 |    0 |   65 |    0 |    0 |    0 |    0 |    3 |"                //newline//&
"#"                                                                                //newline//&
"#===============================================================================" //newline//&
"#"                                                                                //newline//&
"1|   -1 |   -1 |   -1 |END OF FILE MARK                    |"                     //newline//&
"2|    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |"  //newline//&
"3| 000000000000000000000000000000 | 00000000000000000000 |    0 |"                //newline//&
"4|    0 |    0 | -99  -99  -99  -99  -30  -99  -99  -99  -99  -99 |"              //newline//&
"5|    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |    0 |"                //newline//&
"#"                                                                                //newline

CALL assert_equals(0_int64, status,                                            & 
                                 "Failed to write STASHmaster: "//TRIM(message))

CLOSE(scratch_test_unit, IOSTAT=status, IOMSG=message)
CALL assert_equals(0_int64, status,                                            & 
                                 "Failed to close STASHmaster: "//TRIM(message))

! Now try to read that file back in using the API
status = f_shum_read_stashmaster(scratch_filename, STASHmaster, message)

CALL assert_equals(0_int64, status,                                            & 
                                  "Failed to read STASHmaster: "//TRIM(message))
! Check that the stash codes we expect to be there are there
check = ASSOCIATED(STASHmaster(2) % record)
CALL assert_true(check, "STASH record for 00002 does not exist")
check = ASSOCIATED(STASHmaster(16004) % record)
CALL assert_true(check, "STASH record for 00002 does not exist")

! Check that an arbitrary record does not exist
check = ASSOCIATED(STASHmaster(33) % record)
CALL assert_false(check, "STASH record for 00033 exists, but shouldn't")

! Check some of the values from the valid records
grid = STASHmaster(2) % record % grid
CALL assert_equals(18_int64, grid, "STASH record 00002 incorrect grid code")
grid = STASHmaster(16004) % record % grid
CALL assert_equals(1_int64, grid, "STASH record 16004 incorrect grid code")

name = STASHmaster(2) % record % name
CALL assert_equals("Test STASH record name 1            ", name,               &
                   "STASH record 00002 incorrect name")
name = STASHmaster(16004) % record % name
CALL assert_equals("Test STASH record name 2            ", name,               &
                   "STASH record 16004 incorrect name")

packing_codes = STASHmaster(2) % record % packing_codes
CALL assert_equals([-3_int64, -3_int64, -3_int64, -3_int64,-12_int64,          &
                    20_int64, -3_int64,-99_int64,-99_int64,-99_int64],         &
                    packing_codes, 10,                                         &
                    "STASH record 00002 incorrect packing codes")
packing_codes = STASHmaster(16004) % record % packing_codes
CALL assert_equals([-3_int64,-10_int64, -3_int64, -3_int64,-14_int64,          &
                    21_int64, -3_int64,-99_int64,-99_int64,-99_int64],         &
                    packing_codes, 10,                                         &
                    "STASH record 16004 incorrect packing codes")

! Tidy up the temporary file assuming the tests completed okay
CALL get_failed_count(failures_at_exit)
IF (failures_at_exit - failures_at_entry == 0) THEN
  OPEN(scratch_test_unit, FILE=scratch_filename, STATUS="OLD")
  CLOSE(scratch_test_unit, STATUS="DELETE")
END IF

END SUBROUTINE test_stashmaster_read

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_fieldsfile_mod
