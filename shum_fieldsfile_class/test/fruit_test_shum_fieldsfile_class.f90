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
MODULE fruit_test_shum_fieldsfile_class_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_INT, C_BOOL

USE f_shum_ff_status_mod, ONLY: shum_ff_status_type
IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_fieldsfile_class

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
  INTEGER, PARAMETER :: bool   = C_BOOL
!------------------------------------------------------------------------------!

! The unit, filename, and id of the temporary file used by these tests
INTEGER(KIND=int64), PARAMETER :: scratch_test_unit = 20
INTEGER                        :: shum_tmpdir_len
CHARACTER(LEN=:), ALLOCATABLE  :: shum_tmpdir

CONTAINS

SUBROUTINE fruit_test_shum_fieldsfile_class

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
    "Testing shum_fieldsfile_class at Shumlib version: ", version

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
  test_write_and_read_file, "write_and_read_file")

END SUBROUTINE fruit_test_shum_fieldsfile_class

!------------------------------------------------------------------------------!

SUBROUTINE test_write_and_read_file

USE f_shum_field_mod, ONLY: shum_field_type
USE f_shum_file_mod, ONLY: shum_file_type

USE f_shum_fieldsfile_mod, ONLY: f_shum_fixed_length_header_len,               &
  f_shum_lookup_dim1_len
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

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lbuser1, lbpack, lbrel, lbrow, lbnpt, lbhr, lbhrd, lbmin, bdx, bdy, bzx,     &
  bzy, bmdi

IMPLICIT NONE

TYPE(shum_ff_status_type) :: status

TYPE(shum_file_type) :: testfile, readfile
TYPE(shum_field_type) :: testfield, readfield

CHARACTER(LEN=*), PARAMETER   :: tempfile="fruit_test_fieldsfile_class.ff"
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
INTEGER(KIND=int64), PARAMETER :: rows = 3
INTEGER(KIND=int64), PARAMETER :: columns = 3

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
INTEGER(KIND=int64) :: lookup_int(45)
REAL(KIND=real64)   :: lookup_real(19)
REAL(KIND=real64)   :: rdata(columns, rows)

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
INTEGER(KIND=int64) :: lookup_int_r(45)
REAL(KIND=real64)   :: lookup_real_r(19)
REAL(KIND=real64)   :: rdata_r(columns, rows)
LOGICAL(KIND=bool)  :: overwrite

INTEGER(KIND=int64) :: i, j
INTEGER :: failures_at_entry
INTEGER :: failures_at_exit

! Get the number of failed tests prior to this test starting
CALL get_failed_count(failures_at_entry)

! Create a temporary file to use for testing
ALLOCATE(CHARACTER(shum_tmpdir_len + LEN(tempfile)) :: scratch_filename)
scratch_filename = shum_tmpdir // '/' // tempfile


overwrite = .TRUE.
status = testfile%open_file(scratch_filename, num_lookup=1_int64,              &
                            overwrite=overwrite)

CALL assert_equals(0_int64, status%icode,                                      &
                       "Failed to create new file: "//TRIM(status%message))

DO i = 1, f_shum_fixed_length_header_len
  fixed_length_header(i) = INT(i, KIND=int64)
END DO

! Set Endgame grid
fixed_length_header(9) = 6

status = testfile%set_fixed_length_header(fixed_length_header)

CALL assert_equals(0_int64, status%icode,                                      &
              "Failed to set fixed_length_header: "//TRIM(status%message))

DO i = 1, int_const_dim_test
  integer_constants(i) = INT(i + 100, KIND=int64)
END DO

status = testfile%set_integer_constants(integer_constants)

CALL assert_equals(0_int64, status%icode,                                      &
                 "Failed to set integer_constants: "//TRIM(status%message))

DO i = 1, real_const_dim_test
  real_constants(i) = REAL(i + 200, KIND=real64)
END DO

status = testfile%set_real_constants(real_constants)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to set real_constants: "//TRIM(status%message))

DO i = 1, lev_const_dim1_test
  DO j = 1, lev_const_dim2_test
    level_constants(i, j) =                                                    &
                     REAL(i + (j-1)*lev_const_dim1_test + 300, KIND=real64)
  END DO
END DO

status = testfile%set_level_dependent_constants(level_constants)

CALL assert_equals(0_int64, status%icode,                                      &
        "Failed to set level_dependent_constants: "//TRIM(status%message))

DO i = 1, row_const_dim1_test
  DO j = 1, row_const_dim2_test
    row_constants(i, j) =                                                      &
                       REAL(i + (j-1)*row_const_dim1_test + 400, KIND=real64)
  END DO
END DO

status = testfile%set_row_dependent_constants(row_constants)

CALL assert_equals(0_int64, status%icode,                                      &
           "Failed to set row_dependent_constants: "//TRIM(status%message))

DO i = 1, col_const_dim1_test
  DO j = 1, col_const_dim2_test
    col_constants(i, j) =                                                      &
                       REAL(i + (j-1)*col_const_dim1_test + 500, KIND=real64)
  END DO
END DO

status = testfile%set_column_dependent_constants(col_constants)

CALL assert_equals(0_int64, status%icode,                                      &
        "Failed to set column_dependent_constants: "//TRIM(status%message))

DO i = 1, addl_param_dim1_test
  DO j = 1, addl_param_dim2_test
    addl_params(i, j) =                                                        &
                       REAL(i + (j-1)*addl_param_dim1_test + 600, KIND=real64)
  END DO
END DO

status = testfile%set_additional_parameters(addl_params)

CALL assert_equals(0_int64, status%icode,                                      &
             "Failed to set additional_parameters: "//TRIM(status%message))

DO i = 1, extra_const_dim_test
  extra_constants(i) = REAL(i + 700, KIND=real64)
END DO

status = testfile%set_extra_constants(extra_constants)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to set extra_constants: "//TRIM(status%message))

DO i = 1, temp_hist_dim_test
  temp_histfile(i) = REAL(i + 800, KIND=real64)
END DO

status = testfile%set_temp_histfile(temp_histfile)

CALL assert_equals(0_int64, status%icode,                                      &
                     "Failed to set temp_histfile: "//TRIM(status%message))

DO i = 1, comp_ind_1_dim_test
  comp_index_1(i) = REAL(i + 900, KIND=real64)
END DO

status = testfile%set_compressed_index(1_int64, comp_index_1)

CALL assert_equals(0_int64, status%icode,                                      &
                 "Failed to set compressed_index 1: "//TRIM(status%message))

DO i = 1, comp_ind_1_dim_test
  comp_index_2(i) = REAL(i + 1000, KIND=real64)
END DO

status = testfile%set_compressed_index(2_int64, comp_index_2)

CALL assert_equals(0_int64, status%icode,                                      &
                "Failed to set compressed_index 2: "//TRIM(status%message))


DO i = 1, comp_ind_1_dim_test
  comp_index_3(i) = REAL(i + 1100, KIND=real64)
END DO

status = testfile%set_compressed_index( 3_int64, comp_index_3)

CALL assert_equals(0_int64, status%icode,                                      &
                "Failed to set compressed_index 3: "//TRIM(status%message))

! Need to set these to valid header values

DO i = 1, 12
  lookup_int(i) = INT(0.0, KIND=int64)
END DO
DO i = 13, 45
  lookup_int(i) = INT(i, KIND=int64)
END DO
DO i = 1, 19
  lookup_real(i) = REAL(i, KIND=real64)
END DO

! Set forecast and data times
lookup_int(lbhr) = 21
lookup_int(lbmin) = 30
lookup_int(lbhrd) = 20

! Array dimensions
lookup_int(lbrow) = rows
lookup_int(lbnpt) = columns

! Packing and data types
lookup_int(lbpack) = 0
lookup_int(lbuser1) = 1
lookup_int(lbrel) = 3

! Grid dimensions
lookup_real(bdx - 45) = 0.1
lookup_real(bdy - 45) = 0.1
lookup_real(bzx - 45) = 119.0
lookup_real(bzy - 45) = -72.0

! Missing data
lookup_real(bmdi - 45) = -32768.0_real64*32768.0_real64


status = testfield%set_lookup(lookup_int, lookup_real)

CALL assert_equals(0_int64, status%icode, "Failed to set lookup table")


DO j = 1, rows
  DO i = 1, columns
    rdata(i,j) = REAL(i, real64)+REAL((j-1)*columns, real64) * 11.0_real64 /   &
                 10.0_real64
  END DO
END DO

status = testfield%set_data(rdata)
CALL assert_equals(0_int64, status%icode, "Failed to set data payload")

status = testfile%add_field(testfield)
CALL assert_equals(0_int64, status%icode,                                      &
                     "Failed to add field to file: "//TRIM(status%message))

status = testfile%write_header()
CALL assert_equals(0_int64, status%icode,                                      &
                  "Failed to write header to disk: "//TRIM(status%message))

status = testfile%write_field(1_int64)
CALL assert_equals(0_int64, status%icode,                                      &
                  "Failed to write field to disk: "//TRIM(status%message))


status = testfile%close_file()
CALL assert_equals(0_int64, status%icode,                                      &
                  "Failed to close file: "//TRIM(status%message))

! We've now completely written the file to disk
status = readfile%open_file(scratch_filename)
CALL assert_equals(0_int64, status%icode,                                      &
                  "Failed to re-open file: "//TRIM(status%message))


status = readfile%read_header()
CALL assert_equals(0_int64, status%icode,                                      &
                  "Failed to read header from file: "//TRIM(status%message))

status = readfile%get_fixed_length_header(fixed_length_header_r)
CALL assert_equals(0_int64, status%icode,                                      &
                         "Failed to read fixed_length_header: "                &
                         //TRIM(status%message))

DO i = int_const_dim, f_shum_fixed_length_header_len
  fixed_length_header(i) = -32768_int64
END DO
fixed_length_header(9) = 6
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
fixed_length_header(lookup_dim2) = 1
fixed_length_header(lookup_start) =                                            &
  fixed_length_header(comp_field_index3_start) +                               &
  fixed_length_header(comp_field_index3_dim)

fixed_length_header(data_start) = 524289_int64
fixed_length_header(data_dim)   = 524800_int64

fixed_length_header(num_prognostic_fields) = 153_int64

CALL assert_equals(fixed_length_header, fixed_length_header_r,                 &
  f_shum_fixed_length_header_len,                                              &
  "Re-read fixed_length_header does not contain expected values")



status = readfile%get_integer_constants(integer_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read integer_constants: "//TRIM(status%message))

CALL assert_equals(int_const_dim_test, SIZE(integer_constants_r, KIND=int64),  &
                   "Re-read integer_constants incorrect size")

CALL assert_equals(integer_constants, integer_constants_r,                     &
                   int_const_dim_test,                                         &
                   "Re-read integer_constants contain incorrect values")

status = readfile%get_real_constants(real_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read real_constants: "//TRIM(status%message))

CALL assert_equals(real_const_dim_test, SIZE(real_constants_r, KIND=int64),    &
                   "Re-read real_constants incorrect size")

CALL assert_equals(real_constants, real_constants_r,                           &
                   real_const_dim_test,                                        &
                   "Re-read real_constants contain incorrect values")

status = readfile%get_level_dependent_constants(level_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read level_dependent_constants: "//              &
                   TRIM(status%message))

CALL assert_equals(lev_const_dim1_test,                                        &
                   SIZE(level_constants_r, 1, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 1st dimension")

CALL assert_equals(lev_const_dim2_test,                                        &
                   SIZE(level_constants_r, 2, KIND=int64),                     &
                   "Re-read level_dependent_constants incorrect 2nd dimension")

CALL assert_equals(level_constants, level_constants_r,                         &
                   lev_const_dim1_test,                                        &
                   lev_const_dim2_test,                                        &
                   "Re-read level_dependent_constants contain incorrect values")

status = readfile%get_row_dependent_constants(row_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read row_dependent_constants: "//                &
                   TRIM(status%message))

CALL assert_equals(row_const_dim1_test,                                        &
                   SIZE(row_constants_r, 1, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 1st dimension")

CALL assert_equals(row_const_dim2_test,                                        &
                   SIZE(row_constants_r, 2, KIND=int64),                       &
                   "Re-read row_dependent_constants incorrect 2nd dimension")

CALL assert_equals(row_constants, row_constants_r,                             &
                   row_const_dim1_test,                                        &
                   row_const_dim2_test,                                        &
                   "Re-read row_dependent_constants contain incorrect values")

status = readfile%get_column_dependent_constants(col_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read column_dependent_constants: "               &
                   //TRIM(status%message))

CALL assert_equals(col_const_dim1_test,                                        &
                   SIZE(col_constants_r, 1, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 1st dimension")

CALL assert_equals(col_const_dim2_test,                                        &
                   SIZE(col_constants_r, 2, KIND=int64),                       &
                   "Re-read column_dependent_constants incorrect 2nd dimension")

CALL assert_equals(col_constants, col_constants_r,                             &
                   col_const_dim1_test,                                        &
                   col_const_dim2_test,                                        &
                  "Re-read column_dependent_constants contain incorrect values")

status = readfile%get_additional_parameters(addl_params_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read additional_parameters: "//                  &
                   TRIM(status%message))

CALL assert_equals(addl_param_dim1_test,                                       &
                   SIZE(addl_params_r, 1, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 1st dimension")

CALL assert_equals(addl_param_dim2_test,                                       &
                   SIZE(addl_params_r, 2, KIND=int64),                         &
                   "Re-read additional_parameters incorrect 2nd dimension")

CALL assert_equals(addl_params, addl_params_r,                                 &
                   addl_param_dim1_test,                                       &
                   addl_param_dim2_test,                                       &
                  "Re-read additional_parameters contain incorrect values")

status = readfile%get_extra_constants(extra_constants_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read extra_constants: "//TRIM(status%message))

CALL assert_equals(extra_const_dim_test, SIZE(extra_constants_r, KIND=int64),  &
                   "Re-read extra_constants incorrect size")

CALL assert_equals(extra_constants, extra_constants_r,                         &
                   extra_const_dim_test,                                       &
                   "Re-read extra_constants contain incorrect values")

status = readfile%get_temp_histfile(temp_histfile_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read temp_histfile: "//TRIM(status%message))

CALL assert_equals(temp_hist_dim_test, SIZE(temp_histfile_r, KIND=int64),      &
                   "Re-read temp_histfile incorrect size")

CALL assert_equals(temp_histfile, temp_histfile_r,                             &
                   temp_hist_dim_test,                                         &
                   "Re-read temp_histfile contain incorrect values")

status = readfile%get_compressed_index(1_int64, comp_index_1_r)

CALL assert_equals(0_int64, status%icode,                                      &
                 "Failed to read compressed_index 1: "//TRIM(status%message))

CALL assert_equals(comp_ind_1_dim_test, SIZE(comp_index_1_r, KIND=int64),      &
                   "Re-read compressed_index 1 incorrect size")

CALL assert_equals(comp_index_1, comp_index_1_r,                               &
                   comp_ind_1_dim_test,                                        &
                   "Re-read compressed_index 1 contain incorrect values")

status = readfile%get_compressed_index(2_int64, comp_index_2_r)

CALL assert_equals(0_int64, status%icode,                                      &
                 "Failed to read compressed_index 2: "//TRIM(status%message))

CALL assert_equals(comp_ind_2_dim_test, SIZE(comp_index_2_r, KIND=int64),      &
                   "Re-read compressed_index 2 incorrect size")

CALL assert_equals(TRANSFER(comp_index_2,[0_int64]),                           &
                   TRANSFER(comp_index_2_r,[0_int64]),                         &
                   comp_ind_2_dim_test,                                        &
                   "Re-read compressed_index 2 contain incorrect values")

status = readfile%get_compressed_index(3_int64, comp_index_3_r)

CALL assert_equals(0_int64, status%icode,                                      &
                 "Failed to read compressed_index 3: "//TRIM(status%message))

CALL assert_equals(comp_ind_3_dim_test, SIZE(comp_index_3_r, KIND=int64),      &
                   "Re-read compressed_index 3 incorrect size")

CALL assert_equals(TRANSFER(comp_index_3,[0_int64]),                           &
                   TRANSFER(comp_index_3_r,[0_int64]),                         &
                   comp_ind_3_dim_test,                                        &
                   "Re-read compressed_index 3 contain incorrect values")


status = readfile%read_field(1_int64)
CALL assert_equals(0_int64, status%icode, "Failed to read field: "//           &
                                     TRIM(status%message))

status = readfile%get_field(1_int64, readfield)
CALL assert_equals(0_int64, status%icode, "Failed to retrieve field: "//       &
                                     TRIM(status%message))


status = readfield%get_lookup(lookup_int_r, lookup_real_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read lookup")

! Correct data locations
lookup_int(15) = 9
lookup_int(29) = 524288_int64
lookup_int(30) = 512_int64
lookup_int(40) = 1_int64

CALL assert_equals(lookup_int, lookup_int_r,                                   &
                   45_int32,                                                   &
                   "Re-read integer lookup contain incorrect values")

CALL assert_equals(lookup_real, lookup_real_r,                                 &
                   19_int32,                                                   &
                   "Re-read real lookup contain incorrect values")

status = readfield%get_data(rdata_r)

CALL assert_equals(0_int64, status%icode,                                      &
                   "Failed to read data")
CALL assert_equals(rdata, rdata_r,                                             &
                   columns,                                                    &
                   rows,                                                       &
                   "Re-read data contain incorrect values")

status = readfile%close_file()

CALL assert_equals(0_int64, status%icode, "Failed to close file: "//           &
                                     TRIM(status%message))


! Tidy up the temporary file assuming the tests completed okay
CALL get_failed_count(failures_at_exit)
IF (failures_at_exit - failures_at_entry == 0) THEN
  OPEN(scratch_test_unit, FILE=scratch_filename, STATUS="OLD")
  CLOSE(scratch_test_unit, STATUS="DELETE")
END IF

END SUBROUTINE test_write_and_read_file

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_fieldsfile_class_mod
