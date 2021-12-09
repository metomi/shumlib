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
!*******************************************************************************
MODULE f_shum_file_mod

USE f_shum_field_mod, ONLY: shum_field_type
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL
USE f_shum_fieldsfile_mod, ONLY: f_shum_fixed_length_header_len
USE f_shum_ff_status_mod, ONLY: shum_ff_status_type, shumlib_success

IMPLICIT NONE

PRIVATE

! Data types
INTEGER, PARAMETER :: INT64  = C_INT64_T
INTEGER, PARAMETER :: INT32  = C_INT32_T
INTEGER, PARAMETER :: REAL64 = C_DOUBLE
INTEGER, PARAMETER :: REAL32 = C_FLOAT
INTEGER, PARAMETER :: bool   = C_BOOL

! Missing data, real and integer
REAL(KIND=REAL64),   PARAMETER  :: um_rmdi     = -32768.0*32768.0
INTEGER(KIND=INT64), PARAMETER  :: um_imdi     = -32768

! Lookup lengths
INTEGER(KIND=INT64), PARAMETER :: len_integer_lookup = 45
INTEGER(KIND=INT64), PARAMETER :: len_real_lookup = 19

! Land/sea mask STASH code
INTEGER(KIND=INT64), PARAMETER :: stash_land_sea_mask = 30
!-------------------------------------------------------------------------------

TYPE, PUBLIC :: shum_file_type
  PRIVATE
  CHARACTER(LEN=:), ALLOCATABLE            :: filename
  LOGICAL                                  :: fixed_length_header_read=.FALSE.
  INTEGER(KIND=INT64)                      :: file_identifier = um_imdi
  INTEGER(KIND=INT64)                      :: fixed_length_header(             &
                           f_shum_fixed_length_header_len) = um_imdi
  INTEGER(KIND=INT64), ALLOCATABLE         :: integer_constants(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: real_constants(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: level_dependent_constants(:,:)
  REAL(KIND=REAL64), ALLOCATABLE           :: row_dependent_constants(:,:)
  REAL(KIND=REAL64), ALLOCATABLE           :: column_dependent_constants(:,:)
  REAL(KIND=REAL64), ALLOCATABLE           :: additional_parameters(:,:)
  REAL(KIND=REAL64), ALLOCATABLE           :: extra_constants(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: temp_histfile(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: compressed_index_1(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: compressed_index_2(:)
  REAL(KIND=REAL64), ALLOCATABLE           :: compressed_index_3(:)

  TYPE(shum_field_type), PUBLIC, ALLOCATABLE :: fields(:)

  ! Land/sea mask location to deal with land-packed fields
  INTEGER(KIND=INT64) :: field_number_land_sea_mask = -99

  INTEGER(KIND=INT64), PUBLIC              :: num_fields = 0
  CONTAINS
  ! File I/O operations
  PROCEDURE :: open_file
  PROCEDURE :: read_header
  PROCEDURE :: read_field
  PROCEDURE :: write_header
  PROCEDURE :: write_field
  PROCEDURE :: close_file
  ! Header accessors
  PROCEDURE :: set_fixed_length_header
  PROCEDURE :: get_fixed_length_header
  PROCEDURE :: set_fixed_length_header_by_index
  PROCEDURE :: get_fixed_length_header_by_index
  PROCEDURE :: set_integer_constants
  PROCEDURE :: get_integer_constants
  PROCEDURE :: set_integer_constants_by_index
  PROCEDURE :: get_integer_constants_by_index
  PROCEDURE :: set_real_constants
  PROCEDURE :: get_real_constants
  PROCEDURE :: set_real_constants_by_index
  PROCEDURE :: get_real_constants_by_index
  PROCEDURE :: set_level_dependent_constants
  PROCEDURE :: get_level_dependent_constants
  PROCEDURE :: set_row_dependent_constants
  PROCEDURE :: get_row_dependent_constants
  PROCEDURE :: set_column_dependent_constants
  PROCEDURE :: get_column_dependent_constants
  PROCEDURE :: set_additional_parameters
  PROCEDURE :: get_additional_parameters
  PROCEDURE :: set_extra_constants
  PROCEDURE :: get_extra_constants
  PROCEDURE :: set_temp_histfile
  PROCEDURE :: get_temp_histfile
  PROCEDURE :: set_compressed_index
  PROCEDURE :: get_compressed_index
  ! Field accessors/methods
  PROCEDURE :: get_field
  PROCEDURE :: find_field_indices_in_file
  PROCEDURE :: find_fields_in_file
  PROCEDURE :: find_forecast_time
  ! File manipulation
  PROCEDURE :: set_filename
  PROCEDURE :: get_filename
  PROCEDURE :: copy_headers_from_file_object
  ! Field manipulation
  PROCEDURE :: add_field
  PROCEDURE :: unload_field
END TYPE shum_file_type

!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
! File I/O operations
!-------------------------------------------------------------------------------
FUNCTION open_file(self, fname, num_lookup, overwrite) RESULT(STATUS)
USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_open_file,                                                            &
  f_shum_create_file
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)   :: self
CHARACTER(LEN=*), INTENT(IN) :: fname
INTEGER(KIND=INT64), OPTIONAL :: num_lookup
LOGICAL(KIND=bool), OPTIONAL :: overwrite
TYPE(shum_ff_status_type) :: STATUS    ! Return status object
INTEGER(KIND=INT64) :: lookup_size
LOGICAL :: exists, read_only

IF (ALLOCATED(self%filename)) DEALLOCATE(self%filename)
! The following line auto-allocates the CHARACTER array
self%filename = fname
INQUIRE(FILE=fname, EXIST=exists)

! Assume file is opened as read only unless overwrite is specified and true
read_only = .TRUE.
IF (PRESENT(overwrite)) THEN
  IF (overwrite) THEN
    read_only = .FALSE.
  END IF
END IF
! If read only and file does not exist, return with error code
IF ((read_only) .AND. (.NOT. exists)) THEN
  STATUS%icode = 1_int64
  STATUS%message = "Missing input file ("//TRIM(fname)//")"
  RETURN
END IF

IF (read_only) THEN
  ! Open the file using SHUMlib
  STATUS%icode = f_shum_open_file(self%filename,                               &
                            self%file_identifier,                              &
                            STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
  STATUS%message = 'Loaded existing file: ' // TRIM(fname)
ELSE
  IF (PRESENT(num_lookup)) THEN
    lookup_size = num_lookup
  ELSE
    lookup_size = 4096_int64
  END IF
  ! Create a new file using SHUMlib; default behaviour is to overwrite
  ! existing files
  STATUS%icode = f_shum_create_file(self%filename,                             &
                             lookup_size,                                      &
                             self%file_identifier,                             &
                             STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
  STATUS%message = 'Created new file: ' // TRIM(fname)
END IF

END FUNCTION open_file

!-------------------------------------------------------------------------------

FUNCTION read_header(self) RESULT(STATUS)
USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_read_fixed_length_header,                                             &
  f_shum_read_integer_constants,                                               &
  f_shum_read_real_constants,                                                  &
  f_shum_read_level_dependent_constants,                                       &
  f_shum_read_row_dependent_constants,                                         &
  f_shum_read_column_dependent_constants,                                      &
  f_shum_read_additional_parameters,                                           &
  f_shum_read_extra_constants,                                                 &
  f_shum_read_temp_histfile,                                                   &
  f_shum_read_compressed_index,                                                &
  f_shum_read_lookup

USE f_shum_lookup_indices_mod, ONLY:                                           &
    lbrow, lbnpt, lbuser4

USE f_shum_fixed_length_header_indices_mod, ONLY:                              &
    dataset_type, lookup_dim2, grid_staggering, vert_coord_type
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)   :: self

INTEGER(KIND=INT64), ALLOCATABLE :: temp_lookup(:,:)
! Loop iterators
INTEGER(KIND=INT64)              :: i_field
INTEGER(KIND=INT64)              :: p_lookup

REAL(KIND=REAL64) :: temp_lookup_real(len_real_lookup)

INTEGER(KIND=INT64), PARAMETER :: grid_stagger_endgame = 6
INTEGER(KIND=INT64), PARAMETER :: grid_stagger_arakawa_a = 1
INTEGER(KIND=INT64), PARAMETER :: pressure_vert_coord=3

INTEGER(KIND=INT64), PARAMETER :: dump_type = 1
INTEGER(KIND=INT64), PARAMETER :: fieldsfile_type = 3
INTEGER(KIND=INT64), PARAMETER :: ancil_type = 4

LOGICAL :: is_variable_resolution = .FALSE.
LOGICAL :: grid_supported

TYPE(shum_ff_status_type) :: STATUS    ! Return status object

! Read in compulsory headers
STATUS%icode = f_shum_read_fixed_length_header(                                &
                                         self%file_identifier,                 &
                                         self%fixed_length_header,             &
                                         STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF
self%fixed_length_header_read = .TRUE.

! These conditions need to be met for supported files
IF (self%fixed_length_header(grid_staggering) == grid_stagger_endgame) THEN
  ! an endgame file
  grid_supported=.TRUE.

ELSE IF (self%fixed_length_header(grid_staggering) == grid_stagger_arakawa_a .AND.&
         self%fixed_length_header(vert_coord_type) == pressure_vert_coord ) THEN
  ! A background error file
  grid_supported=.TRUE.
ELSE
  grid_supported=.FALSE.
END IF
IF (.NOT. grid_supported) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'This software only supports the Endgame and background error files.'
  RETURN
END IF

! Check this is a instantaneous dump, fieldsfile or ancil - if it's an obs
! mean dump or LBC file, abort
IF (self%fixed_length_header(dataset_type) /= dump_type .AND.                  &
    self%fixed_length_header(dataset_type) /= fieldsfile_type .AND.            &
    self%fixed_length_header(dataset_type) /= ancil_type) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'File is not a fieldsfile, instantaneous dump or ancil'
  RETURN
END IF

STATUS%icode = f_shum_read_integer_constants(                                  &
                                        self%file_identifier,                  &
                                        self%integer_constants,                &
                                        STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_read_real_constants(                                     &
                                        self%file_identifier,                  &
                                        self%real_constants,                   &
                                        STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

! Ancillary files do not include level dependent constants so they are not read here
! See section 3.4 from UMDP F03
IF (self%fixed_length_header(dataset_type) /= ancil_type ) THEN
  STATUS%icode = f_shum_read_level_dependent_constants(                        &
                                     self%file_identifier,                     &
                                     self%level_dependent_constants,           &
                                     STATUS%message)
  IF (STATUS%icode /= shumlib_success) RETURN
END IF

! Read in the optional headers we care about (for variable resolution)
! Note these return negative status if the component isn't found, so this
! is technically a "success" criteria; a positive status is a genuine error.
STATUS%icode = f_shum_read_row_dependent_constants(                            &
                               self%file_identifier,                           &
                               self%row_dependent_constants,                   &
                               STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
ELSE IF (STATUS%icode == shumlib_success) THEN
  is_variable_resolution = .TRUE.
ELSE
  ! No constants found, therefore not variable resolution
  is_variable_resolution = .FALSE.
END IF

STATUS%icode = f_shum_read_column_dependent_constants(                         &
                               self%file_identifier,                           &
                               self%column_dependent_constants,                &
                               STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
ELSE IF (STATUS%icode < 0_int64 .AND. is_variable_resolution) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'File has row-dependent constants but not ' //              &
                 'column-dependent constants'
  RETURN
ELSE IF (STATUS%icode == shumlib_success .AND. .NOT.                           &
         is_variable_resolution) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'File has column-dependent constants but not ' //           &
                 'row-dependent constants'
  RETURN
END IF

STATUS%icode = f_shum_read_additional_parameters(self%file_identifier,         &
                                          self%additional_parameters,          &
                                          STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_read_extra_constants(self%file_identifier,               &
                                    self%extra_constants,                      &
                                    STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF


STATUS%icode = f_shum_read_temp_histfile(self%file_identifier,                 &
                                  self%temp_histfile,                          &
                                  STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_read_compressed_index(self%file_identifier,              &
                                     self%compressed_index_1,                  &
                                     1_int64,                                  &
                                     STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF


STATUS%icode = f_shum_read_compressed_index(self%file_identifier,              &
                                     self%compressed_index_2,                  &
                                     2_int64,                                  &
                                     STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_read_compressed_index(self%file_identifier,              &
                                     self%compressed_index_3,                  &
                                     3_int64,                                  &
                                     STATUS%message)
IF (STATUS%icode > shumlib_success) THEN
  RETURN
END IF

! Allocate space for both temporary lookups and final fields
IF ( ALLOCATED(self%fields) ) DEALLOCATE(self%fields)
ALLOCATE(self%fields(self%fixed_length_header(lookup_dim2)))

! Read the lookup
STATUS%icode = f_shum_read_lookup(self%file_identifier,                        &
                            temp_lookup,                                       &
                            STATUS%message)

IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF


! Populate the lookup in the field objects
DO i_field = 1, self%fixed_length_header(lookup_dim2)
  ! Ignore empty fields in the lookup
  IF (temp_lookup(1, i_field) == -99) CYCLE

  ! Copy the real part of the lookup
  DO p_lookup = 1, len_real_lookup
    temp_lookup_real(p_lookup) = TRANSFER(                                     &
        temp_lookup(p_lookup+len_integer_lookup, i_field),                     &
                                      temp_lookup_real(1))
  END DO

  ! This call should set all properties we need from the STASHmaster file
  ! Currently setting a grid code of 1, as a temporary hack
  STATUS = self%fields(i_field)%set_stashmaster_properties(1_int64)
  IF (STATUS%icode /= shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Error setting STASHmaster properties '    &
                                // ' for field ', i_field
    RETURN
  END IF

  STATUS = self%fields(i_field)%set_lookup(temp_lookup(1:len_integer_lookup,   &
                                              i_field), temp_lookup_real)

  IF (STATUS%icode /= shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Error setting lookup for field ',         &
                                    i_field
    RETURN
  END IF

  IF (is_variable_resolution) THEN
    ! This is a variable resolution field
    STATUS = self%fields(i_field)%set_longitudes(                              &
        self%column_dependent_constants(                                       &
                              1:temp_lookup(lbnpt, i_field), 1))

    IF (STATUS%icode /= shumlib_success) THEN
      WRITE(STATUS%message, '(A,I0)') 'Error setting col-dep constants ' //    &
                                    'for field ', i_field
      RETURN
    END IF
    STATUS = self%fields(i_field)%set_latitudes(                               &
        self%row_dependent_constants(                                          &
                              1:temp_lookup(lbrow, i_field), 1))
    IF (STATUS%icode /= shumlib_success) THEN
      WRITE(STATUS%message, '(A,I0)') 'Error setting row-dep constants ' //    &
                                    'for field ', i_field
      RETURN
    END IF
  END IF ! var/fixed-res

  self%num_fields = self%num_fields + 1

  IF (temp_lookup(lbuser4, i_field) == stash_land_sea_mask) THEN
    self%field_number_land_sea_mask = self%num_fields
  END IF

END DO

DEALLOCATE(temp_lookup)

! If the land-sea mask has been found, read it in so we can deal with
! land-packed fields
IF (self%field_number_land_sea_mask > 0_int64) THEN
  STATUS = self%read_field(self%field_number_land_sea_mask)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to load land-sea mask'
    RETURN
  END IF
END IF

WRITE(STATUS%message, '(A,I0,A,A)') 'Loaded ',self%num_fields,                 &
      ' field headers successfully from file ', TRIM(self%filename)
END FUNCTION read_header

!-------------------------------------------------------------------------------

FUNCTION read_field(self, field_number) RESULT(STATUS)
USE f_shum_lookup_indices_mod, ONLY:                                           &
    lbpack, lbuser1, lbrow, lbnpt, lbuser4, bmdi, lblrec
USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_read_field_data
USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)      :: self
INTEGER(KIND=INT64), INTENT(IN)  :: field_number

REAL(KIND=REAL64),   ALLOCATABLE :: field_data_r64(:)
INTEGER(KIND=INT64), ALLOCATABLE :: field_data_i64(:)
REAL(KIND=REAL32),   ALLOCATABLE :: field_data_r32(:)
INTEGER(KIND=INT32), ALLOCATABLE :: field_data_i32(:)
INTEGER(KIND=INT64)              :: rows, cols, tmp_int

REAL(KIND=REAL64),   ALLOCATABLE :: tmp_field_data_r64(:,:)
REAL(KIND=REAL32),   ALLOCATABLE :: tmp_field_data_r32(:,:)
INTEGER(KIND=INT64), ALLOCATABLE :: tmp_field_data_i64(:,:)
INTEGER(KIND=INT32), ALLOCATABLE :: tmp_field_data_i32(:,:)
INTEGER(KIND=INT64), ALLOCATABLE :: lsm_data(:,:)

INTEGER(KIND=INT64)              :: i_value, j_value, k_count
INTEGER(KIND=INT64)              :: packing, user1, stash, lendisk
REAL(KIND=REAL64)                :: mdi

TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0,A)') 'Field number ',field_number,              &
           ' does not exist in file ',TRIM(self%filename)
  RETURN
END IF

IF (.NOT.self%fixed_length_header_read) then
  ! Header has not been loaded, so automatically load it
  STATUS = self%read_header()
  IF (STATUS%icode /= shumlib_success) THEN
    ! Error reading header, return control immediately with error
    RETURN
  END IF
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(1_int64, tmp_int)
IF (tmp_int == um_imdi .OR. tmp_int == -99_int64) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Field does not exist'
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(lbrow, rows)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Error getting number of rows in field ',    &
                                 field_number
  RETURN
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(lbnpt, cols)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Error getting number of columns in '        &
                                  //'field ', field_number
  RETURN
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(lbpack, packing)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Error getting packing for field ',          &
                                 field_number
  RETURN
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(lbuser1, user1)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Error getting data type for field ',        &
                                 field_number
  RETURN
END IF

! Land-packed data
IF (MOD((packing/100_int64), 10_int64) > 0_int64) THEN
  ! The field will be land-packed
  IF (self%field_number_land_sea_mask < 1_int64) THEN
    STATUS%icode = 1_int64
    STATUS%message = 'Tried to read compressed data but ' //                   &
                   'land-sea mask not found'
    RETURN
  END IF
  STATUS = self%fields(self%field_number_land_sea_mask)                        &
                                       %get_lookup_by_index(lbrow, rows)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get number of rows in land-sea mask'
    RETURN
  END IF
  STATUS = self%fields(self%field_number_land_sea_mask)                        &
                                       %get_lookup_by_index(lbnpt, cols)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get number of columns in land-sea mask'
    RETURN
  END IF

  ! Compressed fields have rows=cols=0 by convention, so we need to change
  ! this so the array is allocated to be the correct size
  STATUS = self%fields(field_number)%set_lookup_by_index(lbrow, rows)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to set number of rows for compressed field'
    RETURN
  END IF
  STATUS = self%fields(field_number)%set_lookup_by_index(lbnpt, cols)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to set number of columns for compressed field'
    RETURN
  END IF

  ALLOCATE(lsm_data(cols, rows))
  STATUS = self%fields(self%field_number_land_sea_mask)%get_data(lsm_data)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get land-sea mask for compressed field'
    RETURN
  END IF

  STATUS = self%fields(field_number)%get_lookup_by_index(lblrec, lendisk)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get length on disk for compressed field'
    RETURN
  END IF

  SELECT CASE (MOD((packing/100_int64), 10_int64))
  CASE (1_int64)
    ! Land-compressed
    SELECT CASE (MOD(packing, 10_int64))
    CASE (0_int64)
      ! Unpacked
      SELECT CASE (user1)
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r64(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_r64,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              tmp_field_data_r64(i_value, j_value) =                           &
                                                field_data_r64(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_r64(i_value, j_value) = um_rmdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i64(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_i64,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              tmp_field_data_i64(i_value, j_value) =                           &
                                                field_data_i64(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_i64(i_value, j_value) = um_imdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF
      END SELECT


    CASE (2_int64)
      ! 32-bit truncated

      SELECT CASE (user1)
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r32(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_r32,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              tmp_field_data_r64(i_value, j_value) =                           &
                                                field_data_r32(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_r64(i_value, j_value) = um_rmdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i32(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_i32,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              tmp_field_data_i64(i_value, j_value) =                           &
                                                field_data_i32(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_i64(i_value, j_value) = um_imdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF
      END SELECT

    CASE DEFAULT
      ! Can't WGDOS pack this sort of compression
      STATUS%icode = 1_int64
      STATUS%message = 'WGDOS-packed land-compressed field not implemented'
      RETURN
    END SELECT

  CASE (2_int64)
    ! Sea-compress
    SELECT CASE (MOD(packing, 10_int64))
    CASE (0_int64)
      ! Unpacked
      SELECT CASE (user1)
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r64(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_r64,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              tmp_field_data_r64(i_value, j_value) =                           &
                                                field_data_r64(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_r64(i_value, j_value) = um_rmdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i64(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_i64,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              tmp_field_data_i64(i_value, j_value) =                           &
                                                field_data_i64(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_i64(i_value, j_value) = um_imdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF
      END SELECT


    CASE (2_int64)
      ! 32-bit truncated

      SELECT CASE (user1)
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r32(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_r32,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              tmp_field_data_r64(i_value, j_value) =                           &
                                                field_data_r32(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_r64(i_value, j_value) = um_rmdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i32(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS%icode = f_shum_read_field_data(                                 &
                                        self%file_identifier,                  &
                                        field_number,                          &
                                        field_data_i32,                        &
                                        STATUS%message)
        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              tmp_field_data_i64(i_value, j_value) =                           &
                                                field_data_i32(k_count)
              k_count = k_count + 1
            ELSE
              tmp_field_data_i64(i_value, j_value) = um_imdi
            END IF
          END DO
        END DO

        STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error setting data for '            &
                                          //'field ', field_number
          RETURN
        END IF
      END SELECT

    CASE DEFAULT
      ! Can't WGDOS pack this sort of compression
      STATUS%icode = 1_int64
      STATUS%message = 'WGDOS-packed sea-compressed field not implemented'
      RETURN
    END SELECT

  CASE DEFAULT
    STATUS%icode = 1_int64
    WRITE(STATUS%message,'(A,I0)') 'Unknown compression type in lbpack ',      &
                                    packing
    RETURN
  END SELECT
ELSE

  SELECT CASE (MOD(packing, 10_int64))
  CASE (0_int64)
    ! Unpacked 64-bit

    SELECT CASE (user1)
    CASE (1_int64)
      ! Real - can copy straight into object array
      STATUS%icode = f_shum_read_field_data(                                   &
                                      self%file_identifier,                    &
                                      field_number,                            &
                                      field_data_r64,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF
      ALLOCATE(tmp_field_data_r64(cols, rows))
      tmp_field_data_r64 = RESHAPE(field_data_r64, (/cols, rows/))
      STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Error setting data for field ',       &
                                       field_number
        RETURN
      END IF

    CASE (2_int64, 3_int64)
      ! Integer or Logical - can copy straight into object array
      STATUS%icode = f_shum_read_field_data(                                   &
                                      self%file_identifier,                    &
                                      field_number,                            &
                                      field_data_i64,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF
      ALLOCATE(tmp_field_data_i64(cols, rows))
      tmp_field_data_i64 = RESHAPE(field_data_i64, (/cols, rows/))
      STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Error setting data for field ',       &
                                       field_number
        RETURN
      END IF
    END SELECT
  CASE (1_int64)
    ! WGDOS - always 32-bit integer
    STATUS%icode = f_shum_read_field_data(self%file_identifier,                &
                                    field_number,                              &
                                    field_data_i32,                            &
                                    STATUS%message)
    IF (STATUS%icode /= shumlib_success) THEN
      RETURN
    END IF

    ! Unpacking, unlike FF, doesn't allocate the array
    ALLOCATE(tmp_field_data_r64(cols, rows))

    STATUS = self%fields(field_number)%get_lookup_by_index(bmdi, mdi)
    ! Call WGDOS unpacking
    STATUS%icode = f_shum_wgdos_unpack(field_data_i32,                         &
                                mdi,                                           &
                                tmp_field_data_r64,                            &
                                STATUS%message)
    IF (STATUS%icode /= shumlib_success) THEN
      RETURN
    END IF
    STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
    IF (STATUS%icode /= shumlib_success) THEN
      WRITE(STATUS%message, '(A,I0)') 'Error setting data for field ',         &
                                     field_number
      RETURN
    END IF
  CASE (2_int64)
    ! 32-bit Truncated
    SELECT CASE (user1)
    CASE (1_int64)
      ! Real
      STATUS%icode = f_shum_read_field_data(                                   &
                                      self%file_identifier,                    &
                                      field_number,                            &
                                      field_data_r32,                          &
                                      STATUS%message)

      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

      ! Promote to 64-bit
      ALLOCATE(tmp_field_data_r64(cols, rows))
      ALLOCATE(tmp_field_data_r32(cols, rows))
      tmp_field_data_r32 = RESHAPE(field_data_r32, (/cols, rows/))
      DO j_value = 1, rows
        DO i_value = 1, cols
          tmp_field_data_r64(i_value,j_value) = tmp_field_data_r32(            &
                                                         i_value,j_value)
        END DO
      END DO
      STATUS = self%fields(field_number)%set_data(tmp_field_data_r64)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Error setting data for field ',       &
                                       field_number
        RETURN
      END IF

    CASE (2_int64, 3_int64)
      ! Integer or Logical
      STATUS%icode = f_shum_read_field_data(                                   &
                                      self%file_identifier,                    &
                                      field_number,                            &
                                      field_data_i32,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

      ! Promote to 64-bit
      ALLOCATE(tmp_field_data_i64(cols, rows))
      ALLOCATE(tmp_field_data_i32(cols, rows))
      tmp_field_data_i32 = RESHAPE(field_data_i32, (/cols, rows/))
      DO j_value = 1, rows
        DO i_value = 1, cols
          tmp_field_data_i64(i_value, j_value) = tmp_field_data_i32(           &
                                                          i_value, j_value)
        END DO
      END DO
      STATUS = self%fields(field_number)%set_data(tmp_field_data_i64)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Error setting data for field ',       &
                                       field_number
        RETURN
      END IF

    END SELECT
  CASE DEFAULT
    STATUS%icode = 1_int64
    STATUS%message = 'Unrecognised packing type'
    RETURN
  END SELECT

END IF

IF (ALLOCATED(tmp_field_data_i64)) THEN
  DEALLOCATE(tmp_field_data_i64)
END IF
IF (ALLOCATED(tmp_field_data_r64)) THEN
  DEALLOCATE(tmp_field_data_r64)
END IF
IF (ALLOCATED(tmp_field_data_i32)) THEN
  DEALLOCATE(tmp_field_data_i32)
END IF
IF (ALLOCATED(tmp_field_data_r32)) THEN
  DEALLOCATE(tmp_field_data_r32)
END IF

IF (ALLOCATED(field_data_i64)) THEN
  DEALLOCATE(field_data_i64)
END IF
IF (ALLOCATED(field_data_r64)) THEN
  DEALLOCATE(field_data_r64)
END IF
IF (ALLOCATED(field_data_i32)) THEN
  DEALLOCATE(field_data_i32)
END IF
IF (ALLOCATED(field_data_r32)) THEN
  DEALLOCATE(field_data_r32)
END IF
IF (ALLOCATED(lsm_data)) THEN
  DEALLOCATE(lsm_data)
END IF

STATUS = self%fields(field_number)%get_lookup_by_index(lbuser4, stash)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Error getting STASH code for field ',       &
                                 field_number
  RETURN
END IF
WRITE(STATUS%message, '(A,I0,A,I0,A,I0,A,I0,A)') 'Field ',                     &
      field_number, ' (STASH ', stash,                                         &
      ') loaded with ', cols, 'x', rows,' points'
END FUNCTION read_field

!-------------------------------------------------------------------------------

FUNCTION write_header(self) RESULT(STATUS)
USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_write_fixed_length_header,                                            &
  f_shum_write_integer_constants,                                              &
  f_shum_write_real_constants,                                                 &
  f_shum_write_level_dependent_constants,                                      &
  f_shum_write_row_dependent_constants,                                        &
  f_shum_write_column_dependent_constants,                                     &
  f_shum_write_additional_parameters,                                          &
  f_shum_write_extra_constants,                                                &
  f_shum_write_temp_histfile,                                                  &
  f_shum_write_compressed_index

IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)      :: self

TYPE(shum_ff_status_type) :: STATUS    ! Return status object

STATUS%icode = shumlib_success

STATUS%icode = f_shum_write_fixed_length_header(self%file_identifier,          &
                                         self%fixed_length_header,             &
                                         STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_write_integer_constants(self%file_identifier,            &
                                       self%integer_constants,                 &
                                       STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_write_real_constants(self%file_identifier,               &
                                    self%real_constants,                       &
                                    STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

STATUS%icode = f_shum_write_level_dependent_constants(self%file_identifier,    &
                                            self%level_dependent_constants,    &
                                            STATUS%message)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF

IF (ALLOCATED(self%row_dependent_constants) .AND.                              &
    ALLOCATED(self%column_dependent_constants)) THEN

  STATUS%icode = f_shum_write_row_dependent_constants(self%file_identifier,    &
                                               self%row_dependent_constants,   &
                                               STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF

  STATUS%icode = f_shum_write_column_dependent_constants(                      &
                                            self%file_identifier,              &
                                            self%column_dependent_constants,   &
                                            STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF

ELSE IF (ALLOCATED(self%row_dependent_constants) .OR.                          &
    ALLOCATED(self%column_dependent_constants)) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Tried to write a file with only partial ' //               &
                 'variable-resolution headers'
  RETURN
END IF

IF (ALLOCATED(self%additional_parameters)) THEN
  STATUS%icode = f_shum_write_additional_parameters(self%file_identifier,      &
                                             self%additional_parameters,       &
                                             STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF


IF (ALLOCATED(self%extra_constants)) THEN
  STATUS%icode = f_shum_write_extra_constants(self%file_identifier,            &
                                       self%extra_constants,                   &
                                       STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF



IF (ALLOCATED(self%temp_histfile)) THEN
  STATUS%icode = f_shum_write_temp_histfile(self%file_identifier,              &
                                     self%temp_histfile,                       &
                                     STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF


IF (ALLOCATED(self%compressed_index_1)) THEN
  STATUS%icode = f_shum_write_compressed_index(self%file_identifier,           &
                                        self%compressed_index_1,               &
                                        1_int64,                               &
                                        STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF

IF (ALLOCATED(self%compressed_index_2)) THEN
  STATUS%icode = f_shum_write_compressed_index(self%file_identifier,           &
                                        self%compressed_index_2,               &
                                        2_int64,                               &
                                        STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF

IF (ALLOCATED(self%compressed_index_3)) THEN
  STATUS%icode = f_shum_write_compressed_index(self%file_identifier,           &
                                        self%compressed_index_3,               &
                                        3_int64,                               &
                                        STATUS%message)
  IF (STATUS%icode /= shumlib_success) THEN
    RETURN
  END IF
END IF

WRITE(STATUS%message, '(A,A)') 'Written header of file ', TRIM(self%filename)

END FUNCTION write_header

!-------------------------------------------------------------------------------

FUNCTION write_field(self, field_number) RESULT(STATUS)
USE f_shum_fieldsfile_mod, ONLY:                                               &
        f_shum_write_field_data, f_shum_lookup_dim1_len
USE f_shum_lookup_indices_mod, ONLY: lbpack, lbuser1, lbrow, lbnpt, lbuser4,   &
                                     bacc, bmdi
USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)      :: self
INTEGER(KIND=INT64), INTENT(IN)  :: field_number
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=INT64) :: lookup(f_shum_lookup_dim1_len)
INTEGER(KIND=INT64) :: lookup_int(len_integer_lookup)
REAL(KIND=REAL64)   :: lookup_real(len_real_lookup)

INTEGER(KIND=INT64) :: i_lookup
INTEGER(KIND=INT64) :: i_value, j_value, k_count

REAL(KIND=REAL64),   ALLOCATABLE :: field_data_r64(:)
INTEGER(KIND=INT64), ALLOCATABLE :: field_data_i64(:)
REAL(KIND=REAL32),   ALLOCATABLE :: field_data_r32(:)
INTEGER(KIND=INT32), ALLOCATABLE :: field_data_i32(:)
INTEGER(KIND=INT64)              :: rows, cols

REAL(KIND=REAL64),   ALLOCATABLE :: tmp_field_data_r64(:,:)
REAL(KIND=REAL32),   ALLOCATABLE :: tmp_field_data_r32(:,:)
INTEGER(KIND=INT64), ALLOCATABLE :: tmp_field_data_i64(:,:)
INTEGER(KIND=INT32), ALLOCATABLE :: tmp_field_data_i32(:,:)
INTEGER(KIND=INT64), ALLOCATABLE :: lsm_data(:,:)

INTEGER(KIND=INT64) :: lendisk
INTEGER(KIND=INT64) :: acc
CHARACTER(LEN=64) :: packing_name

STATUS%icode = shumlib_success
IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0,A,A)') 'Field number ',field_number,            &
         ' does not exist in file ',TRIM(self%filename)
  RETURN
END IF

! Get the lookup
STATUS = self%fields(field_number)%get_lookup(lookup_int, lookup_real)
IF (STATUS%icode /= shumlib_success) THEN
  WRITE(STATUS%message, '(A,I0)') 'Failed to get lookup for field ',           &
                                field_number
  RETURN
END IF
lookup(1:len_integer_lookup) = lookup_int(1:len_integer_lookup)

! Convert real part of lookup to integer
DO i_lookup = 1, len_real_lookup
  lookup(i_lookup + len_integer_lookup) = TRANSFER(lookup_real(i_lookup),      &
                                                   lookup(1_int64))
END DO

rows = lookup_int(lbrow)
cols = lookup_int(lbnpt)

! Land-packed data
IF (MOD((lookup_int(lbpack)/100_int64), 10_int64) > 0_int64) THEN
  ! The field will be land/sea-compressed
  IF (self%field_number_land_sea_mask < 1_int64) THEN
    STATUS%icode = 1_int64
    STATUS%message = 'Tried to read compressed data but ' //                   &
                   'land-sea mask not found'
    RETURN
  END IF
  STATUS = self%fields(self%field_number_land_sea_mask)                        &
                                       %get_lookup_by_index(lbrow, rows)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get number of rows in land-sea mask'
    RETURN
  END IF
  STATUS = self%fields(self%field_number_land_sea_mask)                        &
                                       %get_lookup_by_index(lbnpt, cols)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get number of columns in land-sea mask'
    RETURN
  END IF

  ALLOCATE(lsm_data(cols, rows))
  STATUS = self%fields(self%field_number_land_sea_mask)%get_data(lsm_data)
  IF (STATUS%icode /= shumlib_success) THEN
    STATUS%message = 'Failed to get land-sea mask for compressed field'
    RETURN
  END IF


  SELECT CASE (MOD((lookup_int(lbpack)/100_int64), 10_int64))
  CASE (1_int64)
    ! Land-compressed
    lendisk = SUM(lsm_data)
    SELECT CASE (MOD(lookup_int(lbpack), 10_int64))
    CASE (0_int64)
      ! Unpacked
      packing_name = 'land-compressed unpacked'
      SELECT CASE (lookup_int(lbuser1))
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r64(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              field_data_r64(k_count) =                                        &
                                    tmp_field_data_r64(i_value, j_value)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_r64,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF


      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i64(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              field_data_i64(k_count) =                                        &
                                    tmp_field_data_i64(i_value, j_value)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_i64,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF

      END SELECT
    CASE (2_int64)
      ! 32-bit truncated
      packing_name = 'land-compressed 32-bit truncated'
      SELECT CASE (lookup_int(lbuser1))
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r32(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              field_data_r32(k_count) =                                        &
                    REAL(tmp_field_data_r64(i_value, j_value),KIND=REAL32)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_r32,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF


      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i32(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 1_int64) THEN
              field_data_i32(k_count) =                                        &
                      INT(tmp_field_data_i64(i_value, j_value),KIND=INT32)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_i32,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
      END SELECT
    CASE DEFAULT
      ! Can't WGDOS pack this sort of compression
      STATUS%icode = 1_int64
      STATUS%message = 'WGDOS-packed land-compressed field not ' //            &
                       'implemented'
      RETURN
    END SELECT

  CASE (2_int64)
    ! Sea-compress
    lendisk = SIZE(lsm_data) - SUM(lsm_data)
    SELECT CASE (MOD(lookup_int(lbpack), 10_int64))
    CASE (0_int64)
      ! Unpacked
      packing_name = 'sea-compressed unpacked'
      SELECT CASE (lookup_int(lbuser1))
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r64(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              field_data_r64(k_count) =                                        &
                                    tmp_field_data_r64(i_value, j_value)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_r64,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF


      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i64(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              field_data_i64(k_count) =                                        &
                                    tmp_field_data_i64(i_value, j_value)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_i64,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF

      END SELECT
    CASE (2_int64)
      ! 32-bit truncated
      packing_name = 'sea-compressed 32-bit truncated'
      SELECT CASE (lookup_int(lbuser1))
      CASE (1_int64)
        ! Real data
        ALLOCATE(field_data_r32(lendisk))
        ALLOCATE(tmp_field_data_r64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              field_data_r32(k_count) =                                        &
                    REAL(tmp_field_data_r64(i_value, j_value),KIND=REAL32)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_r32,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF


      CASE (2_int64, 3_int64)
        ! Integer or logical
        ALLOCATE(field_data_i32(lendisk))
        ALLOCATE(tmp_field_data_i64(cols, rows))

        STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
        IF (STATUS%icode /= shumlib_success) THEN
          WRITE(STATUS%message, '(A,I0)') 'Error getting data for '            &
                                          //'field ', field_number
          RETURN
        END IF

        k_count = 1
        DO j_value = 1, rows
          DO i_value = 1, cols
            IF (lsm_data(i_value, j_value) == 0_int64) THEN
              field_data_i32(k_count) =                                        &
                      INT(tmp_field_data_i64(i_value, j_value),KIND=INT32)
              k_count = k_count + 1
            END IF
          END DO
        END DO

        ! Compressed fields have rows=cols=0 by convention, so we need
        ! to obey this
        lookup(lbrow) = 0_int64
        lookup(lbnpt) = 0_int64

        STATUS%icode = f_shum_write_field_data(self%file_identifier,           &
                                        lookup,                                &
                                        field_data_i32,                        &
                                        STATUS%message)

        IF (STATUS%icode /= shumlib_success) THEN
          RETURN
        END IF
      END SELECT
    CASE DEFAULT
      ! Can't WGDOS pack this sort of compression
      STATUS%icode = 1_int64
      STATUS%message = 'WGDOS-packed sea-compressed field not '//              &
                       'implemented'
      RETURN
    END SELECT

  CASE DEFAULT
    STATUS%icode = 1_int64
    WRITE(STATUS%message, '(A,I0)') 'Unknown compression type in lbpack ',     &
         lookup_int(lbpack)
    RETURN
  END SELECT

ELSE !Not land/sea-compressed

  SELECT CASE (MOD(lookup_int(lbpack), 10_int64))
  CASE (0_int64)
    ! Unpacked 64-bit
    packing_name = 'unpacked'
    SELECT CASE (lookup_int(lbuser1))
    CASE (1_int64)
      ! Real
      ALLOCATE(tmp_field_data_r64(cols, rows))
      STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)

      IF (STATUS%icode /= shumlib_success) THEN
        STATUS%message = 'Failed to get real data'
        RETURN
      END IF
      ALLOCATE(field_data_r64(rows*cols))
      field_data_r64 = RESHAPE(tmp_field_data_r64, (/cols * rows/))

      STATUS%icode = f_shum_write_field_data(self%file_identifier,             &
                                      lookup,                                  &
                                      field_data_r64,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

    CASE (2_int64, 3_int64)
      ! Integer or Logical

      ALLOCATE(tmp_field_data_i64(cols, rows))
      STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
      IF (STATUS%icode /= shumlib_success) THEN
        STATUS%message = 'Failed to get integer data'
        RETURN
      END IF
      ALLOCATE(field_data_i64(rows*cols))
      field_data_i64 = RESHAPE(tmp_field_data_i64, (/rows * cols/))

      STATUS%icode = f_shum_write_field_data(self%file_identifier,             &
                                      lookup,                                  &
                                      field_data_i64,                          &
                                      STATUS%message)

      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

    END SELECT
  CASE (1_int64)
    ! WGDOS packed
    ALLOCATE(tmp_field_data_r64(cols, rows))
    STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
    IF (STATUS%icode /= shumlib_success) THEN
      STATUS%message = 'Unable to get real data'
      RETURN
    END IF

    ! For some reason the packing expects BACC to be an integer
    acc = INT(lookup_real(bacc-len_integer_lookup), INT64)
    WRITE(packing_name, '(A,I0)') 'WGDOS-packed: ',acc
    STATUS%icode = f_shum_wgdos_pack(tmp_field_data_r64,                       &
                                  acc,                                         &
                                  lookup_real(bmdi-len_integer_lookup),        &
                                  field_data_i32,                              &
                                  STATUS%message)
    IF (STATUS%icode /= shumlib_success) THEN
      RETURN
    END IF

    STATUS%icode = f_shum_write_field_data(self%file_identifier,               &
                                    lookup,                                    &
                                    field_data_i32,                            &
                                    STATUS%message)

    IF (STATUS%icode /= shumlib_success) THEN
      RETURN
    END IF

  CASE (2_int64)
    ! 32-bit Truncated
    packing_name = '32-bit truncated'
    SELECT CASE (lookup_int(lbuser1))
    CASE (1_int64)
      ! Real
      ALLOCATE(tmp_field_data_r64(cols, rows))
      STATUS = self%fields(field_number)%get_data(tmp_field_data_r64)
      IF (STATUS%icode /= shumlib_success) THEN
        STATUS%message = 'Unable to get real data'
        RETURN
      END IF
      ALLOCATE(tmp_field_data_r32(cols, rows))
      ALLOCATE(field_data_r32(rows*cols))
      DO j_value = 1, rows
        DO i_value = 1, cols
          tmp_field_data_r32(i_value,j_value) = REAL(tmp_field_data_r64(       &
                                             i_value,j_value), REAL32)
        END DO
      END DO

      field_data_r32 = RESHAPE(tmp_field_data_r32, (/rows*cols/))
      STATUS%icode = f_shum_write_field_data(self%file_identifier,             &
                                      lookup,                                  &
                                      field_data_r32,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

    CASE (2_int64, 3_int64)
      ! Integer or Logical
      ALLOCATE(tmp_field_data_i64(cols, rows))
      STATUS = self%fields(field_number)%get_data(tmp_field_data_i64)
      IF (STATUS%icode /= shumlib_success) THEN
        STATUS%message = 'Unable to get integer data'
        RETURN
      END IF

      ALLOCATE(tmp_field_data_i32(cols, rows))
      ALLOCATE(field_data_i32(rows*cols))

      DO j_value = 1, rows
        DO i_value = 1, cols
          tmp_field_data_i32(i_value,j_value) = INT(tmp_field_data_i64(        &
                                                 i_value,j_value), INT32)
        END DO
      END DO
      field_data_i32 = RESHAPE(tmp_field_data_i32, (/rows*cols/))
      STATUS%icode = f_shum_write_field_data(self%file_identifier,             &
                                      lookup,                                  &
                                      field_data_i32,                          &
                                      STATUS%message)
      IF (STATUS%icode /= shumlib_success) THEN
        RETURN
      END IF

    END SELECT
  CASE DEFAULT
    STATUS%icode = 1_int64
    STATUS%message = 'Unrecognised packing type'
    RETURN
  END SELECT
END IF ! land-compressed or not

IF (ALLOCATED(tmp_field_data_i64)) THEN
  DEALLOCATE(tmp_field_data_i64)
END IF
IF (ALLOCATED(tmp_field_data_r64)) THEN
  DEALLOCATE(tmp_field_data_r64)
END IF
IF (ALLOCATED(tmp_field_data_i32)) THEN
  DEALLOCATE(tmp_field_data_i32)
END IF
IF (ALLOCATED(tmp_field_data_r32)) THEN
  DEALLOCATE(tmp_field_data_r32)
END IF

IF (ALLOCATED(field_data_i64)) THEN
  DEALLOCATE(field_data_i64)
END IF
IF (ALLOCATED(field_data_r64)) THEN
  DEALLOCATE(field_data_r64)
END IF
IF (ALLOCATED(field_data_i32)) THEN
  DEALLOCATE(field_data_i32)
END IF
IF (ALLOCATED(field_data_r32)) THEN
  DEALLOCATE(field_data_r32)
END IF
IF (ALLOCATED(lsm_data)) THEN
  DEALLOCATE(lsm_data)
END IF

WRITE(STATUS%message, '(A,I0,A,I0,A,I0,A,I0,A,A,A)') 'Field ',                 &
      field_number, ' (STASH ', lookup_int(lbuser4),                           &
      ') written with ', cols, 'x', rows,' points (', TRIM(packing_name),      &
      ')'

END FUNCTION write_field

!-------------------------------------------------------------------------------

FUNCTION close_file(self) RESULT(STATUS)
USE f_shum_fieldsfile_mod, ONLY:                                               &
  f_shum_close_file
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT) :: self
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

STATUS%icode = f_shum_close_file(self%file_identifier, STATUS%message)

! reset the shum_file_type fields with save attribute to initial values
self%fixed_length_header_read=.FALSE.
self%file_identifier = um_imdi
self%fixed_length_header = um_imdi
self%field_number_land_sea_mask = -99
self%num_fields = 0

! deallocate to reset the filename. the status of other allocatable members
! is checked during file open so they do not need to be reset here.
IF (ALLOCATED(self%filename)) DEALLOCATE(self%filename)

END FUNCTION close_file

!-------------------------------------------------------------------------------
! Header Accessors
!-------------------------------------------------------------------------------

FUNCTION set_fixed_length_header(self, fixed_length_header) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64), INTENT(IN) :: fixed_length_header(                        &
                                              f_shum_fixed_length_header_len)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

self%fixed_length_header = fixed_length_header
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_fixed_length_header

!-------------------------------------------------------------------------------

FUNCTION get_fixed_length_header(self, fixed_length_header) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64) :: fixed_length_header(f_shum_fixed_length_header_len)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

fixed_length_header = self%fixed_length_header
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION get_fixed_length_header

!-------------------------------------------------------------------------------

FUNCTION set_fixed_length_header_by_index(self, num_index, value_to_set)       &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64) :: num_index, value_to_set
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (num_index < 1_int64 .OR. num_index > f_shum_fixed_length_header_len)       &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range:', num_index
ELSE
  self%fixed_length_header(num_index) = value_to_set
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION set_fixed_length_header_by_index

!-------------------------------------------------------------------------------

FUNCTION get_fixed_length_header_by_index(self, num_index, value_to_get)       &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64) :: num_index, value_to_get
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (num_index < 1_int64 .OR. num_index > f_shum_fixed_length_header_len)       &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range:', num_index
ELSE
  value_to_get = self%fixed_length_header(num_index)
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION get_fixed_length_header_by_index

!-------------------------------------------------------------------------------

FUNCTION set_integer_constants(self, integer_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64), INTENT(IN) :: integer_constants(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=INT64) :: s_ic

s_ic = SIZE(integer_constants)

IF (ALLOCATED(self%integer_constants)) THEN
  IF (SIZE(self%integer_constants) /= s_ic) THEN
    DEALLOCATE(self%integer_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%integer_constants)) THEN
  ALLOCATE(self%integer_constants(s_ic))
END IF

self%integer_constants(1:s_ic) = integer_constants(1:s_ic)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_integer_constants

!-------------------------------------------------------------------------------

FUNCTION get_integer_constants(self, integer_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64), ALLOCATABLE :: integer_constants(:)
INTEGER(KIND=INT64) :: s_ic
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (ALLOCATED(self%integer_constants)) THEN
  s_ic = SIZE(self%integer_constants)
  IF (ALLOCATED(integer_constants)) THEN
    IF (SIZE(integer_constants)/=s_ic) THEN
      DEALLOCATE(integer_constants)
    END IF
  END IF
  IF (.NOT. ALLOCATED(integer_constants)) THEN
    ALLOCATE(integer_constants(s_ic))
  END IF
  integer_constants(1:s_ic) = self%integer_constants(1:s_ic)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = 1_int64
  STATUS%message = 'Attempted to get unset integer constants'
END IF

END FUNCTION get_integer_constants

!-------------------------------------------------------------------------------

FUNCTION set_integer_constants_by_index(self, num_index, value_to_set)         &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64) :: num_index, value_to_set
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (.NOT. ALLOCATED(self%integer_constants)) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Integer constants not allocated'
ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%integer_constants))    &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range: ', num_index
ELSE
  self%integer_constants(num_index) = value_to_set
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION set_integer_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION get_integer_constants_by_index(self, num_index, value_to_get)         &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64) :: num_index, value_to_get
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (.NOT. ALLOCATED(self%integer_constants)) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Integer constants not allocated'
ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%integer_constants))    &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range: ', num_index
ELSE
  value_to_get = self%integer_constants(num_index)
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION get_integer_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION set_real_constants(self, real_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: real_constants(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_rc

s_rc = SIZE(real_constants)

IF (ALLOCATED(self%real_constants)) THEN
  IF (SIZE(self%real_constants) /= s_rc) THEN
    DEALLOCATE(self%real_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%real_constants)) THEN
  ALLOCATE(self%real_constants(s_rc))
END IF

self%real_constants(1:s_rc) = real_constants(1:s_rc)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_real_constants

!-------------------------------------------------------------------------------

FUNCTION get_real_constants(self, real_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: real_constants(:)
INTEGER(KIND=INT64) :: s_rc
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (ALLOCATED(self%real_constants)) THEN
  s_rc = SIZE(self%real_constants)
  IF (ALLOCATED(real_constants)) THEN
    IF (SIZE(real_constants)/=s_rc) THEN
      DEALLOCATE(real_constants)
    END IF
  END IF
  IF (.NOT. ALLOCATED(real_constants)) THEN
    ALLOCATE(real_constants(s_rc))
  END IF
  real_constants(1:s_rc) = self%real_constants(1:s_rc)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = 1_int64
  STATUS%message = 'Attempted to get unset real constants'
END IF

END FUNCTION get_real_constants

!-------------------------------------------------------------------------------

FUNCTION set_real_constants_by_index(self, num_index, value_to_set)            &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64) :: num_index
REAL(KIND=REAL64) :: value_to_set
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (.NOT. ALLOCATED(self%real_constants)) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Real constants not allocated'
ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%real_constants))       &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range: ', num_index
ELSE
  self%real_constants(num_index) = value_to_set
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION set_real_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION get_real_constants_by_index(self, num_index, value_to_get)            &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64) :: num_index
REAL(KIND=REAL64) :: value_to_get
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (.NOT. ALLOCATED(self%real_constants)) THEN
  STATUS%icode = 1_int64
  STATUS%message = 'Real constants not allocated'
ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%real_constants))       &
    THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0)') 'Index out of range: ', num_index
ELSE
  value_to_get = self%real_constants(num_index)
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION get_real_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION set_level_dependent_constants(self, level_dependent_constants)        &
     RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: level_dependent_constants(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ldc1, s_ldc2

s_ldc1 = SIZE(level_dependent_constants,1)
s_ldc2 = SIZE(level_dependent_constants,2)

IF (ALLOCATED(self%level_dependent_constants)) THEN
  IF (SIZE(self%level_dependent_constants,1) /= s_ldc1                         &
      .OR. SIZE(self%level_dependent_constants,2) /= s_ldc2) THEN
    DEALLOCATE(self%level_dependent_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%level_dependent_constants)) THEN
  ALLOCATE(self%level_dependent_constants(1:s_ldc1,1:s_ldc2))
END IF

self%level_dependent_constants(1:s_ldc1,1:s_ldc2) =                            &
                                    level_dependent_constants(1:s_ldc1,1:s_ldc2)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_level_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_level_dependent_constants(self, level_dependent_constants)        &
     RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: level_dependent_constants(:,:)
INTEGER(KIND=INT64) :: s_ldc1,s_ldc2
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (ALLOCATED(self%level_dependent_constants)) THEN
  s_ldc1 = SIZE(self%level_dependent_constants,1)
  s_ldc2 = SIZE(self%level_dependent_constants,2)
  IF (ALLOCATED(level_dependent_constants)) THEN
    IF (SIZE(level_dependent_constants,1)/=s_ldc1 .OR.                         &
        SIZE(level_dependent_constants,2)/=s_ldc2) THEN
      DEALLOCATE(level_dependent_constants)
    END IF
  END IF
  IF (.NOT. ALLOCATED(level_dependent_constants)) THEN
    ALLOCATE(level_dependent_constants(s_ldc1,s_ldc2))
  END IF
  level_dependent_constants(1:s_ldc1,1:s_ldc2) =                               &
                               self%level_dependent_constants(1:s_ldc1,1:s_ldc2)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = 1_int64
  STATUS%message = 'Attempted to get unset level-dependent constants'
END IF

END FUNCTION get_level_dependent_constants
!-------------------------------------------------------------------------------

FUNCTION set_row_dependent_constants(self, row_dependent_constants)            &
      RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: row_dependent_constants(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_rdc1, s_rdc2

s_rdc1 = SIZE(row_dependent_constants,1)
s_rdc2 = SIZE(row_dependent_constants,2)

IF (ALLOCATED(self%row_dependent_constants)) THEN
  IF (SIZE(self%row_dependent_constants,1) /= s_rdc1                           &
      .OR. SIZE(self%row_dependent_constants,2) /= s_rdc2) THEN
    DEALLOCATE(self%row_dependent_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%row_dependent_constants)) THEN
  ALLOCATE(self%row_dependent_constants(1:s_rdc1,1:s_rdc2))
END IF

self%row_dependent_constants(1:s_rdc1,1:s_rdc2) =                              &
                                      row_dependent_constants(1:s_rdc1,1:s_rdc2)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_row_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_row_dependent_constants(self, row_dependent_constants)            &
     RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: row_dependent_constants(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_rdc1,s_rdc2

IF (ALLOCATED(self%row_dependent_constants)) THEN
  s_rdc1 = SIZE(self%row_dependent_constants,1)
  s_rdc2 = SIZE(self%row_dependent_constants,2)

  IF (ALLOCATED(row_dependent_constants)) THEN
    IF (SIZE(row_dependent_constants,1) /= s_rdc1                              &
        .OR. SIZE(row_dependent_constants,2) /= s_rdc2) THEN
      DEALLOCATE(row_dependent_constants)
    END IF
  END IF

  IF (.NOT. ALLOCATED(row_dependent_constants)) THEN
    ALLOCATE(row_dependent_constants(1:s_rdc1,1:s_rdc2))
  END IF
  row_dependent_constants(1:s_rdc1,1:s_rdc2) =                                 &
                                 self%row_dependent_constants(1:s_rdc1,1:s_rdc2)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = -1_int64
  STATUS%message = 'Attempted to get unset row-dependent constants'
END IF

END FUNCTION get_row_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION set_column_dependent_constants(self, column_dependent_constants)      &
     RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: column_dependent_constants(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_cdc1, s_cdc2

s_cdc1 = SIZE(column_dependent_constants,1)
s_cdc2 = SIZE(column_dependent_constants,2)

IF (ALLOCATED(self%column_dependent_constants)) THEN
  IF (SIZE(self%column_dependent_constants,1) /= s_cdc1                        &
      .OR. SIZE(self%column_dependent_constants,2) /= s_cdc2) THEN
    DEALLOCATE(self%column_dependent_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%column_dependent_constants)) THEN
  ALLOCATE(self%column_dependent_constants(1:s_cdc1,1:s_cdc2))
END IF

self%column_dependent_constants(1:s_cdc1,1:s_cdc2) =                           &
                                   column_dependent_constants(1:s_cdc1,1:s_cdc2)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_column_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_column_dependent_constants(self, column_dependent_constants)      &
     RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: column_dependent_constants(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_cdc1,s_cdc2

IF (ALLOCATED(self%column_dependent_constants)) THEN
  s_cdc1 = SIZE(self%column_dependent_constants,1)
  s_cdc2 = SIZE(self%column_dependent_constants,2)

  IF (ALLOCATED(column_dependent_constants)) THEN
    IF (SIZE(column_dependent_constants,1) /= s_cdc1                           &
        .OR. SIZE(column_dependent_constants,2) /= s_cdc2) THEN
      DEALLOCATE(column_dependent_constants)
    END IF
  END IF

  IF (.NOT. ALLOCATED(column_dependent_constants)) THEN
    ALLOCATE(column_dependent_constants(1:s_cdc1,1:s_cdc2))
  END IF
  column_dependent_constants(1:s_cdc1,1:s_cdc2) =                              &
                              self%column_dependent_constants(1:s_cdc1,1:s_cdc2)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = -1_int64
  STATUS%message = 'Attempted to get unset column-dependent constants'
END IF

END FUNCTION get_column_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION set_additional_parameters(self, additional_parameters) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: additional_parameters(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ap1, s_ap2

s_ap1 = SIZE(additional_parameters,1)
s_ap2 = SIZE(additional_parameters,2)

IF (ALLOCATED(self%additional_parameters)) THEN
  IF (SIZE(self%additional_parameters,1) /= s_ap1                              &
      .OR. SIZE(self%additional_parameters,2) /= s_ap2) THEN
    DEALLOCATE(self%additional_parameters)
  END IF
END IF

IF (.NOT. ALLOCATED(self%additional_parameters)) THEN
  ALLOCATE(self%additional_parameters(1:s_ap1,1:s_ap2))
END IF

self%additional_parameters(1:s_ap1,1:s_ap2) =                                  &
                                          additional_parameters(1:s_ap1,1:s_ap2)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_additional_parameters

!-------------------------------------------------------------------------------

FUNCTION get_additional_parameters(self, additional_parameters) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: additional_parameters(:,:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ap1, s_ap2

IF (ALLOCATED(self%additional_parameters)) THEN
  s_ap1 = SIZE(self%additional_parameters,1)
  s_ap2 = SIZE(self%additional_parameters,2)

  IF (ALLOCATED(additional_parameters)) THEN
    IF (SIZE(additional_parameters,1) /= s_ap1                                 &
        .OR. SIZE(additional_parameters,2) /= s_ap2) THEN
      DEALLOCATE(additional_parameters)
    END IF
  END IF

  IF (.NOT. ALLOCATED(additional_parameters)) THEN
    ALLOCATE(additional_parameters(1:s_ap1,1:s_ap2))
  END IF
  additional_parameters(1:s_ap1,1:s_ap2) =                                     &
                                     self%additional_parameters(1:s_ap1,1:s_ap2)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = -1_int64
  STATUS%message = 'Attempted to get unset additional parameters'
END IF

END FUNCTION get_additional_parameters

!-------------------------------------------------------------------------------

FUNCTION set_extra_constants(self, extra_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: extra_constants(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ec

s_ec = SIZE(extra_constants)

IF (ALLOCATED(self%extra_constants)) THEN
  IF (SIZE(self%extra_constants) /= s_ec) THEN
    DEALLOCATE(self%extra_constants)
  END IF
END IF

IF (.NOT. ALLOCATED(self%extra_constants)) THEN
  ALLOCATE(self%extra_constants(s_ec))
END IF

self%extra_constants(1:s_ec) = extra_constants(1:s_ec)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_extra_constants

!-------------------------------------------------------------------------------

FUNCTION get_extra_constants(self, extra_constants) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: extra_constants(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ec

IF (ALLOCATED(self%extra_constants)) THEN
  s_ec = SIZE(self%extra_constants)

  IF (ALLOCATED(extra_constants)) THEN
    IF (SIZE(extra_constants,1) /= s_ec) THEN
      DEALLOCATE(extra_constants)
    END IF
  END IF

  IF (.NOT. ALLOCATED(extra_constants)) THEN
    ALLOCATE(extra_constants(1:s_ec))
  END IF
  extra_constants(1:s_ec) = self%extra_constants(1:s_ec)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = -1_int64
  STATUS%message = 'Attempted to get unset extra constants'
END IF

END FUNCTION get_extra_constants

!-------------------------------------------------------------------------------

FUNCTION set_temp_histfile(self, temp_histfile) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
REAL(KIND=REAL64), INTENT(IN) :: temp_histfile(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_thf

s_thf = SIZE(temp_histfile)

IF (ALLOCATED(self%temp_histfile)) THEN
  IF (SIZE(self%temp_histfile) /= s_thf) THEN
    DEALLOCATE(self%temp_histfile)
  END IF
END IF

IF (.NOT. ALLOCATED(self%temp_histfile)) THEN
  ALLOCATE(self%temp_histfile(s_thf))
END IF

self%temp_histfile(1:s_thf) = temp_histfile(1:s_thf)
STATUS%icode = shumlib_success
STATUS%message = ''

END FUNCTION set_temp_histfile

!-------------------------------------------------------------------------------

FUNCTION get_temp_histfile(self, temp_histfile) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
REAL(KIND=REAL64), ALLOCATABLE :: temp_histfile(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_thf

s_thf = SIZE(temp_histfile)

IF (ALLOCATED(self%temp_histfile)) THEN
  s_thf = SIZE(self%temp_histfile)

  IF (ALLOCATED(temp_histfile)) THEN
    IF (SIZE(temp_histfile,1) /= s_thf) THEN
      DEALLOCATE(temp_histfile)
    END IF
  END IF

  IF (.NOT. ALLOCATED(temp_histfile)) THEN
    ALLOCATE(temp_histfile(1:s_thf))
  END IF
  temp_histfile(1:s_thf) = self%temp_histfile(1:s_thf)
  STATUS%icode = shumlib_success
  STATUS%message = ''
ELSE
  STATUS%icode = -1_int64
  STATUS%message = 'Attempted to get unset temp histfile'
END IF

END FUNCTION get_temp_histfile

!-------------------------------------------------------------------------------

FUNCTION set_compressed_index(self, num_index, compressed_index) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64) :: num_index
REAL(KIND=REAL64), INTENT(IN) :: compressed_index(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ci

s_ci = SIZE(compressed_index)

STATUS%message = ''
SELECT CASE(num_index)
CASE (1_int64)
  IF (ALLOCATED(self%compressed_index_1)) THEN
    IF (SIZE(self%compressed_index_1) /= s_ci) THEN
      DEALLOCATE(self%compressed_index_1)
    END IF
  END IF

  IF (.NOT. ALLOCATED(self%compressed_index_1)) THEN
    ALLOCATE(self%compressed_index_1(s_ci))
  END IF
  self%compressed_index_1(1:s_ci) = compressed_index(1:s_ci)
  STATUS%icode = shumlib_success
CASE (2_int64)
  IF (ALLOCATED(self%compressed_index_2)) THEN
    IF (SIZE(self%compressed_index_2) /= s_ci) THEN
      DEALLOCATE(self%compressed_index_2)
    END IF
  END IF

  IF (.NOT. ALLOCATED(self%compressed_index_2)) THEN
    ALLOCATE(self%compressed_index_2(s_ci))
  END IF
  self%compressed_index_2(1:s_ci) = compressed_index(1:s_ci)
  STATUS%icode = shumlib_success
CASE (3_int64)
  IF (ALLOCATED(self%compressed_index_3)) THEN
    IF (SIZE(self%compressed_index_3) /= s_ci) THEN
      DEALLOCATE(self%compressed_index_3)
    END IF
  END IF

  IF (.NOT. ALLOCATED(self%compressed_index_3)) THEN
    ALLOCATE(self%compressed_index_3(s_ci))
  END IF
  self%compressed_index_3(1:s_ci) = compressed_index(1:s_ci)
  STATUS%icode = shumlib_success
CASE DEFAULT
  STATUS%icode = 1_int64
  STATUS%message = 'Attempted to set invalid compressed index'
END SELECT

END FUNCTION set_compressed_index

!-------------------------------------------------------------------------------

FUNCTION get_compressed_index(self, num_index, compressed_index) RESULT(STATUS)
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN)     :: self
INTEGER(KIND=INT64) :: num_index
REAL(KIND=REAL64), ALLOCATABLE :: compressed_index(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER(KIND=int64) :: s_ci

STATUS%message = ''
SELECT CASE(num_index)
CASE (1_int64)
  IF (ALLOCATED(self%compressed_index_1)) THEN
    s_ci = SIZE(self%compressed_index_1)

    IF (ALLOCATED(compressed_index)) THEN
      IF (SIZE(compressed_index,1) /= s_ci) THEN
        DEALLOCATE(compressed_index)
      END IF
    END IF

    IF (.NOT. ALLOCATED(compressed_index)) THEN
      ALLOCATE(compressed_index(1:s_ci))
    END IF
    compressed_index (1:s_ci)= self%compressed_index_1(1:s_ci)
    STATUS%icode = shumlib_success
  ELSE
    STATUS%icode = -1_int64
    STATUS%message = 'Attempted to get unset compressed index 1'
  END IF
CASE (2_int64)
  IF (ALLOCATED(self%compressed_index_2)) THEN
    s_ci = SIZE(self%compressed_index_2)

    IF (ALLOCATED(compressed_index)) THEN
      IF (SIZE(compressed_index,1) /= s_ci) THEN
        DEALLOCATE(compressed_index)
      END IF
    END IF

    IF (.NOT. ALLOCATED(compressed_index)) THEN
      ALLOCATE(compressed_index(1:s_ci))
    END IF
    compressed_index(1:s_ci) = self%compressed_index_2(1:s_ci)
    STATUS%icode = shumlib_success
  ELSE
    STATUS%icode = -1_int64
    STATUS%message = 'Attempted to get unset compressed index 2'
  END IF
CASE (3_int64)
  IF (ALLOCATED(self%compressed_index_3)) THEN
    s_ci = SIZE(self%compressed_index_3)

    IF (ALLOCATED(compressed_index)) THEN
      IF (SIZE(compressed_index,1) /= s_ci) THEN
        DEALLOCATE(compressed_index)
      END IF
    END IF

    IF (.NOT. ALLOCATED(compressed_index)) THEN
      ALLOCATE(compressed_index(1:s_ci))
    END IF
    compressed_index(1:s_ci) = self%compressed_index_3(1:s_ci)
    STATUS%icode = shumlib_success
  ELSE
    STATUS%icode = -1_int64
    STATUS%message = 'Attempted to get unset compressed index 3'
  END IF
CASE DEFAULT
  STATUS%icode = 1_int64
  STATUS%message = 'Attempted to get invalid compressed index'
END SELECT

END FUNCTION get_compressed_index

!-------------------------------------------------------------------------------
! Field accessors
!-------------------------------------------------------------------------------

FUNCTION get_field(self, field_number, field) RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN)  :: self
INTEGER(KIND=INT64), INTENT(IN)    :: field_number
TYPE(shum_field_type)              :: field
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (field_number > self%num_fields) THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0,A,A)') 'Field number ',field_number,            &
         ' does not exist in file ',TRIM(self%filename)
  RETURN
ELSE
  field = self%fields(field_number)
  STATUS%icode = shumlib_success
  STATUS%message = ''
END IF

END FUNCTION get_field

!-------------------------------------------------------------------------------

FUNCTION find_field_indices_in_file(self, found_field_indices,              &
                                    max_returned_fields, stashcode, lbproc, &
                                    fctime, level_code) RESULT(STATUS)

  ! This function takes in a number of optional arguments containing the
  ! criteria to match, viz. stashcode, lbproc, fctime and level_code, and
  ! returns an array of all matching indices that could be used to index a
  ! field via the file handle. For example:
  ! STATUS = um_file%read_field(found_field_indices(1_int64))
  ! STATUS = um_file%get_field(found_field_indices(1_int64), local_field)
  !  where found_field_indices is a list on indices matching the criteria and 
  !  local_field is a field of type shum_field_type. In this case the field
  ! corresponding to the first index of found_field_indices is retrieved. 
  ! An optional argument "max_returned_fields" can be set to limit the number of
  ! indices returned by this function.
  ! Note that FCTIME is a REAL argument which is calculated manually, and
  ! doesn't equate to the LBFC header item; this allows sub-hourly forecast
  ! times to be matched.
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: stashcode
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: lbproc
REAL(KIND=REAL64), OPTIONAL, INTENT(IN)   :: fctime   ! In hours
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: level_code
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: max_returned_fields

! Returned list
INTEGER(KIND=INT64), ALLOCATABLE :: found_field_indices(:)

! Internal variables
TYPE(shum_field_type) :: current_field

! Tolerance for real comparisons
REAL(KIND=REAL64), PARAMETER :: tolerance = 1.0e-6

! Local message string
CHARACTER(LEN=256) :: cmessage

! Loop counters
INTEGER(KIND=INT64) :: i_field

! Matching variables
INTEGER(KIND=INT64) :: num_matching ! Number of matching fields

! List of matching fields by index in file
INTEGER(KIND=INT64) :: matching_fields(self%num_fields)

INTEGER(KIND=INT64)       :: stash, proc, level
REAL(KIND=REAL64)         :: rfctime
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

num_matching = 0
matching_fields = um_imdi

! Loop over fields to find potential matches 
DO i_field = 1, self%num_fields
  STATUS = self%get_field(i_field, current_field)
  IF (STATUS%icode /= shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Failed to retrieve field at index = ', i_field
    RETURN
  END IF

  IF (PRESENT(stashcode)) THEN
    IF (stashcode/=um_imdi) THEN
      STATUS = current_field%get_stashcode(stash)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Failed to get STASH code for field ',   &
                                       i_field
        RETURN
      END IF
      IF (stash /= stashcode) CYCLE
    END IF
  END IF

  IF (PRESENT(lbproc)) THEN
    IF (lbproc/=um_imdi) THEN
      STATUS = current_field%get_lbproc(proc)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Failed to get LBPROC for  field ',      &
                                       i_field
        RETURN
      END IF
      ! change to:
      IF (proc /= lbproc) CYCLE
    END IF
  END IF

  IF (PRESENT(fctime)) THEN
    IF (fctime/=um_rmdi) THEN
      STATUS = current_field%get_real_fctime(rfctime)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Failed to get forecast time code for'   &
                              //' field ', i_field
        RETURN
      END IF
      IF (ABS(rfctime - fctime) > tolerance) CYCLE
    END IF
  END IF

  IF (PRESENT(level_code)) THEN
    IF (level_code/=um_imdi) THEN
      STATUS = current_field%get_level_number(level)
      IF (STATUS%icode /= shumlib_success) THEN
        WRITE(STATUS%message, '(A,I0)') 'Failed to get level number for '        &
                              //' field ', i_field
        RETURN
      END IF
      IF (level /= level_code) CYCLE
    END IF
  END IF

  ! All criteria match - add field index to the list 
  num_matching = num_matching + 1
  matching_fields(num_matching) = i_field
END DO

WRITE(cmessage, '(A,I0,A)') 'Found ', num_matching,                 &
                            ' matching fields'

! Check whether there's a maximum permitted number of fields to return
IF (PRESENT(max_returned_fields)) THEN
  IF (max_returned_fields > 0 .AND. max_returned_fields /= um_imdi) THEN
    IF (num_matching > max_returned_fields) THEN
      WRITE(cmessage, '(A,I0,A,I0,A,I0,A)') 'Found ', num_matching,            &
       ' matching fields; this exceeds the maximum permitted number of '//     &
       'fields (',max_returned_fields,') so only the first ',                  &
      max_returned_fields,' are being loaded and returned'
      num_matching = max_returned_fields
    END IF
  END IF
END IF

IF (num_matching == 0) THEN
  ! No fields found
  STATUS%icode = -1_int64
  STATUS%message = 'No matching fields found'
  RETURN
ELSE
  ALLOCATE(found_field_indices(num_matching))
  ! copy indices
  found_field_indices = matching_fields(1:num_matching)
  STATUS%icode = shumlib_success
END IF

STATUS%message = cmessage
END FUNCTION find_field_indices_in_file

!-------------------------------------------------------------------------------

FUNCTION find_fields_in_file(self, found_fields, max_returned_fields,          &
                             stashcode, lbproc,                                &
                             fctime, level_code) RESULT(STATUS)

  ! This function takes in a number of optional arguments containing the
  ! criteria to match, viz. stashcode, lbproc, fctime and level_code, and
  ! returns an array of all matching fields (with their data loaded, since
  ! upon returning these fields are independent of a file object which contains
  ! the representation of the file on disk, which includes how to load the
  ! data from the file).
  ! To avoid inadvertantly using too much memory, an optional argument
  ! "max_returned_fields" can be set to limit the number of fields returned by
  ! this function and thus the memory usage.
  ! Note that FCTIME is a REAL argument which is calculated manually, and
  ! doesn't equate to the LBFC header item; this allows sub-hourly forecast
  ! times to be matched.
IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT)     :: self
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: stashcode
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: lbproc
REAL(KIND=REAL64), OPTIONAL, INTENT(IN)   :: fctime   ! In hours
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: level_code
INTEGER(KIND=INT64), OPTIONAL, INTENT(IN) :: max_returned_fields

! Returned list
TYPE(shum_field_type), ALLOCATABLE :: found_fields(:)

! Local message string
CHARACTER(LEN=256) :: cmessage

! Loop counters
INTEGER(KIND=INT64) :: j_matching_field

! Matching variables
INTEGER(KIND=INT64) :: num_matching ! Number of matching fields

! List of matching field indices in file
INTEGER(KIND=INT64), ALLOCATABLE :: found_field_indices(:)
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

! Local optionals
INTEGER(KIND=INT64) :: stashcode_local
INTEGER(KIND=INT64) :: lbproc_local
REAL(KIND=REAL64)   :: fctime_local
INTEGER(KIND=INT64) :: level_code_local
INTEGER(KIND=INT64) :: max_returned_fields_local

! Set the local versions of the optional inputs for use in
! a call to find_field_indices_in_file

IF (PRESENT(stashcode)) THEN
  stashcode_local = stashcode
ELSE
  stashcode_local = um_imdi
ENDIF

IF (PRESENT(max_returned_fields)) THEN
  max_returned_fields_local = max_returned_fields
ELSE
  max_returned_fields_local = um_imdi
ENDIF

IF (PRESENT(lbproc)) THEN
  lbproc_local = lbproc
ELSE
  lbproc_local = um_imdi
ENDIF

IF (PRESENT(fctime)) THEN
  fctime_local = fctime
ELSE
  fctime_local = um_rmdi
ENDIF

IF (PRESENT(level_code)) THEN
  level_code_local = level_code
ELSE
  level_code_local = um_imdi
ENDIF

! Get the indices of the fields defined by the supplied input criteria
! mdi is used if the optional argument is missing
STATUS = self%find_field_indices_in_file(found_field_indices,            &
                                         max_returned_fields_local,      &
                                         stashcode_local, lbproc_local,  &
                                         fctime_local, level_code_local)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF
num_matching = size(found_field_indices)

ALLOCATE(found_fields(num_matching))
DO j_matching_field = 1, num_matching
  ! Load the field data
  STATUS = self%read_field(found_field_indices(j_matching_field))
  IF (STATUS%icode /= shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Failed to read field ',                 &
                                    found_field_indices(j_matching_field)
    RETURN
  END IF

  ! Copy the field, including data
  STATUS = self%get_field(found_field_indices(j_matching_field) ,                &
                          found_fields(j_matching_field))
  IF (STATUS%icode /= shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Failed to copy field ',                 &
                                    found_field_indices(j_matching_field)
    RETURN
  END IF

  ! Unload the field data from the file object's version of the field
  STATUS = self%unload_field(found_field_indices(j_matching_field))
  IF (STATUS%icode > shumlib_success) THEN
    WRITE(STATUS%message, '(A,I0)') 'Failed to unload field ',               &
                                    found_field_indices(j_matching_field)
    RETURN
  END IF
END DO
STATUS%icode = shumlib_success
STATUS%message = cmessage

END FUNCTION find_fields_in_file

!-------------------------------------------------------------------------------

FUNCTION find_forecast_time(self, found_fctime, stashcode) RESULT(STATUS)

  ! This function takes a stashcode as input and returns a list of all the times 
  ! associated with that stashcode.   

IMPLICIT NONE

! Arguments
CLASS(shum_file_type), INTENT(IN OUT) :: self
INTEGER(KIND=INT64), INTENT(IN)       :: stashcode

! Returned list
REAL(KIND=REAL64), ALLOCATABLE        :: found_fctime(:)

TYPE(shum_ff_status_type) :: STATUS    ! Return status object

INTEGER :: i_field, num_matching
REAL(REAL64) :: rfctime
INTEGER(KIND=INT64), ALLOCATABLE :: found_field_indices(:)

! Get the indices of the fields defined by the supplied input criteria
! mdi is used if the optional argument is missing
STATUS = self%find_field_indices_in_file(found_field_indices,  &
                                         stashcode=stashcode)
IF (STATUS%icode /= shumlib_success) THEN
  RETURN
END IF
num_matching = size(found_field_indices)

ALLOCATE(found_fctime(num_matching))
! get and copy times
DO i_field=1,num_matching
  STATUS = self%fields(found_field_indices(i_field))%get_real_fctime(rfctime)
  found_fctime(i_field) = rfctime
END DO

END FUNCTION find_forecast_time

!-------------------------------------------------------------------------------
! File manipulation
!-------------------------------------------------------------------------------

FUNCTION set_filename(self, fname) RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT) :: self
CHARACTER(LEN=*) :: fname
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

IF (ALLOCATED(self%filename)) DEALLOCATE(self%filename)
! The following line auto-allocates the CHARACTER array
self%filename = fname
self%file_identifier = um_imdi
STATUS%icode = shumlib_success
END FUNCTION set_filename

!-------------------------------------------------------------------------------

FUNCTION get_filename(self, fname) RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN) :: self
CHARACTER(LEN=*) :: fname
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

! return empty string if filename is not allocated
IF (ALLOCATED(self%filename)) THEN
  fname = self%filename
ELSE
  fname = ''
ENDIF

STATUS%icode = shumlib_success
END FUNCTION get_filename

!-------------------------------------------------------------------------------

FUNCTION copy_headers_from_file_object(self, template_file_object)             &
    RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT) :: self
CLASS(shum_file_type), INTENT(IN OUT) :: template_file_object
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

STATUS = template_file_object%get_fixed_length_header(                         &
                                                    self%fixed_length_header)
IF (STATUS%icode /= shumlib_success) THEN
  STATUS%message = 'Failed to copy fixed-length header'
  RETURN
END IF
STATUS = template_file_object%get_integer_constants(self%integer_constants)
IF (STATUS%icode /= shumlib_success) THEN
  STATUS%message = 'Failed to copy integer constants'
  RETURN
END IF
STATUS = template_file_object%get_real_constants(self%real_constants)
IF (STATUS%icode /= shumlib_success) THEN
  STATUS%message = 'Failed to copy real constants'
  RETURN
END IF

STATUS = template_file_object%get_level_dependent_constants(                   &
                                             self%level_dependent_constants)
IF (STATUS%icode /= shumlib_success) THEN
  STATUS%message = 'Failed to copy level-dependent constants'
  RETURN
END IF

! The remaining components are optional, so we test for positive icode only,
! as a missing component returns -1
STATUS = template_file_object%get_row_dependent_constants(                     &
                                               self%row_dependent_constants)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy row-dependent constants'
  RETURN
END IF

STATUS = template_file_object%get_column_dependent_constants(                  &
                                            self%column_dependent_constants)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy column-dependent constants'
  RETURN
END IF

STATUS = template_file_object%get_additional_parameters(                       &
                                                 self%additional_parameters)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy additional parameters'
  RETURN
END IF

STATUS = template_file_object%get_extra_constants(self%extra_constants)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy extra constants'
  RETURN
END IF

STATUS = template_file_object%get_temp_histfile(self%temp_histfile)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy temporary histfile'
  RETURN
END IF

STATUS = template_file_object%get_compressed_index(1_int64,                    &
                                                    self%compressed_index_1)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy compressed index 1'
  RETURN
END IF

STATUS = template_file_object%get_compressed_index(2_int64,                    &
                                                    self%compressed_index_2)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy compressed index 2'
  RETURN
END IF

STATUS = template_file_object%get_compressed_index(3_int64,                    &
                                                    self%compressed_index_3)
IF (STATUS%icode > shumlib_success) THEN
  STATUS%message = 'Failed to copy compressed index 3'
  RETURN
END IF

STATUS%icode = shumlib_success
STATUS%message = 'Loaded headers from another file'
END FUNCTION copy_headers_from_file_object

!-------------------------------------------------------------------------------
! Field manipulation
!-------------------------------------------------------------------------------

FUNCTION add_field(self, new_field) RESULT(STATUS)
  ! This adds a field to a file, returning the integer position of the field
  ! in the file.
IMPLICIT NONE
CLASS(shum_file_type),  INTENT(IN OUT) :: self
TYPE(shum_field_type)                 :: new_field

! Internal variables
TYPE(shum_field_type), ALLOCATABLE :: tmp_fields(:)

INTEGER(KIND=INT64) :: stash
CHARACTER(LEN=16) :: timestring

TYPE(shum_ff_status_type) :: STATUS    ! Return status object

! Check to see if some fields are present already
IF (ALLOCATED(self%fields)) THEN
  ! Copy the existing fields to a temporary array
  ALLOCATE(tmp_fields(self%num_fields))
  tmp_fields(1:self%num_fields) = self%fields(1:self%num_fields)
  DEALLOCATE(self%fields)

  ! Resize the fields array to make room for the new field
  self%num_fields = self%num_fields + 1
  ALLOCATE(self%fields(self%num_fields))

  ! Copy the existing fields from temporary storage and tidy up
  self%fields(1:self%num_fields-1) = tmp_fields(1:self%num_fields-1)
  DEALLOCATE(tmp_fields)
ELSE
  ! Empty file object
  ALLOCATE(self%fields(1))
  self%num_fields = 1_int64
END IF

! Add the new field
self%fields(self%num_fields) = new_field

! If the field is a land-sea mask, save it's location
STATUS = new_field%get_stashcode(stash)
IF (stash == stash_land_sea_mask) THEN
  self%field_number_land_sea_mask = self%num_fields
END IF

! Return the position in the file the new field was added as
STATUS = new_field%get_timestring(timestring)
STATUS%icode = shumlib_success
WRITE(STATUS%message, '(A,I0,A,A,A,I0)') 'Added new field (STASH ',stash,      &
       ', validity ', timestring, ') in position ', self%num_fields

END FUNCTION add_field

!-------------------------------------------------------------------------------

FUNCTION unload_field(self, field_number) RESULT(STATUS)
IMPLICIT NONE
CLASS(shum_file_type), INTENT(IN OUT) :: self
INTEGER(KIND=INT64), INTENT(IN)      :: field_number
TYPE(shum_ff_status_type) :: STATUS    ! Return status object

  ! Refuse to unload the land-sea mask
IF (field_number == self%field_number_land_sea_mask) THEN
  STATUS%message = 'Refusing to unload land-sea mask'
  STATUS%icode = 0  ! Still report success
ELSE IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
  STATUS%icode = 1_int64
  WRITE(STATUS%message, '(A,I0,A,A)') 'Field number ',field_number,            &
           ' does not exist in file ',TRIM(self%filename)
ELSE
  STATUS = self%fields(field_number)%unload_data()
END IF

END FUNCTION unload_field

!-------------------------------------------------------------------------------
END MODULE f_shum_file_mod
