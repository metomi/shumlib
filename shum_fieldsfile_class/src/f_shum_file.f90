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
USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                        &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL
USE f_shum_fieldsfile_mod, ONLY: f_shum_fixed_length_header_len
USE f_shum_ff_status_mod, ONLY: shum_ff_status_type, SHUMLIB_SUCCESS

IMPLICIT NONE

PRIVATE

! Data types
INTEGER, PARAMETER :: int64  = C_INT64_T
INTEGER, PARAMETER :: int32  = C_INT32_T
INTEGER, PARAMETER :: real64 = C_DOUBLE
INTEGER, PARAMETER :: real32 = C_FLOAT
INTEGER, PARAMETER :: bool   = C_BOOL

! Missing data, real and integer
REAL(KIND=real64),   PARAMETER  :: um_rmdi     = -32768.0*32768.0
INTEGER(KIND=int64), PARAMETER  :: um_imdi     = -32768

! Lookup lengths
INTEGER(KIND=int64), PARAMETER :: len_integer_lookup = 45
INTEGER(KIND=int64), PARAMETER :: len_real_lookup = 19

! Land/sea mask STASH code
INTEGER(KIND=int64), PARAMETER :: stash_land_sea_mask = 30
!-------------------------------------------------------------------------------

TYPE, PUBLIC :: shum_file_type
  PRIVATE
  CHARACTER(LEN=:), ALLOCATABLE            :: filename
  INTEGER(KIND=int64)                      :: file_identifier = um_imdi
  INTEGER(KIND=int64)                      :: fixed_length_header(            &
                           f_shum_fixed_length_header_len) = um_imdi
  INTEGER(KIND=int64), ALLOCATABLE         :: integer_constants(:)
  REAL(KIND=real64), ALLOCATABLE           :: real_constants(:)
  REAL(KIND=real64), ALLOCATABLE           :: level_dependent_constants(:,:)
  REAL(KIND=real64), ALLOCATABLE           :: row_dependent_constants(:,:)
  REAL(KIND=real64), ALLOCATABLE           :: column_dependent_constants(:,:)
  REAL(KIND=real64), ALLOCATABLE           :: additional_parameters(:,:)
  REAL(KIND=real64), ALLOCATABLE           :: extra_constants(:)
  REAL(KIND=real64), ALLOCATABLE           :: temp_histfile(:)
  REAL(KIND=real64), ALLOCATABLE           :: compressed_index_1(:)
  REAL(KIND=real64), ALLOCATABLE           :: compressed_index_2(:)
  REAL(KIND=real64), ALLOCATABLE           :: compressed_index_3(:)

  TYPE(shum_field_type), PUBLIC, ALLOCATABLE :: fields(:)

  ! Land/sea mask location to deal with land-packed fields
  INTEGER(KIND=int64) :: field_number_land_sea_mask = -99

  INTEGER(KIND=int64), PUBLIC              :: num_fields = 0
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
  PROCEDURE :: find_fields_in_file
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
FUNCTION open_file(self, fname, num_lookup, overwrite) RESULT(status)
  USE f_shum_fieldsfile_mod, ONLY:                                            &
    f_shum_open_file,                                                         &
    f_shum_create_file
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)   :: self
  CHARACTER(LEN=*), INTENT(IN) :: fname
  INTEGER(KIND=int64), OPTIONAL :: num_lookup
  LOGICAL(KIND=bool), OPTIONAL :: overwrite
  TYPE(shum_ff_status_type) :: status    ! Return status object
  INTEGER(KIND=int64) :: lookup_size
  LOGICAL :: exists, read_only

  IF (ALLOCATED(self%filename)) DEALLOCATE(self%filename)
  ! The following line auto-allocates the CHARACTER array
  self%filename = fname
  INQUIRE(FILE=fname, EXIST=exists)

  IF(exists) THEN
    IF (PRESENT(overwrite)) THEN
      IF (overwrite) THEN
        read_only = .FALSE.
      ELSE
        read_only = .TRUE.
      END IF
    ELSE
      read_only = .TRUE.
    END IF
  ELSE
    read_only = .FALSE.
  END IF

  IF (read_only) THEN
    ! Open the file using SHUMlib
    status%icode = f_shum_open_file(self%filename,                            &
                              self%file_identifier,                           &
                              status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      RETURN
    END IF
    status%message = 'Loaded existing file: ' // TRIM(fname)
  ELSE
    IF (PRESENT(num_lookup)) THEN
      lookup_size = num_lookup
    ELSE
      lookup_size = 4096_int64
    END IF
    ! Create a new file using SHUMlib; default behaviour is to overwrite
    ! existing files
    status%icode = f_shum_create_file(self%filename,                          &
                               lookup_size,                                   &
                               self%file_identifier,                          &
                               status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      RETURN
    END IF
    status%message = 'Created new file: ' // TRIM(fname)
  END IF

END FUNCTION open_file

!-------------------------------------------------------------------------------

FUNCTION read_header(self) RESULT(status)
  USE f_shum_fieldsfile_mod, ONLY:                                            &
    f_shum_read_fixed_length_header,                                          &
    f_shum_read_integer_constants,                                            &
    f_shum_read_real_constants,                                               &
    f_shum_read_level_dependent_constants,                                    &
    f_shum_read_row_dependent_constants,                                      &
    f_shum_read_column_dependent_constants,                                   &
    f_shum_read_additional_parameters,                                        &
    f_shum_read_extra_constants,                                              &
    f_shum_read_temp_histfile,                                                &
    f_shum_read_compressed_index,                                             &
    f_shum_read_lookup

  USE f_shum_lookup_indices_mod, ONLY:                                        &
      lbrow, lbnpt, lbuser4

  USE f_shum_fixed_length_header_indices_mod, ONLY:                           &
      dataset_type, lookup_dim2, grid_staggering
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)   :: self

  INTEGER(KIND=int64), ALLOCATABLE :: temp_lookup(:,:)
  ! Loop iterators
  INTEGER(KIND=int64)              :: i_field
  INTEGER(KIND=int64)              :: p_lookup

  REAL(KIND=real64) :: temp_lookup_real(len_real_lookup)

  INTEGER(KIND=int64), PARAMETER :: grid_stagger_endgame = 6

  LOGICAL :: is_variable_resolution = .FALSE.

  TYPE(shum_ff_status_type) :: status    ! Return status object

  ! Read in compulsory headers
  status%icode = f_shum_read_fixed_length_header(                             &
                                           self%file_identifier,              &
                                           self%fixed_length_header,          &
                                           status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  IF (self%fixed_length_header(grid_staggering) /= grid_stagger_endgame) THEN
    status%icode = 1_int64
    status%message = 'This software only supports the Endgame grid.'
    RETURN
  END IF


  ! Check this is a instantaneous dump or a fieldsfile - if it's an ancil, obs
  ! mean dump or LBC file, abort
  IF (self%fixed_length_header(dataset_type) /= 1_int64 .AND.                 &
      self%fixed_length_header(dataset_type) /= 3_int64) THEN
    status%icode = 1_int64
    status%message = 'File is not a fieldsfile or instantaneous dump'
  END IF

  status%icode = f_shum_read_integer_constants(                               &
                                          self%file_identifier,               &
                                          self%integer_constants,             &
                                          status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_read_real_constants(                                  &
                                          self%file_identifier,               &
                                          self%real_constants,                &
                                          status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_read_level_dependent_constants(                       &
                                     self%file_identifier,                    &
                                     self%level_dependent_constants,          &
                                     status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  ! Read in the optional headers we care about (for variable resolution)
  ! Note these return negative status if the component isn't found, so this
  ! is technically a "success" criteria; a positive status is a genuine error.
  status%icode = f_shum_read_row_dependent_constants(                         &
                                 self%file_identifier,                        &
                                 self%row_dependent_constants,                &
                                 status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  ELSE IF (status%icode == SHUMLIB_SUCCESS) THEN
    is_variable_resolution = .TRUE.
  END IF

  status%icode = f_shum_read_column_dependent_constants(                      &
                                 self%file_identifier,                        &
                                 self%column_dependent_constants,             &
                                 status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  ELSE IF (status%icode < 0_int64 .AND. is_variable_resolution) THEN
    status%icode = 1_int64
    status%message = 'File has row-dependent constants but not ' //           &
                   'column-dependent constants'
    RETURN
  ELSE IF (status%icode == SHUMLIB_SUCCESS .AND. .NOT.                                &
           is_variable_resolution) THEN
    status%icode = 1_int64
    status%message = 'File has column-dependent constants but not ' //        &
                   'row-dependent constants'
    RETURN
  END IF

  status%icode = f_shum_read_additional_parameters(self%file_identifier,      &
                                            self%additional_parameters,       &
                                            status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_read_extra_constants(self%file_identifier,            &
                                      self%extra_constants,                   &
                                      status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF


  status%icode = f_shum_read_temp_histfile(self%file_identifier,              &
                                    self%temp_histfile,                       &
                                    status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_read_compressed_index(self%file_identifier,           &
                                       self%compressed_index_1,               &
                                       1_int64,                               &
                                       status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF


  status%icode = f_shum_read_compressed_index(self%file_identifier,           &
                                       self%compressed_index_2,               &
                                       2_int64,                               &
                                       status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_read_compressed_index(self%file_identifier,           &
                                       self%compressed_index_3,               &
                                       3_int64,                               &
                                       status%message)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  ! Allocate space for both temporary lookups and final fields
  ALLOCATE(self%fields(self%fixed_length_header(lookup_dim2)))

  ! Read the lookup
  status%icode = f_shum_read_lookup(self%file_identifier,                     &
                              temp_lookup,                                    &
                              status%message)

  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF


  ! Populate the lookup in the field objects
  DO i_field = 1, self%fixed_length_header(lookup_dim2)
    ! Ignore empty fields in the lookup
    IF(temp_lookup(1, i_field) == -99) CYCLE

    ! Copy the real part of the lookup
    DO p_lookup = 1, len_real_lookup
      temp_lookup_real(p_lookup) = TRANSFER(                                  &
          temp_lookup(p_lookup+len_integer_lookup, i_field),                  &
                                        temp_lookup_real(1))
    END DO

    ! This call should set all properties we need from the STASHmaster file
    ! Currently setting a grid code of 1, as a temporary hack
    status = self%fields(i_field)%set_stashmaster_properties(1_int64)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      WRITE(status%message, '(A,I0)') 'Error setting STASHmaster properties ' &
                                  // ' for field ', i_field
      RETURN
    END IF

    status = self%fields(i_field)%set_lookup(temp_lookup(1:len_integer_lookup,&
                                                i_field), temp_lookup_real)

    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      WRITE(status%message, '(A,I0)') 'Error setting lookup for field ',      &
                                      i_field
      RETURN
    END IF

    IF (is_variable_resolution) THEN
      ! This is a variable resolution field
      status = self%fields(i_field)%set_longitudes(                           &
          self%column_dependent_constants(                                    &
                                1:temp_lookup(lbnpt, i_field), 1))

      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Error setting col-dep constants ' // &
                                      'for field ', i_field
        RETURN
      END IF
      status = self%fields(i_field)%set_latitudes(                            &
          self%row_dependent_constants(                                       &
                                1:temp_lookup(lbrow, i_field), 1))
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Error setting row-dep constants ' // &
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
    status = self%read_field(self%field_number_land_sea_mask)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to load land-sea mask'
      RETURN
    END IF
  END IF

  WRITE(status%message, '(A,I0,A,A)') 'Loaded ',self%num_fields,              &
        ' field headers successfully from file ', TRIM(self%filename)
END FUNCTION read_header

!-------------------------------------------------------------------------------

FUNCTION read_field(self, field_number) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY:                                        &
      lbpack, lbuser1, lbrow, lbnpt, lbuser4, bmdi, lblrec
  USE f_shum_fieldsfile_mod, ONLY:                                            &
    f_shum_read_field_data
  USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)      :: self
  INTEGER(KIND=int64), INTENT(IN)  :: field_number

  REAL(KIND=real64),   ALLOCATABLE :: field_data_r64(:)
  INTEGER(KIND=int64), ALLOCATABLE :: field_data_i64(:)
  REAL(KIND=real32),   ALLOCATABLE :: field_data_r32(:)
  INTEGER(KIND=int32), ALLOCATABLE :: field_data_i32(:)
  INTEGER(KIND=int64)              :: rows, cols, tmp_int

  REAL(KIND=real64),   ALLOCATABLE :: tmp_field_data_r64(:,:)
  REAL(KIND=real32),   ALLOCATABLE :: tmp_field_data_r32(:,:)
  INTEGER(KIND=int64), ALLOCATABLE :: tmp_field_data_i64(:,:)
  INTEGER(KIND=int32), ALLOCATABLE :: tmp_field_data_i32(:,:)
  INTEGER(KIND=int64), ALLOCATABLE :: lsm_data(:,:)

  INTEGER(KIND=int64)              :: i_value, j_value, k_count
  INTEGER(KIND=int64)              :: packing, user1, stash, lendisk
  REAL(KIND=real64)                :: mdi

  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A)') 'Field number ',field_number,           &
           ' does not exist in file ',TRIM(self%filename)
    RETURN
  END IF

  IF (self%fixed_length_header(1) == um_imdi) THEN
    ! Header has not been loaded, so automatically load it
    status = self%read_header()
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      ! Error reading header, return control immediately with error
      RETURN
    END IF
  END IF

  status = self%fields(field_number)%get_lookup_by_index(1_int64, tmp_int)
  IF (tmp_int == um_imdi .OR. tmp_int == -99_int64) THEN
    status%icode = 1_int64
    status%message = 'Field does not exist'
  END IF

  status = self%fields(field_number)%get_lookup_by_index(lbrow, rows)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    WRITE(status%message, '(A,I0)') 'Error getting number of rows in field ', &
                                   field_number
    RETURN
  END IF

  status = self%fields(field_number)%get_lookup_by_index(lbnpt, cols)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    WRITE(status%message, '(A,I0)') 'Error getting number of columns in '     &
                                    //'field ', field_number
    RETURN
  END IF

  status = self%fields(field_number)%get_lookup_by_index(lbpack, packing)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    WRITE(status%message, '(A,I0)') 'Error getting packing for field ',       &
                                   field_number
    RETURN
  END IF

  status = self%fields(field_number)%get_lookup_by_index(lbuser1, user1)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    WRITE(status%message, '(A,I0)') 'Error getting data type for field ',     &
                                   field_number
    RETURN
  END IF

  ! Land-packed data
  IF (MOD((packing/100_int64), 10_int64) > 0_int64) THEN
    ! The field will be land-packed
    IF (self%field_number_land_sea_mask < 1_int64) THEN
      status%icode = 1_int64
      status%message = 'Tried to read compressed data but ' //                &
                     'land-sea mask not found'
      RETURN
    END IF
    status = self%fields(self%field_number_land_sea_mask)                     &
                                         %get_lookup_by_index(lbrow, rows)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get number of rows in land-sea mask'
      RETURN
    END IF
    status = self%fields(self%field_number_land_sea_mask)                     &
                                         %get_lookup_by_index(lbnpt, cols)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get number of columns in land-sea mask'
      RETURN
    END IF

    ! Compressed fields have rows=cols=0 by convention, so we need to change
    ! this so the array is allocated to be the correct size
    status = self%fields(field_number)%set_lookup_by_index(lbrow, rows)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to set number of rows for compressed field'
      RETURN
    END IF
    status = self%fields(field_number)%set_lookup_by_index(lbnpt, cols)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to set number of columns for compressed field'
      RETURN
    END IF

    ALLOCATE(lsm_data(cols, rows))
    status = self%fields(self%field_number_land_sea_mask)%get_data(lsm_data)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get land-sea mask for compressed field'
      RETURN
    END IF

    status = self%fields(field_number)%get_lookup_by_index(lblrec, lendisk)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get length on disk for compressed field'
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

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_r64,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    tmp_field_data_r64(i_value, j_value) =                    &
                                                      field_data_r64(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_r64(i_value, j_value) = um_rmdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i64(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_i64,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    tmp_field_data_i64(i_value, j_value) =                    &
                                                      field_data_i64(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_i64(i_value, j_value) = um_imdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
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

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_r32,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    tmp_field_data_r64(i_value, j_value) =                    &
                                                      field_data_r32(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_r64(i_value, j_value) = um_rmdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i32(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_i32,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    tmp_field_data_i64(i_value, j_value) =                    &
                                                      field_data_i32(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_i64(i_value, j_value) = um_imdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF
            END SELECT

          CASE DEFAULT
          ! Can't WGDOS pack this sort of compression
            status%icode = 1_int64
            status%message = 'WGDOS-packed land-compressed field not implemented'
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

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_r64,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    tmp_field_data_r64(i_value, j_value) =                    &
                                                      field_data_r64(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_r64(i_value, j_value) = um_rmdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i64(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_i64,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    tmp_field_data_i64(i_value, j_value) =                    &
                                                      field_data_i64(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_i64(i_value, j_value) = um_imdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
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

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_r32,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    tmp_field_data_r64(i_value, j_value) =                    &
                                                      field_data_r32(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_r64(i_value, j_value) = um_rmdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i32(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status%icode = f_shum_read_field_data(                          &
                                              self%file_identifier,           &
                                              field_number,                   &
                                              field_data_i32,                 &
                                              status%message)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    tmp_field_data_i64(i_value, j_value) =                    &
                                                      field_data_i32(k_count)
                    k_count = k_count + 1
                  ELSE
                    tmp_field_data_i64(i_value, j_value) = um_imdi
                  END IF
                END DO
              END DO

              status = self%fields(field_number)%set_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error setting data for '     &
                                                //'field ', field_number
                RETURN
              END IF
            END SELECT

          CASE DEFAULT
          ! Can't WGDOS pack this sort of compression
            status%icode = 1_int64
            status%message = 'WGDOS-packed sea-compressed field not implemented'
            RETURN
        END SELECT

      CASE DEFAULT
        status%icode = 1_int64
        WRITE(status%message,'(A,I0)') 'Unknown compression type in lbpack ', &
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
          status%icode = f_shum_read_field_data(                              &
                                          self%file_identifier,               &
                                          field_number,                       &
                                          field_data_r64,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF
          ALLOCATE(tmp_field_data_r64(cols, rows))
          tmp_field_data_r64 = RESHAPE(field_data_r64, (/cols, rows/))
          status = self%fields(field_number)%set_data(tmp_field_data_r64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            WRITE(status%message, '(A,I0)') 'Error setting data for field ',  &
                                           field_number
            RETURN
          END IF

        CASE (2_int64, 3_int64)
          ! Integer or Logical - can copy straight into object array
          status%icode = f_shum_read_field_data(                              &
                                          self%file_identifier,               &
                                          field_number,                       &
                                          field_data_i64,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF
          ALLOCATE(tmp_field_data_i64(cols, rows))
          tmp_field_data_i64 = RESHAPE(field_data_i64, (/cols, rows/))
          status = self%fields(field_number)%set_data(tmp_field_data_i64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            WRITE(status%message, '(A,I0)') 'Error setting data for field ',  &
                                           field_number
            RETURN
          END IF
        END SELECT
    CASE (1_int64)
      ! WGDOS - always 32-bit integer
      status%icode = f_shum_read_field_data(self%file_identifier,             &
                                      field_number,                           &
                                      field_data_i32,                         &
                                      status%message)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        RETURN
      END IF

      ! Unpacking, unlike FF, doesn't allocate the array
      ALLOCATE(tmp_field_data_r64(cols, rows))

      status = self%fields(field_number)%get_lookup_by_index(bmdi, mdi)
      ! Call WGDOS unpacking
      status%icode = f_shum_wgdos_unpack(field_data_i32,                      &
                                  mdi,                                        &
                                  tmp_field_data_r64,                         &
                                  status%message)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        RETURN
      END IF
      status = self%fields(field_number)%set_data(tmp_field_data_r64)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Error setting data for field ',      &
                                       field_number
        RETURN
      END IF
    CASE (2_int64)
      ! 32-bit Truncated
      SELECT CASE (user1)
        CASE (1_int64)
          ! Real
          status%icode = f_shum_read_field_data(                              &
                                          self%file_identifier,               &
                                          field_number,                       &
                                          field_data_r32,                     &
                                          status%message)

          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

          ! Promote to 64-bit
          ALLOCATE(tmp_field_data_r64(cols, rows))
          ALLOCATE(tmp_field_data_r32(cols, rows))
          tmp_field_data_r32 = RESHAPE(field_data_r32, (/cols, rows/))
          DO j_value = 1, rows
            DO i_value = 1, cols
              tmp_field_data_r64(i_value,j_value) = tmp_field_data_r32(       &
                                                             i_value,j_value)
            END DO
          END DO
          status = self%fields(field_number)%set_data(tmp_field_data_r64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            WRITE(status%message, '(A,I0)') 'Error setting data for field ',  &
                                           field_number
            RETURN
          END IF

        CASE (2_int64, 3_int64)
          ! Integer or Logical
          status%icode = f_shum_read_field_data(                              &
                                          self%file_identifier,               &
                                          field_number,                       &
                                          field_data_i32,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

          ! Promote to 64-bit
          ALLOCATE(tmp_field_data_i64(cols, rows))
          ALLOCATE(tmp_field_data_i32(cols, rows))
          tmp_field_data_i32 = RESHAPE(field_data_i32, (/cols, rows/))
          DO j_value = 1, rows
            DO i_value = 1, cols
              tmp_field_data_i64(i_value, j_value) = tmp_field_data_i32(      &
                                                              i_value, j_value)
            END DO
          END DO
          status = self%fields(field_number)%set_data(tmp_field_data_i64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            WRITE(status%message, '(A,I0)') 'Error setting data for field ',  &
                                           field_number
            RETURN
          END IF

        END SELECT
    CASE DEFAULT
      status%icode = 1_int64
      status%message = 'Unrecognised packing type'
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

    status = self%fields(field_number)%get_lookup_by_index(lbuser4, stash)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      WRITE(status%message, '(A,I0)') 'Error getting STASH code for field ',  &
                                     field_number
      RETURN
    END IF
    WRITE(status%message, '(A,I0,A,I0,A,I0,A,I0,A)') 'Field ',                &
          field_number, ' (STASH ', stash,                                    &
          ') loaded with ', cols, 'x', rows,' points'
END FUNCTION read_field

!-------------------------------------------------------------------------------

FUNCTION write_header(self) RESULT(status)
  USE f_shum_fieldsfile_mod, ONLY:                                            &
    f_shum_write_fixed_length_header,                                         &
    f_shum_write_integer_constants,                                           &
    f_shum_write_real_constants,                                              &
    f_shum_write_level_dependent_constants,                                   &
    f_shum_write_row_dependent_constants,                                     &
    f_shum_write_column_dependent_constants,                                  &
    f_shum_write_additional_parameters,                                       &
    f_shum_write_extra_constants,                                             &
    f_shum_write_temp_histfile,                                               &
    f_shum_write_compressed_index

  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)      :: self

  TYPE(shum_ff_status_type) :: status    ! Return status object

  status%icode = SHUMLIB_SUCCESS

  status%icode = f_shum_write_fixed_length_header(self%file_identifier,       &
                                           self%fixed_length_header,          &
                                           status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_write_integer_constants(self%file_identifier,         &
                                         self%integer_constants,              &
                                         status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_write_real_constants(self%file_identifier,            &
                                      self%real_constants,                    &
                                      status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status%icode = f_shum_write_level_dependent_constants(self%file_identifier, &
                                              self%level_dependent_constants, &
                                              status%message)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  IF (ALLOCATED(self%row_dependent_constants) .AND.                           &
      ALLOCATED(self%column_dependent_constants)) THEN

    status%icode = f_shum_write_row_dependent_constants(self%file_identifier, &
                                                 self%row_dependent_constants,&
                                                 status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF

    status%icode = f_shum_write_column_dependent_constants(                   &
                                              self%file_identifier,           &
                                              self%column_dependent_constants,&
                                              status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF

  ELSE IF (ALLOCATED(self%row_dependent_constants) .OR.                       &
      ALLOCATED(self%column_dependent_constants)) THEN
    status%icode = 1_int64
    status%message = 'Tried to write a file with only partial ' //            &
                   'variable-resolution headers'
    RETURN
  END IF

  IF (ALLOCATED(self%additional_parameters)) THEN
    status%icode = f_shum_write_additional_parameters(self%file_identifier,   &
                                               self%additional_parameters,    &
                                               status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF


  IF (ALLOCATED(self%extra_constants)) THEN
    status%icode = f_shum_write_extra_constants(self%file_identifier,         &
                                         self%extra_constants,                &
                                         status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF



  IF (ALLOCATED(self%temp_histfile)) THEN
    status%icode = f_shum_write_temp_histfile(self%file_identifier,           &
                                       self%temp_histfile,                    &
                                       status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF


  IF (ALLOCATED(self%compressed_index_1)) THEN
    status%icode = f_shum_write_compressed_index(self%file_identifier,        &
                                          self%compressed_index_1,            &
                                          1_int64,                            &
                                          status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF

  IF (ALLOCATED(self%compressed_index_2)) THEN
    status%icode = f_shum_write_compressed_index(self%file_identifier,        &
                                          self%compressed_index_2,            &
                                          2_int64,                            &
                                          status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF

  IF (ALLOCATED(self%compressed_index_3)) THEN
    status%icode = f_shum_write_compressed_index(self%file_identifier,        &
                                          self%compressed_index_3,            &
                                          3_int64,                            &
                                          status%message)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
     RETURN
    END IF
  END IF

  WRITE(status%message, '(A,A)') 'Written header of file ', TRIM(self%filename)

END FUNCTION write_header

!-------------------------------------------------------------------------------

FUNCTION write_field(self, field_number) RESULT(status)
  USE f_shum_fieldsfile_mod, ONLY:                                            &
          f_shum_write_field_data, f_shum_lookup_dim1_len
  USE f_shum_lookup_indices_mod, ONLY: lbpack, lbuser1, lbrow, lbnpt, lbuser4,&
                                       bacc, bmdi
  USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)      :: self
  INTEGER(KIND=int64), INTENT(IN)  :: field_number
  TYPE(shum_ff_status_type) :: status    ! Return status object

  INTEGER(KIND=int64) :: lookup(f_shum_lookup_dim1_len)
  INTEGER(KIND=int64) :: lookup_int(len_integer_lookup)
  REAL(KIND=real64)   :: lookup_real(len_real_lookup)

  INTEGER(KIND=int64) :: i_lookup
  INTEGER(KIND=int64) :: i_value, j_value, k_count

  REAL(KIND=real64),   ALLOCATABLE :: field_data_r64(:)
  INTEGER(KIND=int64), ALLOCATABLE :: field_data_i64(:)
  REAL(KIND=real32),   ALLOCATABLE :: field_data_r32(:)
  INTEGER(KIND=int32), ALLOCATABLE :: field_data_i32(:)
  INTEGER(KIND=int64)              :: rows, cols

  REAL(KIND=real64),   ALLOCATABLE :: tmp_field_data_r64(:,:)
  REAL(KIND=real32),   ALLOCATABLE :: tmp_field_data_r32(:,:)
  INTEGER(KIND=int64), ALLOCATABLE :: tmp_field_data_i64(:,:)
  INTEGER(KIND=int32), ALLOCATABLE :: tmp_field_data_i32(:,:)
  INTEGER(KIND=int64), ALLOCATABLE :: lsm_data(:,:)

  INTEGER(KIND=int64) :: lendisk
  INTEGER(KIND=int64) :: acc
  CHARACTER(LEN=64) :: packing_name

  status%icode = SHUMLIB_SUCCESS
  IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A,A)') 'Field number ',field_number,         &
           ' does not exist in file ',TRIM(self%filename)
    RETURN
  END IF

  ! Get the lookup
  status = self%fields(field_number)%get_lookup(lookup_int, lookup_real)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    WRITE(status%message, '(A,I0)') 'Failed to get lookup for field ',        &
                                  field_number
    RETURN
  END IF
  lookup(1:len_integer_lookup) = lookup_int(1:len_integer_lookup)

  ! Convert real part of lookup to integer
  DO i_lookup = 1, len_real_lookup
    lookup(i_lookup + len_integer_lookup) = TRANSFER(lookup_real(i_lookup),   &
                                                     lookup(1_int64))
  END DO

  rows = lookup_int(lbrow)
  cols = lookup_int(lbnpt)

  ! Land-packed data
  IF (MOD((lookup_int(lbpack)/100_int64), 10_int64) > 0_int64) THEN
    ! The field will be land/sea-compressed
    IF (self%field_number_land_sea_mask < 1_int64) THEN
      status%icode = 1_int64
      status%message = 'Tried to read compressed data but ' //                &
                     'land-sea mask not found'
      RETURN
    END IF
    status = self%fields(self%field_number_land_sea_mask)                     &
                                         %get_lookup_by_index(lbrow, rows)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get number of rows in land-sea mask'
      RETURN
    END IF
    status = self%fields(self%field_number_land_sea_mask)                     &
                                         %get_lookup_by_index(lbnpt, cols)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get number of columns in land-sea mask'
      RETURN
    END IF

    ALLOCATE(lsm_data(cols, rows))
    status = self%fields(self%field_number_land_sea_mask)%get_data(lsm_data)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to get land-sea mask for compressed field'
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

              status = self%fields(field_number)%get_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    field_data_r64(k_count) =                                 &
                                          tmp_field_data_r64(i_value, j_value)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_r64,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF


            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i64(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status = self%fields(field_number)%get_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    field_data_i64(k_count) =                                 &
                                          tmp_field_data_i64(i_value, j_value)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_i64,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
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

              status = self%fields(field_number)%get_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    field_data_r32(k_count) =                                  &
                          REAL(tmp_field_data_r64(i_value, j_value),KIND=real32)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_r32,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF


            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i32(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status = self%fields(field_number)%get_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 1_int64) THEN
                    field_data_i32(k_count) =                                  &
                            INT(tmp_field_data_i64(i_value, j_value),KIND=int32)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_i32,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
            END SELECT
          CASE DEFAULT
          ! Can't WGDOS pack this sort of compression
            status%icode = 1_int64
            status%message = 'WGDOS-packed land-compressed field not ' //     &
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

              status = self%fields(field_number)%get_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    field_data_r64(k_count) =                                 &
                                          tmp_field_data_r64(i_value, j_value)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_r64,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF


            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i64(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status = self%fields(field_number)%get_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    field_data_i64(k_count) =                                 &
                                          tmp_field_data_i64(i_value, j_value)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_i64,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
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

              status = self%fields(field_number)%get_data(tmp_field_data_r64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    field_data_r32(k_count) =                                  &
                          REAL(tmp_field_data_r64(i_value, j_value),KIND=real32)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_r32,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF


            CASE (2_int64, 3_int64)
            ! Integer or logical
              ALLOCATE(field_data_i32(lendisk))
              ALLOCATE(tmp_field_data_i64(cols, rows))

              status = self%fields(field_number)%get_data(tmp_field_data_i64)
              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                WRITE(status%message, '(A,I0)') 'Error getting data for '     &
                                                //'field ', field_number
                RETURN
              END IF

              k_count = 1
              DO j_value = 1, rows
                DO i_value = 1, cols
                  IF (lsm_data(i_value, j_value) == 0_int64) THEN
                    field_data_i32(k_count) =                                  &
                            INT(tmp_field_data_i64(i_value, j_value),KIND=int32)
                    k_count = k_count + 1
                  END IF
                END DO
              END DO

              ! Compressed fields have rows=cols=0 by convention, so we need
              ! to obey this
              lookup(lbrow) = 0_int64
              lookup(lbnpt) = 0_int64

              status%icode = f_shum_write_field_data(self%file_identifier,    &
                                              lookup,                         &
                                              field_data_i32,                 &
                                              status%message)

              IF (status%icode /= SHUMLIB_SUCCESS) THEN
                RETURN
              END IF
            END SELECT
          CASE DEFAULT
          ! Can't WGDOS pack this sort of compression
            status%icode = 1_int64
            status%message = 'WGDOS-packed sea-compressed field not '//       &
                             'implemented'
            RETURN
        END SELECT

      CASE DEFAULT
        status%icode = 1_int64
        WRITE(status%message, '(A,I0)') 'Unknown compression type in lbpack ',&
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
          status = self%fields(field_number)%get_data(tmp_field_data_r64)

          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            status%message = 'Failed to get real data'
            RETURN
          END IF
          ALLOCATE(field_data_r64(rows*cols))
          field_data_r64 = RESHAPE(tmp_field_data_r64, (/cols * rows/))

          status%icode = f_shum_write_field_data(self%file_identifier,        &
                                          lookup,                             &
                                          field_data_r64,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

        CASE (2_int64, 3_int64)
          ! Integer or Logical

          ALLOCATE(tmp_field_data_i64(cols, rows))
          status = self%fields(field_number)%get_data(tmp_field_data_i64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            status%message = 'Failed to get integer data'
            RETURN
          END IF
          ALLOCATE(field_data_i64(rows*cols))
          field_data_i64 = RESHAPE(tmp_field_data_i64, (/rows * cols/))

          status%icode = f_shum_write_field_data(self%file_identifier,        &
                                          lookup,                             &
                                          field_data_i64,                     &
                                          status%message)

          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

        END SELECT
    CASE (1_int64)
      ! WGDOS packed
        ALLOCATE(tmp_field_data_r64(cols, rows))
        status = self%fields(field_number)%get_data(tmp_field_data_r64)
        IF (status%icode /= SHUMLIB_SUCCESS) THEN
          status%message = 'Unable to get real data'
          RETURN
        END IF

        ! For some reason the packing expects BACC to be an integer
        acc = INT(lookup_real(bacc-len_integer_lookup), int64)
        WRITE(packing_name, '(A,I0)') 'WGDOS-packed: ',acc
        status%icode = f_shum_wgdos_pack(tmp_field_data_r64,                  &
                                  acc,                                        &
                                  lookup_real(bmdi-len_integer_lookup),       &
                                  field_data_i32,                             &
                                  status%message)
        IF (status%icode /= SHUMLIB_SUCCESS) THEN
          RETURN
        END IF

        status%icode = f_shum_write_field_data(self%file_identifier,          &
                                        lookup,                               &
                                        field_data_i32,                       &
                                        status%message)

        IF (status%icode /= SHUMLIB_SUCCESS) THEN
          RETURN
        END IF

    CASE (2_int64)
      ! 32-bit Truncated
      packing_name = '32-bit truncated'
      SELECT CASE (lookup_int(lbuser1))
        CASE (1_int64)
          ! Real
          ALLOCATE(tmp_field_data_r64(cols, rows))
          status = self%fields(field_number)%get_data(tmp_field_data_r64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            status%message = 'Unable to get real data'
            RETURN
          END IF
          ALLOCATE(tmp_field_data_r32(cols, rows))
          ALLOCATE(field_data_r32(rows*cols))
          DO j_value = 1, rows
            DO i_value = 1, cols
              tmp_field_data_r32(i_value,j_value) = REAL(tmp_field_data_r64(  &
                                                 i_value,j_value), real32)
            END DO
          END DO

          field_data_r32 = RESHAPE(tmp_field_data_r32, (/rows*cols/))
          status%icode = f_shum_write_field_data(self%file_identifier,        &
                                          lookup,                             &
                                          field_data_r32,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

        CASE (2_int64, 3_int64)
          ! Integer or Logical
          ALLOCATE(tmp_field_data_i64(cols, rows))
          status = self%fields(field_number)%get_data(tmp_field_data_i64)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            status%message = 'Unable to get integer data'
            RETURN
          END IF

          ALLOCATE(tmp_field_data_i32(cols, rows))
          ALLOCATE(field_data_i32(rows*cols))

          DO j_value = 1, rows
            DO i_value = 1, cols
              tmp_field_data_i32(i_value,j_value) = INT(tmp_field_data_i64(   &
                                                     i_value,j_value), int32)
            END DO
          END DO
          field_data_i32 = RESHAPE(tmp_field_data_i32, (/rows*cols/))
          status%icode = f_shum_write_field_data(self%file_identifier,        &
                                          lookup,                             &
                                          field_data_i32,                     &
                                          status%message)
          IF (status%icode /= SHUMLIB_SUCCESS) THEN
            RETURN
          END IF

        END SELECT
    CASE DEFAULT
      status%icode = 1_int64
      status%message = 'Unrecognised packing type'
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

    WRITE(status%message, '(A,I0,A,I0,A,I0,A,I0,A,A,A)') 'Field ',            &
          field_number, ' (STASH ', lookup_int(lbuser4),                      &
          ') written with ', cols, 'x', rows,' points (', TRIM(packing_name), &
          ')'

END FUNCTION write_field

!-------------------------------------------------------------------------------

FUNCTION close_file(self) RESULT(status)
  USE f_shum_fieldsfile_mod, ONLY:                                            &
    f_shum_close_file
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT) :: self
  TYPE(shum_ff_status_type) :: status    ! Return status object

  status%icode = f_shum_close_file(self%file_identifier, status%message)

END FUNCTION close_file

!-------------------------------------------------------------------------------
! Header Accessors
!-------------------------------------------------------------------------------

FUNCTION set_fixed_length_header(self, fixed_length_header) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64), INTENT(IN) :: fixed_length_header(                     &
                                                f_shum_fixed_length_header_len)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%fixed_length_header = fixed_length_header
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_fixed_length_header

!-------------------------------------------------------------------------------

FUNCTION get_fixed_length_header(self, fixed_length_header) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64) :: fixed_length_header(f_shum_fixed_length_header_len)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  fixed_length_header = self%fixed_length_header
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_fixed_length_header

!-------------------------------------------------------------------------------

FUNCTION set_fixed_length_header_by_index(self, num_index, value_to_set)      &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64) :: num_index, value_to_set
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (num_index < 1_int64 .OR. num_index > f_shum_fixed_length_header_len)    &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range:', num_index
  ELSE
    self%fixed_length_header(num_index) = value_to_set
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION set_fixed_length_header_by_index

!-------------------------------------------------------------------------------

FUNCTION get_fixed_length_header_by_index(self, num_index, value_to_get)      &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64) :: num_index, value_to_get
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (num_index < 1_int64 .OR. num_index > f_shum_fixed_length_header_len)    &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range:', num_index
  ELSE
    value_to_get = self%fixed_length_header(num_index)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION get_fixed_length_header_by_index

!-------------------------------------------------------------------------------

FUNCTION set_integer_constants(self, integer_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64), INTENT(IN) :: integer_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%integer_constants = integer_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_integer_constants

!-------------------------------------------------------------------------------

FUNCTION get_integer_constants(self, integer_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64), ALLOCATABLE :: integer_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%integer_constants)) THEN
    integer_constants = self%integer_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'Attempted to get unset integer constants'
  END IF

END FUNCTION get_integer_constants

!-------------------------------------------------------------------------------

FUNCTION set_integer_constants_by_index(self, num_index, value_to_set)        &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64) :: num_index, value_to_set
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (.NOT. ALLOCATED(self%integer_constants)) THEN
    status%icode = 1_int64
    status%message = 'Integer constants not allocated'
  ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%integer_constants)) &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range: ', num_index
  ELSE
    self%integer_constants(num_index) = value_to_set
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION set_integer_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION get_integer_constants_by_index(self, num_index, value_to_get)        &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64) :: num_index, value_to_get
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (.NOT. ALLOCATED(self%integer_constants)) THEN
    status%icode = 1_int64
    status%message = 'Integer constants not allocated'
  ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%integer_constants)) &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range: ', num_index
  ELSE
    value_to_get = self%integer_constants(num_index)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION get_integer_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION set_real_constants(self, real_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: real_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%real_constants = real_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_real_constants

!-------------------------------------------------------------------------------

FUNCTION get_real_constants(self, real_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: real_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%real_constants)) THEN
    real_constants = self%real_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'Attempted to get unset real constants'
  END IF

END FUNCTION get_real_constants

!-------------------------------------------------------------------------------

FUNCTION set_real_constants_by_index(self, num_index, value_to_set)           &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64) :: value_to_set
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (.NOT. ALLOCATED(self%real_constants)) THEN
    status%icode = 1_int64
    status%message = 'Real constants not allocated'
  ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%real_constants))    &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range: ', num_index
  ELSE
    self%real_constants(num_index) = value_to_set
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION set_real_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION get_real_constants_by_index(self, num_index, value_to_get)           &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64) :: value_to_get
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (.NOT. ALLOCATED(self%real_constants)) THEN
    status%icode = 1_int64
    status%message = 'Real constants not allocated'
  ELSE IF (num_index < 1_int64 .OR. num_index > SIZE(self%real_constants))    &
      THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0)') 'Index out of range: ', num_index
  ELSE
    value_to_get = self%real_constants(num_index)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION get_real_constants_by_index

!-------------------------------------------------------------------------------

FUNCTION set_level_dependent_constants(self, level_dependent_constants)       &
     RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: level_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%level_dependent_constants = level_dependent_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_level_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_level_dependent_constants(self, level_dependent_constants)       &
     RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: level_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%level_dependent_constants)) THEN
    level_dependent_constants = self%level_dependent_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'Attempted to get unset level-dependent constants'
  END IF

END FUNCTION get_level_dependent_constants
!-------------------------------------------------------------------------------

FUNCTION set_row_dependent_constants(self, row_dependent_constants)           &
      RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: row_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%row_dependent_constants = row_dependent_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_row_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_row_dependent_constants(self, row_dependent_constants)           &
     RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: row_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%row_dependent_constants)) THEN
    row_dependent_constants = self%row_dependent_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = -1_int64
    status%message = 'Attempted to get unset row-dependent constants'
  END IF

END FUNCTION get_row_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION set_column_dependent_constants(self, column_dependent_constants)     &
     RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: column_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%column_dependent_constants = column_dependent_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_column_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION get_column_dependent_constants(self, column_dependent_constants)     &
     RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: column_dependent_constants(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%column_dependent_constants)) THEN
    column_dependent_constants = self%column_dependent_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = -1_int64
    status%message = 'Attempted to get unset column-dependent constants'
  END IF

END FUNCTION get_column_dependent_constants

!-------------------------------------------------------------------------------

FUNCTION set_additional_parameters(self, additional_parameters) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: additional_parameters(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%additional_parameters = additional_parameters
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_additional_parameters

!-------------------------------------------------------------------------------

FUNCTION get_additional_parameters(self, additional_parameters) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: additional_parameters(:,:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%additional_parameters)) THEN
    additional_parameters = self%additional_parameters
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = -1_int64
    status%message = 'Attempted to get unset additional parameters'
  END IF

END FUNCTION get_additional_parameters

!-------------------------------------------------------------------------------

FUNCTION set_extra_constants(self, extra_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: extra_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%extra_constants = extra_constants
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_extra_constants

!-------------------------------------------------------------------------------

FUNCTION get_extra_constants(self, extra_constants) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: extra_constants(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%extra_constants)) THEN
    extra_constants = self%extra_constants
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = -1_int64
    status%message = 'Attempted to get unset extra constants'
  END IF

END FUNCTION get_extra_constants

!-------------------------------------------------------------------------------

FUNCTION set_temp_histfile(self, temp_histfile) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  REAL(KIND=real64), INTENT(IN) :: temp_histfile(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  self%temp_histfile = temp_histfile
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_temp_histfile

!-------------------------------------------------------------------------------

FUNCTION get_temp_histfile(self, temp_histfile) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  REAL(KIND=real64), ALLOCATABLE :: temp_histfile(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%temp_histfile)) THEN
    temp_histfile = self%temp_histfile
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = -1_int64
    status%message = 'Attempted to get unset temp histfile'
  END IF

END FUNCTION get_temp_histfile

!-------------------------------------------------------------------------------

FUNCTION set_compressed_index(self, num_index, compressed_index) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(INOUT)     :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64), INTENT(IN) :: compressed_index(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  status%message = ''
  SELECT CASE(num_index)
    CASE(1_int64)
      self%compressed_index_1 = compressed_index
      status%icode = SHUMLIB_SUCCESS
    CASE(2_int64)
      self%compressed_index_2 = compressed_index
      status%icode = SHUMLIB_SUCCESS
    CASE(3_int64)
      self%compressed_index_3 = compressed_index
      status%icode = SHUMLIB_SUCCESS
    CASE DEFAULT
      status%icode = 1_int64
      status%message = 'Attempted to set invalid compressed index'
  END SELECT

END FUNCTION set_compressed_index

!-------------------------------------------------------------------------------

FUNCTION get_compressed_index(self, num_index, compressed_index) RESULT(status)
  IMPLICIT NONE

  ! Arguments
  CLASS(shum_file_type), INTENT(IN)     :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64), ALLOCATABLE :: compressed_index(:)
  TYPE(shum_ff_status_type) :: status    ! Return status object

  status%message = ''
  SELECT CASE(num_index)
    CASE(1_int64)
      IF (ALLOCATED(self%compressed_index_1)) THEN
        compressed_index = self%compressed_index_1
        status%icode = SHUMLIB_SUCCESS
      ELSE
        status%icode = -1_int64
        status%message = 'Attempted to get unset compressed index 1'
      END IF
    CASE(2_int64)
      IF (ALLOCATED(self%compressed_index_2)) THEN
        compressed_index = self%compressed_index_2
        status%icode = SHUMLIB_SUCCESS
      ELSE
        status%icode = -1_int64
        status%message = 'Attempted to get unset compressed index 2'
      END IF
    CASE(3_int64)
      IF (ALLOCATED(self%compressed_index_3)) THEN
        compressed_index = self%compressed_index_3
        status%icode = SHUMLIB_SUCCESS
      ELSE
        status%icode = -1_int64
        status%message = 'Attempted to get unset compressed index 3'
      END IF
    CASE DEFAULT
      status%icode = 1_int64
      status%message = 'Attempted to get invalid compressed index'
  END SELECT

END FUNCTION get_compressed_index

!-------------------------------------------------------------------------------
! Field accessors
!-------------------------------------------------------------------------------

FUNCTION get_field(self, field_number, field) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(IN)  :: self
  INTEGER(KIND=int64), INTENT(IN)    :: field_number
  TYPE(shum_field_type)              :: field
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (field_number > self%num_fields) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A,A)') 'Field number ',field_number,         &
           ' does not exist in file ',TRIM(self%filename)
    RETURN
  ELSE
    field = self%fields(field_number)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION get_field

!-------------------------------------------------------------------------------

FUNCTION find_fields_in_file(self, found_fields, max_returned_fields,         &
                             stashcode, lbproc,                               &
                             fctime, level_code) RESULT(status)

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
  CLASS(shum_file_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64), OPTIONAL :: stashcode
  INTEGER(KIND=int64), OPTIONAL :: lbproc
  REAL(KIND=real64), OPTIONAL :: fctime   ! In hours
  INTEGER(KIND=int64), OPTIONAL :: level_code
  INTEGER(KIND=int64), OPTIONAL :: max_returned_fields

  ! Returned list
  TYPE(shum_field_type), ALLOCATABLE :: found_fields(:)

  ! Internal variables
  TYPE(shum_field_type) :: current_field

  ! Tolerance for real comparisons
  REAL(KIND=real64), PARAMETER :: tolerance = 1.0e-6

  ! Local message string
  CHARACTER(LEN=256) :: cmessage

  ! Loop counters
  INTEGER(KIND=int64)      :: i_field, j_matching_field

  ! Matching variables
  INTEGER(KIND=int64)      :: num_matching ! Number of matching fields
  LOGICAL :: matches

  ! List of matching fields by index in file
  INTEGER(KIND=int64) :: matching_fields(self%num_fields)

  INTEGER(KIND=int64) :: stash, proc, level
  REAL(KIND=real64)   :: rfctime
  TYPE(shum_ff_status_type) :: status    ! Return status object

  num_matching = 0
  matching_fields = um_imdi

  ! Loop over fields to find potential matches
  DO i_field = 1, self%num_fields
    matches = .TRUE.
    status = self%get_field(i_field, current_field)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      WRITE(status%message, '(A,I0)') 'Failed to analyse field ', i_field
      RETURN
    END IF

    IF (PRESENT(stashcode)) THEN
      status = current_field%get_stashcode(stash)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to get STASH code for field ',&
                                       i_field
        RETURN
      END IF
      IF (stash /= stashcode) THEN
        matches = .FALSE.
      END IF
    END IF

    IF (PRESENT(lbproc)) THEN
      status = current_field%get_lbproc(proc)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to get LBPROC for  field ',   &
                                       i_field
        RETURN
      END IF
      IF (proc /= lbproc) THEN
        matches = .FALSE.
      END IF
    END IF

    IF (PRESENT(fctime)) THEN
      status = current_field%get_real_fctime(rfctime)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to get forecast time code for'&
                              //' field ', i_field
        RETURN
      END IF
      IF (rfctime > fctime*(1+tolerance) .OR.                                 &
          rfctime < fctime*(1-tolerance)) THEN
        matches = .FALSE.
      END IF
    END IF

    IF (PRESENT(level_code)) THEN
      status = current_field%get_level_number(level)
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to get level number for '     &
                              //' field ', i_field
        RETURN
      END IF
      IF (level /= level_code) THEN
        matches = .FALSE.
      END IF
    END IF

    IF (matches) THEN
      num_matching = num_matching + 1
      matching_fields(num_matching) = i_field
    END IF
  END DO

  WRITE(cmessage, '(A,I0,A)') 'Found and loaded ', num_matching,              &
                              ' matching fields'

  ! Check whether there's a maximum permitted number of fields to return
  IF (PRESENT(max_returned_fields)) THEN
    IF (max_returned_fields > 0) THEN
      IF (num_matching > max_returned_fields) THEN
        WRITE(cmessage, '(A,I0,A,I0,A,I0,A)') 'Found ', num_matching,         &
         ' matching fields; this exceeds the maximum permitted number of '//  &
         'fields (',max_returned_fields,') so only the first ',               &
        max_returned_fields,' are being loaded and returned'
        num_matching = max_returned_fields
      END IF
    END IF
  END IF

  IF (num_matching == 0) THEN
    ! No fields found
    ALLOCATE(found_fields(1))
    status%icode = -1_int64
    status%message = 'No matching fields found'
    RETURN
  ELSE
    ALLOCATE(found_fields(num_matching))
    DO j_matching_field = 1, num_matching
      ! Load the field data
      status = self%read_field(matching_fields(j_matching_field))
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to read field ',              &
                                       matching_fields(j_matching_field)
        RETURN
      END IF

      ! Copy the field, including data
      status = self%get_field(matching_fields(j_matching_field) ,             &
                             found_fields(j_matching_field))
      IF (status%icode /= SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to copy field ',              &
                                       matching_fields(j_matching_field)
        RETURN
      END IF

      ! Unload the field data from the file object's version of the field
      status = self%unload_field(matching_fields(j_matching_field))
      IF (status%icode > SHUMLIB_SUCCESS) THEN
        WRITE(status%message, '(A,I0)') 'Failed to unload field ',            &
                                       matching_fields(j_matching_field)
        RETURN
      END IF
    END DO
    status%icode = SHUMLIB_SUCCESS
  END IF

  status%message = cmessage
END FUNCTION find_fields_in_file

!-------------------------------------------------------------------------------
! File manipulation
!-------------------------------------------------------------------------------

FUNCTION set_filename(self, fname) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT) :: self
  CHARACTER(LEN=*) :: fname
  TYPE(shum_ff_status_type) :: status    ! Return status object

  IF (ALLOCATED(self%filename)) DEALLOCATE(self%filename)
  ! The following line auto-allocates the CHARACTER array
  self%filename = fname
  self%file_identifier = um_imdi
  status%icode = SHUMLIB_SUCCESS
END FUNCTION set_filename

!-------------------------------------------------------------------------------

FUNCTION get_filename(self, fname) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(IN) :: self
  CHARACTER(LEN=*) :: fname
  TYPE(shum_ff_status_type) :: status    ! Return status object

  fname = self%filename
  status%icode = SHUMLIB_SUCCESS
END FUNCTION get_filename

!-------------------------------------------------------------------------------

FUNCTION copy_headers_from_file_object(self, template_file_object)            &
    RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT) :: self
  CLASS(shum_file_type), INTENT(INOUT) :: template_file_object
  TYPE(shum_ff_status_type) :: status    ! Return status object

  status = template_file_object%get_fixed_length_header(                      &
                                                      self%fixed_length_header)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy fixed-length header'
    RETURN
  END IF
  status = template_file_object%get_integer_constants(self%integer_constants)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy integer constants'
    RETURN
  END IF
  status = template_file_object%get_real_constants(self%real_constants)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy real constants'
    RETURN
  END IF

  status = template_file_object%get_level_dependent_constants(                &
                                               self%level_dependent_constants)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy level-dependent constants'
    RETURN
  END IF

  ! The remaining components are optional, so we test for positive icode only,
  ! as a missing component returns -1
  status = template_file_object%get_row_dependent_constants(                  &
                                                 self%row_dependent_constants)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy row-dependent constants'
    RETURN
  END IF

  status = template_file_object%get_column_dependent_constants(               &
                                              self%column_dependent_constants)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy column-dependent constants'
    RETURN
  END IF

  status = template_file_object%get_additional_parameters(                    &
                                                   self%additional_parameters)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy additional parameters'
    RETURN
  END IF

  status = template_file_object%get_extra_constants(self%extra_constants)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy extra constants'
    RETURN
  END IF

  status = template_file_object%get_temp_histfile(self%temp_histfile)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy temporary histfile'
    RETURN
  END IF

  status = template_file_object%get_compressed_index(1_int64,                 &
                                                      self%compressed_index_1)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy compressed index 1'
    RETURN
  END IF

  status = template_file_object%get_compressed_index(2_int64,                 &
                                                      self%compressed_index_2)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy compressed index 2'
    RETURN
  END IF

  status = template_file_object%get_compressed_index(3_int64,                 &
                                                      self%compressed_index_3)
  IF (status%icode > SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to copy compressed index 3'
    RETURN
  END IF

  status%icode = SHUMLIB_SUCCESS
  status%message = 'Loaded headers from another file'
END FUNCTION copy_headers_from_file_object

!-------------------------------------------------------------------------------
! Field manipulation
!-------------------------------------------------------------------------------

FUNCTION add_field(self, new_field) RESULT(status)
  ! This adds a field to a file, returning the integer position of the field
  ! in the file.
  IMPLICIT NONE
  CLASS(shum_file_type),  INTENT(INOUT) :: self
  TYPE(shum_field_type)                 :: new_field

  ! Internal variables
  TYPE(shum_field_type), ALLOCATABLE :: tmp_fields(:)

  INTEGER(KIND=int64) :: stash
  CHARACTER(LEN=16) :: timestring

  TYPE(shum_ff_status_type) :: status    ! Return status object

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
  status = new_field%get_stashcode(stash)
  IF (stash == stash_land_sea_mask) THEN
    self%field_number_land_sea_mask = self%num_fields
  END IF

  ! Return the position in the file the new field was added as
  status = new_field%get_timestring(timestring)
  status%icode = SHUMLIB_SUCCESS
  WRITE(status%message, '(A,I0,A,A,A,I0)') 'Added new field (STASH ',stash,   &
         ', validity ', timestring, ') in position ', self%num_fields

END FUNCTION add_field

!-------------------------------------------------------------------------------

FUNCTION unload_field(self, field_number) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_file_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64), INTENT(IN)      :: field_number
  TYPE(shum_ff_status_type) :: status    ! Return status object

  ! Refuse to unload the land-sea mask
  IF (field_number == self%field_number_land_sea_mask) THEN
    status%message = 'Refusing to unload land-sea mask'
    status%icode = 0  ! Still report success
  ELSE IF (field_number < 0_int64 .OR. field_number > self%num_fields) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A,A)') 'Field number ',field_number,         &
           ' does not exist in file ',TRIM(self%filename)
  ELSE
    status = self%fields(field_number)%unload_data()
  END IF

END FUNCTION unload_field

!-------------------------------------------------------------------------------
END MODULE f_shum_file_mod
