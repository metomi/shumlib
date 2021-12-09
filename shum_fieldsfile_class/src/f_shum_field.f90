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
MODULE f_shum_field_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                        &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE
USE f_shum_ff_status_mod, ONLY: shum_ff_status_type, SHUMLIB_SUCCESS

IMPLICIT NONE

PRIVATE

! Data types
INTEGER, PARAMETER :: int64  = C_INT64_T
INTEGER, PARAMETER :: real64 = C_DOUBLE

! Missing data, real and integer
REAL(KIND=real64),   PARAMETER  :: um_rmdi     = -32768.0*32768.0
INTEGER(KIND=int64), PARAMETER  :: um_imdi     = -32768

! Lookup lengths
INTEGER(KIND=int64), PARAMETER :: len_integer_lookup = 45
INTEGER(KIND=int64), PARAMETER :: len_real_lookup = 19

!-------------------------------------------------------------------------------

TYPE, PUBLIC :: shum_field_type
  PRIVATE
  INTEGER(KIND=int64) :: lookup_int(len_integer_lookup) = um_imdi
  REAL(KIND=real64)   :: lookup_real(len_real_lookup) = um_rmdi
  INTEGER(KIND=int64), PUBLIC,  ALLOCATABLE :: idata(:,:)
  REAL(KIND=real64), PUBLIC, ALLOCATABLE    :: rdata(:,:)
  REAL(KIND=real64), ALLOCATABLE       :: latitudes(:)
  REAL(KIND=real64), ALLOCATABLE       :: longitudes(:)
  REAL(KIND=real64)                    :: fctime_real
  INTEGER(KIND=int64)                  :: grid_type_code
CONTAINS
! Generics, for accessors which set/get both real and integer data
  GENERIC :: set_lookup_by_index  => set_int_lookup_by_index,                 &
                                     set_real_lookup_by_index
  GENERIC :: get_lookup_by_index  => get_int_lookup_by_index,                 &
                                     get_real_lookup_by_index
  GENERIC :: set_data             => set_rdata,                               &
                                     set_idata
  GENERIC :: get_data             => get_rdata,                               &
                                     get_idata
  GENERIC :: get_data_by_location => get_rdata_by_location,                   &
                                     get_idata_by_location
! Lookup accessors
  PROCEDURE :: set_lookup
  PROCEDURE :: get_lookup
! Lookup accessors - specific methods for GENERICs above, all PRIVATE
  PROCEDURE, PRIVATE :: set_int_lookup_by_index
  PROCEDURE, PRIVATE :: get_int_lookup_by_index
  PROCEDURE, PRIVATE :: set_real_lookup_by_index
  PROCEDURE, PRIVATE :: get_real_lookup_by_index
! Convenient named-lookup accessors
  PROCEDURE :: get_stashcode
  PROCEDURE :: get_timestring
  PROCEDURE :: get_level_number
  PROCEDURE :: get_level_eta
  PROCEDURE :: get_real_fctime
  PROCEDURE :: get_lbproc
! STASHmaster methods
  PROCEDURE :: set_stashmaster_properties
! Grid accessors
  PROCEDURE :: set_longitudes
  PROCEDURE :: get_longitudes
  PROCEDURE :: set_latitudes
  PROCEDURE :: get_latitudes
  PROCEDURE :: get_coords
  PROCEDURE :: get_pole_location
! Data accessors - specific methods for GENERICs above, all PRIVATE
  PROCEDURE, PRIVATE :: set_rdata
  PROCEDURE, PRIVATE :: get_rdata
  PROCEDURE, PRIVATE :: get_rdata_by_location
  PROCEDURE, PRIVATE :: set_idata
  PROCEDURE, PRIVATE :: get_idata
  PROCEDURE, PRIVATE :: get_idata_by_location
! Utility methods
  PROCEDURE :: unload_data
! Private methods
  PROCEDURE, PRIVATE :: generate_fixed_grid
END TYPE shum_field_type

!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
! Lookup accessors
!-------------------------------------------------------------------------------

FUNCTION set_lookup(self, lookup_int, lookup_real) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY:                                        &
            lbyrd, lbmond, lbdatd, lbhrd, lbmind, lbsecd,                     &
            lbyr, lbmon, lbdat, lbhr, lbmin, lbsec, bdx
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64), INTENT(IN)     :: lookup_int(len_integer_lookup)
  REAL(KIND=real64), INTENT(IN)       :: lookup_real(len_real_lookup)

  ! Julian hour variables
  REAL(KIND=real64)   :: julian_hour_data, julian_hour_validity
  INTEGER(KIND=int64) :: yr, mon, dat, hr, minute, sec
  INTEGER(KIND=int64) :: yrd, mond, datd, hrd, mind, secd

  TYPE(shum_ff_status_type) :: status ! Return status object

  self%lookup_int = lookup_int
  self%lookup_real = lookup_real

  status = self%get_int_lookup_by_index(lbyrd, yrd)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbmond, mond)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbdatd, datd)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbhrd, hrd)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbmind, mind)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbsecd, secd)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbyr, yr)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbmon, mon)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbdat, dat)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbhr, hr)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbmin, minute)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  status = self%get_int_lookup_by_index(lbsec, sec)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  julian_hour_data = convert_to_julian(yrd, mond, datd, hrd, mind, secd)
  julian_hour_validity = convert_to_julian(yr, mon, dat, hr, minute, sec)

  self%fctime_real = julian_hour_validity - julian_hour_data

  status%icode = SHUMLIB_SUCCESS
  status%message = ''

  ! If this has a valid value for the grid positions, generate the grid
  ! If it doesn't, this is a variable resolution file and the grid is set
  ! externally by the file object
  IF (self%lookup_real(bdx-len_integer_lookup) /= um_rmdi) THEN
    status = self%generate_fixed_grid()
  END IF
END FUNCTION set_lookup

!-------------------------------------------------------------------------------

FUNCTION get_lookup(self, lookup_int, lookup_real) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64), INTENT(OUT) :: lookup_int(len_integer_lookup)
  REAL(KIND=real64), INTENT(OUT) :: lookup_real(len_real_lookup)
  TYPE(shum_ff_status_type) :: status ! Return status object

  lookup_int = self%lookup_int
  lookup_real = self%lookup_real
  status%icode = SHUMLIB_SUCCESS
  status%message = ''
END FUNCTION get_lookup

!-------------------------------------------------------------------------------

FUNCTION set_int_lookup_by_index(self, num_index, value_to_set) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64) :: value_to_set, num_index
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (num_index > len_integer_lookup .OR. num_index < 1_int64) THEN
    status%icode = 1_int64
    WRITE(status%message,'(A,I0,A)') 'Integer lookup index ',num_index,       &
                                     ' out of range'
  ELSE
    self%lookup_int(num_index) = value_to_set
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION set_int_lookup_by_index

!-------------------------------------------------------------------------------

FUNCTION get_int_lookup_by_index(self, num_index, value_to_get) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: value_to_get, num_index
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (num_index > len_integer_lookup .OR. num_index < 1_int64) THEN
    status%icode = 1_int64
    WRITE(status%message,'(A,I0,A)') 'Integer lookup index ',num_index,       &
                                     ' out of range'
  ELSE
    value_to_get = self%lookup_int(num_index)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION get_int_lookup_by_index

!-------------------------------------------------------------------------------

FUNCTION set_real_lookup_by_index(self, num_index, value_to_set) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64) :: value_to_set
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (num_index > len_integer_lookup + len_real_lookup .OR.                   &
      num_index < 1_int64 + len_integer_lookup) THEN
    status%icode = 1_int64
    WRITE(status%message,'(A,I0,A)') 'Real lookup index ',num_index,          &
                                     ' out of range'
  ELSE
    ! SHUMlib stores parameters containing the index in the 64-word lookup
    ! However, here we've split it into it's integer and real components, so we
    ! deduct the length of the integer lookup to find the position in the real
    ! part
    self%lookup_real(num_index-len_integer_lookup) = value_to_set
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF

END FUNCTION set_real_lookup_by_index

!-------------------------------------------------------------------------------

FUNCTION get_real_lookup_by_index(self, num_index, value_to_get) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: num_index
  REAL(KIND=real64) :: value_to_get
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (num_index > len_integer_lookup + len_real_lookup .OR.                   &
      num_index < 1_int64 + len_integer_lookup) THEN
    status%icode = 1_int64
    WRITE(status%message,'(A,I0,A)') 'Real lookup index ',num_index,          &
                                     ' out of range'
  ELSE 
  ! SHUMlib stores parameters containing the index in the 64-word lookup
  ! However, here we've split it into it's integer and real components, so we
  ! deduct the length of the integer lookup to find the position in the real
  ! part
    value_to_get = self%lookup_real(num_index-len_integer_lookup)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  END IF
END FUNCTION get_real_lookup_by_index

!-------------------------------------------------------------------------------
! Convenient named-lookup accessors
!-------------------------------------------------------------------------------

FUNCTION get_stashcode(self, stashcode) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbuser4
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: stashcode
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (self%lookup_int(lbuser4) /= um_imdi) THEN
    stashcode = self%lookup_int(lbuser4)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    stashcode = um_imdi
    status%icode = 1_int64
    status%message = 'STASH code not defined'
  END IF

END FUNCTION get_stashcode

!-------------------------------------------------------------------------------

FUNCTION get_timestring(self, timestring) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbyr, lbmon, lbdat, lbhr, lbmin, lbsec
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  CHARACTER(LEN=16) :: timestring
  INTEGER(KIND=int64) :: yr, mon, dat, hr, min, sec
  TYPE(shum_ff_status_type) :: status ! Return status object

  status%message = ''
  status = self%get_int_lookup_by_index(lbyr, yr)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF
  status = self%get_int_lookup_by_index(lbmon, mon)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF
  status = self%get_int_lookup_by_index(lbdat, dat)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF
  status = self%get_int_lookup_by_index(lbhr, hr)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF
  status = self%get_int_lookup_by_index(lbmin, min)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF
  status = self%get_int_lookup_by_index(lbsec, sec)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    RETURN
  END IF

  ! This format statement is deliberately using fixed-width integers
  WRITE(timestring, '(I4.4,I2.2,I2.2,A,I2.2,I2.2,I2.2,A)')                    &
               yr, mon, dat, 'T', hr, min, sec, 'Z'
  status%icode = SHUMLIB_SUCCESS
END FUNCTION get_timestring

!-------------------------------------------------------------------------------

FUNCTION get_level_number(self, level_number) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lblev
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: level_number
  TYPE(shum_ff_status_type) :: status ! Return status object

  level_number = self%lookup_int(lblev)
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_level_number

!-------------------------------------------------------------------------------

FUNCTION get_level_eta(self, level_eta) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: blev
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64) :: level_eta
  TYPE(shum_ff_status_type) :: status ! Return status object

  ! SHUMlib stores parameters containing the index in the 64-word lookup
  ! However, here we've split it into it's integer and real components, so we
  ! deduct the length of the integer lookup to find the position in the real
  ! part
  level_eta = self%lookup_real(blev - len_integer_lookup)
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_level_eta

!-------------------------------------------------------------------------------

FUNCTION get_real_fctime(self, real_fctime) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64) :: real_fctime
  TYPE(shum_ff_status_type) :: status ! Return status object

  real_fctime = self%fctime_real
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_real_fctime

!-------------------------------------------------------------------------------

FUNCTION get_lbproc(self, proc) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbproc
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: proc
  TYPE(shum_ff_status_type) :: status ! Return status object

  proc = self%lookup_int(lbproc)
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_lbproc

!-------------------------------------------------------------------------------
! STASHmaster methods
!-------------------------------------------------------------------------------

FUNCTION set_stashmaster_properties(self, grid) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64), INTENT(IN) :: grid
  TYPE(shum_ff_status_type) :: status ! Return status object

  self%grid_type_code = grid
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION set_stashmaster_properties

!-------------------------------------------------------------------------------
! Grid accessors
!-------------------------------------------------------------------------------

FUNCTION set_longitudes(self, longitudes) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbnpt
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  REAL(KIND=real64), INTENT(IN) :: longitudes(self%lookup_int(lbnpt))
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%longitudes)) THEN
    DEALLOCATE(self%longitudes, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      RETURN
    END IF
  END IF

  status%message = ''
  ALLOCATE(self%longitudes(self%lookup_int(lbnpt)), SOURCE=longitudes,        &
           STAT=status%icode)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to allocate longitudes array'
  END IF
END FUNCTION set_longitudes

!-------------------------------------------------------------------------------

FUNCTION get_longitudes(self, longitudes) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64), ALLOCATABLE :: temp_longitudes(:)
  REAL(KIND=real64), ALLOCATABLE :: longitudes(:)
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%longitudes)) THEN
    ALLOCATE(temp_longitudes(SIZE(self%longitudes)))
    temp_longitudes(:) = self%longitudes(:)
    CALL MOVE_ALLOC(temp_longitudes, longitudes)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'Longitudes not set'
  END IF

END FUNCTION get_longitudes

!-------------------------------------------------------------------------------

FUNCTION set_latitudes(self, latitudes) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbrow
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  REAL(KIND=real64), INTENT(IN) :: latitudes(self%lookup_int(lbrow))
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%latitudes)) THEN
    DEALLOCATE(self%latitudes, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      RETURN
    END IF
  END IF

  status%message = ''
  ALLOCATE(self%latitudes(self%lookup_int(lbrow)), SOURCE=latitudes,          &
           STAT=status%icode)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to allocate latitudes array'
  END IF

END FUNCTION set_latitudes

!-------------------------------------------------------------------------------

FUNCTION get_latitudes(self, latitudes) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64), ALLOCATABLE :: temp_latitudes(:)
  REAL(KIND=real64), ALLOCATABLE :: latitudes(:)
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%latitudes)) THEN
    ALLOCATE(temp_latitudes(SIZE(self%latitudes)))
    temp_latitudes(:) = self%latitudes(:)
    CALL MOVE_ALLOC(temp_latitudes, latitudes)
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'Latitudes not set'
  END IF

END FUNCTION get_latitudes

!-------------------------------------------------------------------------------

FUNCTION get_coords(self, x, y, coords) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64)                 :: x, y
  REAL(KIND=real64)                   :: coords(2)
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (x < 1_int64 .OR. x > SIZE(self%longitudes)) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A,I0,A)') 'Coordinate x-index (', x,         &
                           'out of range (', SIZE(self%longitudes), ')'
    RETURN
  END IF

  IF (y < 1_int64 .OR. y > SIZE(self%latitudes)) THEN
    status%icode = 1_int64
    WRITE(status%message, '(A,I0,A,I0,A)') 'Coordinate y-index (', y,         &
                           'out of range (', SIZE(self%latitudes), ')'
    RETURN
  END IF

  coords(1) = self%longitudes(x)
  coords(2) = self%latitudes(y)
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_coords

!-------------------------------------------------------------------------------

FUNCTION get_pole_location(self, pole_location) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: bplon, bplat
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64) :: pole_location(2)
  TYPE(shum_ff_status_type) :: status ! Return status object

  ! SHUMlib stores parameters containing the index in the 64-word lookup
  ! However, here we've split it into it's integer and real components, so we
  ! deduct the length of the integer lookup to find the position in the real
  ! part for bplon and bplat

  pole_location = [self%lookup_real(bplon - len_integer_lookup),              &
                   self%lookup_real(bplat - len_integer_lookup)]
  status%icode = SHUMLIB_SUCCESS
  status%message = ''

END FUNCTION get_pole_location

!-------------------------------------------------------------------------------
! Data accessors
!-------------------------------------------------------------------------------
FUNCTION set_rdata(self, rdata_in) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbrow, lbnpt
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  REAL(KIND=real64), INTENT(IN) :: rdata_in(self%lookup_int(lbnpt),           &
                                            self%lookup_int(lbrow))
  TYPE(shum_ff_status_type) :: status ! Return status object

  status%icode = SHUMLIB_SUCCESS
  status%message = ''
  IF (ALLOCATED(self%rdata)) THEN
    DEALLOCATE(self%rdata, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to deallocate pre-existing rdata'
      RETURN
    END IF
  END IF

  ALLOCATE(self%rdata(self%lookup_int(lbnpt), self%lookup_int(lbrow)),        &
           SOURCE=rdata_in, STAT=status%icode)
  IF (status%icode /= SHUMLIB_SUCCESS) THEN
    status%message = 'Failed to get allocate rdata'
  END IF

END FUNCTION set_rdata

!-------------------------------------------------------------------------------

FUNCTION get_rdata(self, rdata) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbrow, lbnpt
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  REAL(KIND=real64) :: rdata(self%lookup_int(lbnpt),                          &
                             self%lookup_int(lbrow))
  TYPE(shum_ff_status_type) :: status ! Return status object


  IF (ALLOCATED(self%rdata)) THEN
    rdata = self%rdata
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'rdata not set'
  END IF

END FUNCTION get_rdata

!-------------------------------------------------------------------------------

FUNCTION get_rdata_by_location(self, x, y, rdata) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64)                 :: x, y
  REAL(KIND=real64)                   :: rdata
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%rdata)) THEN
    IF (x < 1_int64 .OR. x > SIZE(self%rdata, 1)) THEN
      status%icode = 1_int64
      WRITE(status%message, '(A,I0,A,I0,A)') 'Requested rdata x-location (',  &
                   x, ') out of range (1:',SIZE(self%rdata, 1), ')'
    ELSE IF  (y < 1_int64 .OR. y > SIZE(self%rdata, 2)) THEN
      status%icode = 1_int64
      WRITE(status%message, '(A,I0,A,I0,A)') 'Requested rdata y-location (',  &
                   y, ') out of range (1:',SIZE(self%rdata, 2), ')'
    ELSE
      rdata = self%rdata(x, y)
      status%icode = SHUMLIB_SUCCESS
      status%message = ''
    END IF
  ELSE
    status%icode = 1_int64
    status%message = 'rdata not set'
  END IF

END FUNCTION get_rdata_by_location

!-------------------------------------------------------------------------------

FUNCTION set_idata(self, idata_in) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbrow, lbnpt
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  INTEGER(KIND=int64), INTENT(IN) :: idata_in(self%lookup_int(lbnpt),         &
                                              self%lookup_int(lbrow))
  TYPE(shum_ff_status_type) :: status ! Return status object

  status%icode = SHUMLIB_SUCCESS
  IF (ALLOCATED(self%idata)) THEN
    DEALLOCATE(self%idata, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%message = 'Failed to deallocate pre-existing idata'
      RETURN
    END IF
  END IF

  status%message = ''
  ALLOCATE(self%idata(self%lookup_int(lbnpt), self%lookup_int(lbrow)),        &
           SOURCE=idata_in, STAT=status%icode)
  IF (status%icode /=0_int64) THEN
    status%message = 'Failed to allocate idata'
  END IF

END FUNCTION set_idata

!-------------------------------------------------------------------------------

FUNCTION get_idata(self, idata) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbrow, lbnpt
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64) :: idata(self%lookup_int(lbnpt),                        &
                                   self%lookup_int(lbrow))
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%idata)) THEN
    idata = self%idata
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
  ELSE
    status%icode = 1_int64
    status%message = 'idata not set'
  END IF


END FUNCTION get_idata

!-------------------------------------------------------------------------------

FUNCTION get_idata_by_location(self, x, y, idata) RESULT(status)
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(IN) :: self
  INTEGER(KIND=int64)                 :: x, y
  INTEGER(KIND=int64)                 :: idata
  TYPE(shum_ff_status_type) :: status ! Return status object

  IF (ALLOCATED(self%idata)) THEN
    IF (x < 1_int64 .OR. x > SIZE(self%idata, 1)) THEN
      status%icode = 1_int64
      WRITE(status%message, '(A,I0,A,I0,A)') 'Requested idata x-location (',  &
                   x, ') out of range (1:',SIZE(self%idata, 1), ')'
    ELSE IF  (y < 1_int64 .OR. y > SIZE(self%idata, 2)) THEN
      status%icode = 1_int64
      WRITE(status%message, '(A,I0,A,I0,A)') 'Requested idata y-location (',  &
                   y, ') out of range (1:',SIZE(self%idata, 2), ')'
    ELSE
      idata = self%idata(x, y)
      status%icode = SHUMLIB_SUCCESS
      status%message = ''
    END IF
  ELSE
    status%icode = 1_int64
    status%message = 'idata not set'
  END IF

END FUNCTION get_idata_by_location

!-------------------------------------------------------------------------------
! Utility methods
!-------------------------------------------------------------------------------

FUNCTION unload_data(self) RESULT(status)
  ! This returns negative if there is nothing to
  ! Zero is data is successfully deallocated
  ! Positive if there is an error deallocating
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  TYPE(shum_ff_status_type) :: status ! Return status object

  status%icode = -1_int64
  status%message = 'Data already not loaded'
  IF (ALLOCATED(self%rdata)) THEN
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
    DEALLOCATE(self%rdata, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%icode = ABS(status%icode)
      status%message = 'Failed to deallocate rdata'
    END IF
  END IF

  IF (ALLOCATED(self%idata)) THEN
    status%icode = SHUMLIB_SUCCESS
    status%message = ''
    DEALLOCATE(self%idata, STAT=status%icode)
    IF (status%icode /= SHUMLIB_SUCCESS) THEN
      status%icode = ABS(status%icode)
      status%message = 'Failed to deallocate idata'
    END IF
  END IF

END FUNCTION unload_data

!-------------------------------------------------------------------------------
! Private methods
!-------------------------------------------------------------------------------

FUNCTION generate_fixed_grid(self) RESULT(status)
  USE f_shum_lookup_indices_mod, ONLY: lbnpt,lbrow, bzy, bdy, bzx, bdx
  IMPLICIT NONE
  CLASS(shum_field_type), INTENT(INOUT) :: self
  TYPE(shum_ff_status_type) :: status ! Return status object

  ! Positional headers in real lookup
  ! SHUMlib stores parameters containing the index in the 64-word lookup
  ! However, here we've split it into it's integer and real components, so we
  ! deduct the length of the integer lookup to find the position in the real
  ! part when using bdx, bdy, bzx, bzy

  ! loop counter
  INTEGER(KIND=int64) :: k_coord

  ! Temporary positional counters
  REAL(KIND=real64) :: longitude, latitude


  ALLOCATE(self%longitudes(self%lookup_int(lbnpt)))
  longitude = self%lookup_real(bzx - len_integer_lookup)
  DO k_coord = 1, self%lookup_int(lbnpt)
    longitude = longitude + self%lookup_real(bdx - len_integer_lookup)
    self%longitudes(k_coord) = longitude
  END DO

  ALLOCATE(self%latitudes(self%lookup_int(lbrow)))
  latitude = self%lookup_real(bzy - len_integer_lookup)
  DO k_coord = 1, self%lookup_int(lbrow)
    latitude = latitude + self%lookup_real(bdy - len_integer_lookup)
    self%latitudes(k_coord) = latitude
  END DO
  status%icode = SHUMLIB_SUCCESS
  status%message = ''
END FUNCTION generate_fixed_grid

!-------------------------------------------------------------------------------
! Auxiliary (non-type bound) procedures
!-------------------------------------------------------------------------------

REAL(KIND=real64) FUNCTION convert_to_julian(year,month,day,hr,minute,sec)
  ! Convert a date and time to fractional Julian day in hours
  ! Algorithm from Wikipedia: https://en.wikipedia.org/wiki/Julian_day
  ! Note that this function uses units of hours (since we measure forecast
  ! time in hours), rather than days as in the source.

  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: year,month,day,hr,minute,sec
  INTEGER(KIND=int64) :: convert_to_julian_int

  convert_to_julian_int = (1461_int64 * (year + 4800_int64 + (month -         &
                       14_int64)/12_int64))/4_int64 +(367_int64 *             &
                      (month - 2_int64  - 12_int64 * ((month -14_int64)/      &
                      12_int64 )))/12_int64 - (3_int64 *                      &
                      ((year + 4900_int64 + (month - 14_int64)/12_int64)/     &
                      100_int64))/4_int64 + day - 32075_int64

  convert_to_julian = REAL(convert_to_julian_int, real64) * 24_real64 +       &
                      (hr + (minute + sec/60.0_real64) /60.0_real64)

END FUNCTION convert_to_julian
!-------------------------------------------------------------------------------

END MODULE f_shum_field_mod
