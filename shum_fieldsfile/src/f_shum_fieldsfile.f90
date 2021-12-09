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
! Description: Methods for reading and writing UM fieldsfiles.
!
MODULE f_shum_fieldsfile_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

USE f_shum_lookup_indices_mod, ONLY:                                           &
  lblrec, lbpack, lbegin, lbnrec, lbuser1, lbrel, lbuser2

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

USE f_shum_byteswap_mod, ONLY:                                                 &
  f_shum_byteswap, f_shum_get_machine_endianism,                               &
  f_shum_littleendian, f_shum_bigendian

IMPLICIT NONE

PRIVATE

PUBLIC :: f_shum_open_file,                                                    &
          f_shum_create_file,                                                  &
          f_shum_close_file,                                                   &
          f_shum_read_fixed_length_header,                                     &
          f_shum_read_integer_constants,                                       &
          f_shum_read_real_constants,                                          &
          f_shum_read_level_dependent_constants,                               &
          f_shum_read_row_dependent_constants,                                 &
          f_shum_read_column_dependent_constants,                              &
          f_shum_read_additional_parameters,                                   &
          f_shum_read_extra_constants,                                         &
          f_shum_read_temp_histfile,                                           &
          f_shum_read_compressed_index,                                        &
          f_shum_read_lookup,                                                  &
          f_shum_read_field_data,                                              &
          f_shum_write_fixed_length_header,                                    &
          f_shum_write_integer_constants,                                      &
          f_shum_write_real_constants,                                         &
          f_shum_write_level_dependent_constants,                              &
          f_shum_write_row_dependent_constants,                                &
          f_shum_write_column_dependent_constants,                             &
          f_shum_write_additional_parameters,                                  &
          f_shum_write_extra_constants,                                        &
          f_shum_write_temp_histfile,                                          &
          f_shum_write_compressed_index,                                       &
          f_shum_write_lookup,                                                 &
          f_shum_precalc_data_positions,                                       &
          f_shum_write_field_data,                                             &
          f_shum_fixed_length_header_len,                                      &
          f_shum_lookup_dim1_len


INTERFACE f_shum_read_field_data
  MODULE PROCEDURE                                                             &
          f_shum_read_field_data_real64,                                       &
          f_shum_read_field_data_int64,                                        &
          f_shum_read_field_data_real32,                                       &
          f_shum_read_field_data_int32
END INTERFACE

INTERFACE f_shum_write_field_data
  MODULE PROCEDURE                                                             &
          f_shum_write_field_data_direct_real64,                               &
          f_shum_write_field_data_direct_int64,                                &
          f_shum_write_field_data_direct_real32,                               &
          f_shum_write_field_data_direct_int32,                                &
          f_shum_write_field_data_sequential_real64,                           &
          f_shum_write_field_data_sequential_int64,                            &
          f_shum_write_field_data_sequential_real32,                           &
          f_shum_write_field_data_sequential_int32
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

INTEGER(KIND=int64), PARAMETER :: f_shum_fixed_length_header_len = 256
INTEGER(KIND=int64), PARAMETER :: f_shum_lookup_dim1_len = 64

INTEGER(KIND=int64), PARAMETER :: imdi = -32768
INTEGER(KIND=int64), PARAMETER :: field_padding = 512
INTEGER(KIND=int64), PARAMETER :: data_start_alignment = 524288

! A linked-list element object storing information on an open file; for each
! file we store an "id" that identifies it, a controlled copy of both the
! fixed length header and lookup and the path to the file
TYPE ff_type
  INTEGER(KIND=int64) :: unique_id
  INTEGER(KIND=int64) :: fixed_length_header(f_shum_fixed_length_header_len)
  INTEGER(KIND=int64), ALLOCATABLE :: lookup(:, :)
  CHARACTER(LEN=:),    ALLOCATABLE :: filename
  INTEGER(KIND=int64) :: next_unwritten_field = 1
  LOGICAL             :: read_only = .TRUE.
  LOGICAL             :: native_endian = .TRUE.
  TYPE(ff_type), POINTER :: next => NULL()
  TYPE(ff_type), POINTER :: prev => NULL()
END TYPE ff_type

! An object to act as the access point and counter of the elements above
TYPE ff_list_type
  INTEGER(KIND=int64) :: length = 0
  TYPE(ff_type), POINTER :: head => NULL()
  TYPE(ff_type), POINTER :: tail => NULL()
END TYPE ff_list_type

TYPE(ff_list_type), SAVE :: ff_list

CONTAINS

!------------------------------------------------------------------------------!
! Creates a new ff object and assigns it a given filename and "id"
SUBROUTINE create_ff_type(id, filename, ff)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN) :: id
CHARACTER(LEN=*),    INTENT(IN) :: filename
TYPE(ff_type),       INTENT(OUT), POINTER :: ff

ALLOCATE(ff)
ff % filename = TRIM(filename)
ff % unique_id = id
ff % fixed_length_header(:) = imdi

END SUBROUTINE create_ff_type

!------------------------------------------------------------------------------!
! Cleanup the memory used by an ff object when it is no longer needed
SUBROUTINE destroy_ff_type(ff)
IMPLICIT NONE

TYPE(ff_type), INTENT(INOUT), POINTER :: ff

DEALLOCATE(ff % lookup)
DEALLOCATE(ff % filename)
DEALLOCATE(ff)

END SUBROUTINE destroy_ff_type

!------------------------------------------------------------------------------!
! Insert an ff object onto the end of the list, updating the pointers
SUBROUTINE append_to_file_list(ff)
IMPLICIT NONE

TYPE(ff_type), INTENT(INOUT), POINTER :: ff

NULLIFY(ff % next)
NULLIFY(ff % prev)

IF (ff_list % length == 0) THEN
  ! If the list is empty the ff object becomes the start and end of the list
  ff_list % head => ff
  ff_list % tail => ff
ELSE
  ! If the list is not empty the ff object is attached as the next link from
  ! the final member of the list, the current final member becomes the previous
  ! link of the ff object and then the ff object becomes the new final member
  ff_list % tail % next => ff
  ff % prev => ff_list % tail
  ff_list % tail => ff
END IF
ff_list % length = ff_list % length + 1

END SUBROUTINE append_to_file_list

!------------------------------------------------------------------------------!
! Remove a given ff object from the list, updating the pointers
SUBROUTINE remove_from_file_list(ff)
IMPLICIT NONE

TYPE(ff_type), INTENT(INOUT), POINTER :: ff
TYPE(ff_type), POINTER :: ff_check

LOGICAL :: found

! First ensure the object is in the list in the first place
NULLIFY(ff_check)
found = .FALSE.
ff_check => ff_list % head
DO WHILE (ASSOCIATED(ff_check))
  IF (ff_check % unique_id == ff % unique_id) THEN
    found = .TRUE.
    EXIT
  END IF
  ff_check => ff_check % next
END DO

IF (found) THEN
  IF (ff_list % length > 1) THEN
    IF (.NOT. ASSOCIATED(ff % next)) THEN
      ! In the case that ff is the final member of the list, make the previous
      ! link the new final member and break its forward link (to ff)
      ff_list % tail => ff % prev
      NULLIFY(ff_list % tail % next)
    ELSE IF (.NOT. ASSOCIATED(ff % prev)) THEN
      ! In the case that ff is the first member of the list, make the next
      ! link the new first member and break its backward link (to ff)
      ff_list % head => ff % next
      NULLIFY(ff_list % head % prev)
    ELSE
      ! If ff is an intermediate member of the list, update the links before
      ! and after it to bypass it (by pointing to its own next/previous links)
      ff % prev % next => ff % next
      ff % next % prev => ff % prev
    END IF
  ELSE
    ! If this list only has one member left the list can just be reset
    NULLIFY(ff_list % head)
    NULLIFY(ff_list % tail)
  END IF
  ff_list % length = ff_list % length - 1
END IF

END SUBROUTINE remove_from_file_list

!------------------------------------------------------------------------------!
! This is the mechanism to retreive a pointer to a member of the list given
! its unique id
FUNCTION unique_id_to_ff(id) RESULT(ff)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN) :: id
TYPE(ff_type),          POINTER :: ff

NULLIFY(ff)
ff => ff_list % head
DO WHILE (ASSOCIATED(ff))
  IF (ff % unique_id == id) THEN
    RETURN
  END IF
  ff => ff % next
END DO
! Note that the loop returns once the id is found; therefore if we reach this
! point we have failed to find the object we should clear the pointer to avoid
! returning the last element in the list
NULLIFY(ff)

END FUNCTION

!------------------------------------------------------------------------------!

FUNCTION f_shum_open_file(filename, ff_id, message) RESULT(status)
IMPLICIT NONE

CHARACTER(LEN=*),    INTENT(IN)  :: filename
INTEGER(KIND=int64), INTENT(OUT) :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
TYPE(ff_type), POINTER :: ff
REAL(KIND=real64)      :: rand
LOGICAL                :: is_open

INTEGER(KIND=int64), ALLOCATABLE :: lookup(:, :)

status = 0_int64
message = ""

! TODO: Replace this with portio; for now we're just using intrinsic read
! and write methods, so the ff_id is just file unit; but we'll randomise it
! and keep it above a certain range to try and avoid clashes with genuine
! file units
is_open = .TRUE.
DO WHILE (is_open)
  CALL RANDOM_NUMBER(rand)
  ff_id = 50000 + FLOOR(10000*rand)
  INQUIRE(UNIT=ff_id, OPENED=is_open)
END DO

OPEN(UNIT=ff_id, ACCESS="STREAM", FORM="UNFORMATTED", FILE=filename,           &
                                     IOSTAT=status, STATUS="OLD", IOMSG=message)
IF (status /= 0) THEN
  message = "Failed to open file ("//TRIM(message)//")"
  RETURN
END IF

! Register this file with the module
CALL create_ff_type(ff_id, filename, ff)
CALL append_to_file_list(ff)

! Determine the byte-ordering of the file
IF (f_shum_get_machine_endianism() == f_shum_littleendian) THEN
  ff % native_endian = .FALSE.
ELSE IF (f_shum_get_machine_endianism() == f_shum_bigendian) THEN
  ff % native_endian = .TRUE.
ELSE
  message = "Unexpected return value from get_machine_endianism"
  status = 1_int64
  RETURN
END IF

! Tag this file as read only
ff % read_only = .TRUE.

! Extract the fixed length header and save it to the private array
status = f_shum_read_fixed_length_header(                                      &
                                       ff_id, ff % fixed_length_header, message)
IF (status /= 0) RETURN

! Extract the lookup and save it to the private array
status = f_shum_read_lookup(ff_id, lookup, message)
IF (status /= 0) RETURN

ff % lookup = lookup
DEALLOCATE(lookup)

END FUNCTION f_shum_open_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_create_file(                                                   &
                  filename, n_lookups, ff_id, message, overwrite) RESULT(status)

IMPLICIT NONE

CHARACTER(LEN=*),    INTENT(IN)  :: filename
INTEGER(KIND=int64), INTENT(IN)  :: n_lookups
INTEGER(KIND=int64), INTENT(OUT) :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: overwrite

INTEGER(KIND=int64)    :: status
TYPE(ff_type), POINTER :: ff
REAL(KIND=real64)      :: rand
LOGICAL                :: is_open
CHARACTER(LEN=7)       :: open_status

status = 0_int64
message = ""

! TODO: Replace this with portio; for now we're just using intrinsic read
! and write methods, so the ff_id is just file unit; but we'll randomise it
! and keep it above a certain range to try and avoid clashes with genuine
! file units
is_open = .TRUE.
DO WHILE (is_open)
  CALL RANDOM_NUMBER(rand)
  ff_id = 50000 + FLOOR(10000*rand)
  INQUIRE(UNIT=ff_id, OPENED=is_open)
END DO

! Check if we can overwrite (default is that we can)
open_status = "REPLACE"
IF (PRESENT(overwrite)) THEN
  IF (.NOT. overwrite) THEN
    open_status = "NEW"
  END IF
END IF

OPEN(UNIT=ff_id, ACCESS="STREAM", FORM="UNFORMATTED", FILE=filename,           &
                         IOSTAT=status, STATUS=TRIM(open_status), IOMSG=message)
IF (status /= 0) THEN
  message = "Failed to create file ("//TRIM(message)//")"
  RETURN
END IF

! Register this file with the module
CALL create_ff_type(ff_id, filename, ff)
CALL append_to_file_list(ff)

! Check if the file is to be output in the non-native byte ordering
IF (f_shum_get_machine_endianism() == f_shum_littleendian) THEN
  ff % native_endian = .FALSE.
ELSE IF (f_shum_get_machine_endianism() == f_shum_bigendian) THEN
  ff % native_endian = .TRUE.
ELSE
  message = "Unexpected return value from get_machine_endianism"
  status = 1_int64
  RETURN
END IF

! Tag this file as writeable
ff % read_only = .FALSE.

! Allocate the lookup array
ALLOCATE(ff % lookup(f_shum_lookup_dim1_len, n_lookups))

! Populate it with empty values
ff % lookup(1:45, :) = -99
ff % lookup(46:, :) = TRANSFER(0.0_real64, 0_int64)

! Set the lookup dimensions in the fixed_length_header
ff % fixed_length_header(lookup_dim1) = f_shum_lookup_dim1_len
ff % fixed_length_header(lookup_dim2) = n_lookups

END FUNCTION f_shum_create_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_close_file(ff_id, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)            :: status
INTEGER(KIND=int64)            :: padding_start
INTEGER(KIND=int64)            :: padding_length
INTEGER(KIND=int64)            :: i
INTEGER(KIND=int64)            :: i_last
TYPE(ff_type), POINTER         :: ff
REAL(KIND=real64), ALLOCATABLE :: padding(:)

status = 0_int64
message = ""

ff => unique_id_to_ff(ff_id)

! On close, ensure the final form of both the fixed length header and the
! lookup are written to disk (for a created file)
IF (.NOT. ff % read_only) THEN
  status = commit_lookup(ff, message)
  IF (status /= 0) RETURN

  ! Trim the 2nd dimension of the lookup table so that it gives the total
  ! *populated* headers only
  IF (ff % next_unwritten_field > 1) THEN
    ff % fixed_length_header(lookup_dim2) = ff % next_unwritten_field - 1
  ELSE
    DO i = 1, ff % fixed_length_header(lookup_dim2)
      IF (ff % lookup(lbegin, i) == -99) THEN
        EXIT
      END IF
      i_last = i
    END DO
    ff % fixed_length_header(lookup_dim2) = i_last
  END IF

  status = commit_fixed_length_header(ff, message)
  IF (status /= 0) RETURN

  ! The "padding" accompanying each field is never written (the API just
  ! leaves a gap), but the final field must be explicitly padded with data
  ! or a read of the last field may overrun the file
  i = ff % fixed_length_header(lookup_dim2)
  IF (i > 0) THEN
    IF (MOD(ff % lookup(lbpack, i), 10_int64) == 2) THEN
      ! For 32-bit packed fields, the lblrec value must be halved
      padding_start = ff % lookup(lbegin, i) + (ff % lookup(lblrec, i) + 1)/2
      padding_length = ff % lookup(lbnrec, i) - (ff % lookup(lblrec, i) + 1)/2
    ELSE
      padding_start = ff % lookup(lbegin, i) + ff % lookup(lblrec, i)
      padding_length = ff % lookup(lbnrec, i) - ff % lookup(lblrec, i)
    END IF

    ALLOCATE(padding(padding_length))
    padding(:) = 0.0_real64

    WRITE(ff_id, POS=(padding_start)*8+1, IOSTAT=status, IOMSG=message) padding
    IF (status /= 0) THEN
      status = 1_int64
      message = "Failed to write padding to end of file ("//TRIM(message)//")"
      RETURN
    END IF
    DEALLOCATE(padding)
  END IF
END IF

CALL remove_from_file_list(ff)
CALL destroy_ff_type(ff)

CLOSE(ff_id, IOSTAT=status, IOMSG=message)
IF (status /= 0) THEN
  message = "Failed to close file ("//TRIM(message)//")"
END IF

END FUNCTION f_shum_close_file

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_fixed_length_header(                                      &
                             ff_id, fixed_length_header, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(OUT) ::                                            &
                             fixed_length_header(f_shum_fixed_length_header_len)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status
TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Since the private file object keeps a copy of the header, we can return
! it without re-reading if it has already been read
ff => unique_id_to_ff(ff_id)

IF (ALL(ff % fixed_length_header == imdi)) THEN
  ! Read in the file data (TODO: replace this with proper "buffin" and
  ! "setpos" calls once portio makes it into Shumlib)
  READ(ff_id, POS=1_int64, IOSTAT=status, IOMSG=message) fixed_length_header(:)
  IF (status /= 0) THEN
    message = "Failed to read fixed_length_header ("//TRIM(message)//")"
    RETURN
  END IF

  ! Apply any required byteswapping
  IF (.NOT. ff % native_endian) THEN
    status = f_shum_byteswap(fixed_length_header,                              &
                             f_shum_fixed_length_header_len, 8_int64, message)
    IF (status /= 0) THEN
      message = "Failed to byteswap fixed_length_header ("//TRIM(message)//")"
      RETURN
    END IF
  END IF
ELSE
  ! The header was already read in - just return it
  fixed_length_header = ff % fixed_length_header
END IF

END FUNCTION f_shum_read_fixed_length_header

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_integer_constants(                                        &
                               ff_id, integer_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)                 :: ff_id
INTEGER(KIND=int64), INTENT(INOUT), ALLOCATABLE :: integer_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)                :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(int_const_start)
dim = ff % fixed_length_header(int_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any integer_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(integer_constants) .AND. (SIZE(integer_constants) /= dim)) THEN
  DEALLOCATE(integer_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed integer_constants"
    RETURN
  END IF
END IF
ALLOCATE(integer_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for integer_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) integer_constants
IF (status /= 0) THEN
  message = "Failed to read integer_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(integer_constants, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap integer_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_integer_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_real_constants(                                           &
                                  ff_id, real_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)                 :: ff_id
REAL(KIND=real64),   INTENT(INOUT), ALLOCATABLE :: real_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)                :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(real_const_start)
dim = ff % fixed_length_header(real_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any real_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(real_constants) .AND. (SIZE(real_constants) /= dim)) THEN
  DEALLOCATE(real_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed real_constants"
    RETURN
  END IF
END IF
ALLOCATE(real_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for real_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) real_constants
IF (status /= 0) THEN
  message = "Failed to read real_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(real_constants, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap real_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_real_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_level_dependent_constants(                                &
                       ff_id, level_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: level_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(lev_dep_const_start)
dim1 = ff % fixed_length_header(lev_dep_const_dim1)
dim2 = ff % fixed_length_header(lev_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any level_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(level_dependent_constants)                                       &
    .AND. (SIZE(level_dependent_constants, 1) /= dim1)                         &
    .AND. (SIZE(level_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(level_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed level_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(level_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for level_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                   &
                                                       level_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read level_dependent_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(                                                    &
                      level_dependent_constants, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message =                                                                  &
            "Failed to byteswap level_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_level_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_row_dependent_constants(                                  &
                         ff_id, row_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: row_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(row_dep_const_start)
dim1 = ff % fixed_length_header(row_dep_const_dim1)
dim2 = ff % fixed_length_header(row_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any row_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(row_dependent_constants)                                         &
    .AND. (SIZE(row_dependent_constants, 1) /= dim1)                           &
    .AND. (SIZE(row_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(row_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed row_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(row_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for row_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                   &
                                                         row_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read row_dependent_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(                                                    &
                        row_dependent_constants, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap row_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_row_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_column_dependent_constants(                               &
                      ff_id, column_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: column_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(col_dep_const_start)
dim1 = ff % fixed_length_header(col_dep_const_dim1)
dim2 = ff % fixed_length_header(col_dep_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any column_dependent_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(column_dependent_constants)                                      &
    .AND. (SIZE(column_dependent_constants, 1) /= dim1)                        &
    .AND. (SIZE(column_dependent_constants, 2) /= dim2)) THEN
  DEALLOCATE(column_dependent_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed column_dependent_constants"
    RETURN
  END IF
END IF
ALLOCATE(column_dependent_constants(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for column_dependent_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                   &
                                                      column_dependent_constants
IF (status /= 0) THEN
  message = "Failed to read column_dependent_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(                                                    &
                     column_dependent_constants, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message =                                                                  &
           "Failed to byteswap column_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_column_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_additional_parameters(                                    &
                           ff_id, additional_parameters, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: additional_parameters(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(additional_const_start)
dim1 = ff % fixed_length_header(additional_const_dim1)
dim2 = ff % fixed_length_header(additional_const_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any additional_parameters"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(additional_parameters)                                      &
    .AND. (SIZE(additional_parameters, 1) /= dim1)                        &
    .AND. (SIZE(additional_parameters, 2) /= dim2)) THEN
  DEALLOCATE(additional_parameters, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed additional_parameters"
    RETURN
  END IF
END IF
ALLOCATE(additional_parameters(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for additional_parameters"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                   &
                                                           additional_parameters
IF (status /= 0) THEN
  message = "Failed to read additional_parameters ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(                                                    &
                          additional_parameters, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap additional_parameters ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_additional_parameters

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_extra_constants(                                          &
                                 ff_id, extra_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: extra_constants(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(extra_const_start)
dim = ff % fixed_length_header(extra_const_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any extra_constants"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(extra_constants) .AND. (SIZE(extra_constants) /= dim)) THEN
  DEALLOCATE(extra_constants, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed extra_constants"
    RETURN
  END IF
END IF
ALLOCATE(extra_constants(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for extra_constants"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) extra_constants
IF (status /= 0) THEN
  message = "Failed to read extra_constants ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(extra_constants, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap extra_constants ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_extra_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_temp_histfile(ff_id, temp_histfile, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: temp_histfile(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(temp_histfile_start)
dim = ff % fixed_length_header(temp_histfile_dim)

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any temp_histfile"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(temp_histfile) .AND. (SIZE(temp_histfile) /= dim)) THEN
  DEALLOCATE(temp_histfile, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed temp_histfile"
    RETURN
  END IF
END IF
ALLOCATE(temp_histfile(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for temp_histfile"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) temp_histfile
IF (status /= 0) THEN
  message = "Failed to read temp_histfile ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(temp_histfile, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap temp_histfile ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_temp_histfile

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_compressed_index(                                         &
                         ff_id, compressed_index, index, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)    :: ff_id
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE   :: compressed_index(:)
INTEGER(KIND=int64), INTENT(IN)    :: index
CHARACTER(LEN=*),    INTENT(OUT)   :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
SELECT CASE(index)
  CASE(1_int64)
    start = ff % fixed_length_header(comp_field_index1_start)
    dim = ff % fixed_length_header(comp_field_index1_dim)
  CASE(2_int64)
    start = ff % fixed_length_header(comp_field_index2_start)
    dim = ff % fixed_length_header(comp_field_index2_dim)
  CASE(3_int64)
    start = ff % fixed_length_header(comp_field_index3_start)
    dim = ff % fixed_length_header(comp_field_index3_dim)
  CASE DEFAULT
    status = 1_int64
    message = "Invalid compressed index requested, must be 1, 2 or 3"
    RETURN
END SELECT

! Check that these make sense
IF ((start == imdi) .OR. (dim <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify the requested compressed_index"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(compressed_index) .AND. (SIZE(compressed_index) /= dim)) THEN
  DEALLOCATE(compressed_index, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed compressed_index"
    RETURN
  END IF
END IF
ALLOCATE(compressed_index(dim), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for compressed_index"
  RETURN
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
! byte so that "POS=1" is the start of the file (hence the "+1")
READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) compressed_index
IF (status /= 0) THEN
  message = "Failed to read compressed_index ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(compressed_index, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap compressed_index ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_compressed_index

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_lookup(ff_id, lookup, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(INOUT),                                            &
                     ALLOCATABLE    :: lookup(:, :)
CHARACTER(LEN=*),    INTENT(OUT)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: dim1
INTEGER(KIND=int64) :: dim2
INTEGER(KIND=int64) :: i

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored fixed length header
ff => unique_id_to_ff(ff_id)

! Get the start position and dimensions from the fixed_length_header
start = ff % fixed_length_header(lookup_start)
dim1 = ff % fixed_length_header(lookup_dim1)
dim2 = ff % fixed_length_header(lookup_dim2)

! Check that these make sense
IF ((start == imdi) .OR. (dim1 <= 0_int64) .OR. (dim2 <= 0_int64)) THEN
  status = -1_int64
  message = "File does not specify any lookup table"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(lookup)                                                          &
    .AND. (SIZE(lookup, 1) /= dim1)                                            &
    .AND. (SIZE(lookup, 2) /= dim2)) THEN
  DEALLOCATE(lookup, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed lookup array"
    RETURN
  END IF
END IF
ALLOCATE(lookup(dim1, dim2), STAT=status)
IF (status /= 0) THEN
  message = "Unable to allocate memory for lookup array"
  RETURN
END IF

! Since the private file object keeps a copy of the lookup, only read it
! if it hasn't been populated
IF (.NOT. ALLOCATED(ff % lookup)) THEN
  ! Read in the file data (TODO: replace this with proper "buffin" and
  ! "setpos" calls once portio makes it into Shumlib)
  ! "start" is inclusive of the given word (hence the "-1") and is in 64-bit
  ! (8-byte) words but "POS" is in bytes (hence the "*8") and is offset by one
  ! byte so that "POS=1" is the start of the file (hence the "+1")
  READ(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) lookup
  IF (status /= 0) THEN
    message = "Failed to read lookup table ("//TRIM(message)//")"
    RETURN
  END IF

  ! Apply any required byteswapping
  IF (.NOT. ff % native_endian) THEN
    status = f_shum_byteswap(lookup, dim1*dim2, 8_int64, message)
    IF (status /= 0) THEN
      message = "Failed to byteswap lookup ("//TRIM(message)//")"
      RETURN
    END IF
  END IF

  ! Check to see if the file appears to use indirect access; some older files
  ! don't use the positional elements and instead rely on the disk size and
  ! field-order.  For simplicity, calculate the positional elements here and
  ! update the value of lbegin so that the returned lookup can always be used
  ! for direct access.
  IF (ANY(lookup(lbegin,:) == 0_int64)) THEN
    lookup(lbegin, 1) = data_start
    DO i = 2, SIZE(lookup, 2)
      IF (lookup(lbnrec, i-1) > 0) THEN
        lookup(lbegin, i) = lookup(lbegin, i-1) + lookup(lbnrec, i-1) + 1
      END IF
    END DO
  END IF
ELSE
  ! If the lookup was already read in return it
  lookup = ff % lookup
END IF

END FUNCTION f_shum_read_lookup

!------------------------------------------------------------------------------!

FUNCTION describe_allowed_kind_based_on_lbpack(lbpack) RESULT(description)

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)        :: lbpack
INTEGER(KIND=int64)                    :: lbpack_n1
INTEGER(KIND=int64), PARAMETER         :: prefix_len = 41
INTEGER(KIND=int64), PARAMETER         :: suffix_len = 40
CHARACTER(LEN=prefix_len)              :: prefix
CHARACTER(LEN=prefix_len + suffix_len) :: description

WRITE(prefix, "(A,I0,A)") "lookup item LBPACK=", lbpack, " indicates data"

lbpack_n1 = MOD(lbpack, 10_int64)

SELECT CASE (lbpack_n1)
  CASE (0_int64)
    ! Unpacked 64-bit data
    description = TRIM(prefix)//" will be 64-bit (Unpacked)"
  CASE (1_int64)
    ! WGDOS packed data
    description = TRIM(prefix)//" will be 32-bit INTEGER (WGDOS packed)"
  CASE (2_int64)
    ! Unpacked 32-bit data
    description = TRIM(prefix)//" will be 32-bit (Truncated)"
  CASE DEFAULT
    ! Unknown
    description = TRIM(prefix)//" is not supported by Shumlib"
END SELECT

END FUNCTION describe_allowed_kind_based_on_lbpack

!------------------------------------------------------------------------------!

FUNCTION describe_allowed_type_based_on_lbuser1(lbuser1) RESULT(description)

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)        :: lbuser1
INTEGER(KIND=int64), PARAMETER         :: prefix_len = 38
INTEGER(KIND=int64), PARAMETER         :: suffix_len = 31
CHARACTER(LEN=prefix_len)              :: prefix
CHARACTER(LEN=prefix_len + suffix_len) :: description

WRITE(prefix, "(A,I0,A)") "lookup item LBUSER1=", lbuser1, " indicates data"

SELECT CASE (lbuser1)
  CASE (1_int64)
    ! REAL data
    description = TRIM(prefix)//" will be REAL"
  CASE (2_int64)
    ! Integer data
    description = TRIM(prefix)//" will be INTEGER"
  CASE (3_int64)
    ! Logical data (treated as integer)
    description = TRIM(prefix)//" will be LOGICAL (as INTEGER)"
  CASE DEFAULT
    ! Unknown
    description = TRIM(prefix)//" is not supported by Shumlib"
END SELECT

END FUNCTION describe_allowed_type_based_on_lbuser1

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_real64(                                        &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)    :: ff_id
INTEGER(KIND=int64), INTENT(IN)    :: index
REAL(KIND=real64),   INTENT(INOUT),                                            &
                     ALLOCATABLE   :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)   :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL      :: ignore_dtype

LOGICAL                :: ignore_dtype_local
INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: len_data
INTEGER(KIND=int64)    :: pack_type
INTEGER(KIND=int64)    :: data_type
INTEGER(KIND=int64)    :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len_data = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array
  ! is 64-bit, so any 32-bit types only require half the number of 64-bit words
  IF (MOD(pack_type, 10_int64) == 2) THEN
    len_data = (len_data + 1)/2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len_data <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len_data) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len_data), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of
! the file (hence the "+1")
READ(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(field_data, len_data, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap field_data array ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_field_data_real64

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_int64(                                         &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
INTEGER(KIND=int64), INTENT(INOUT),                                            &
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL       :: ignore_dtype

LOGICAL                :: ignore_dtype_local
INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: len_data
INTEGER(KIND=int64)    :: pack_type
INTEGER(KIND=int64)    :: data_type
INTEGER(KIND=int64)    :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len_data = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array
  ! is 64-bit, so any 32-bit types only require half the number of 64-bit words
  IF (MOD(pack_type, 10_int64) == 2) THEN
    len_data = (len_data + 1)/2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF ((data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len_data <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len_data) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len_data), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of
! the file (hence the "+1")
READ(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(field_data, len_data, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap field_data array ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_field_data_int64

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_real32(                                        &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
REAL(KIND=real32),   INTENT(INOUT),                                            &
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL       :: ignore_dtype

LOGICAL                :: ignore_dtype_local
INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: len_data
INTEGER(KIND=int64)    :: pack_type
INTEGER(KIND=int64)    :: data_type
INTEGER(KIND=int64)    :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
! Note: The data length reported for 32-bit truncated data is actually in
!       32-bit words, so should be correct
pack_type = lookup(lbpack)
start = lookup(lbegin)
len_data = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array
  ! is 32-bit, so any 64-bit types require double the number of 32-bit words
  ! (Note WGDOS fields are reported in 64-bit words despite being 32-bit!)
  IF (MOD(pack_type, 10_int64) /= 2) THEN
    len_data = 2*len_data
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF (MOD(pack_type, 10_int64) /= 2) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len_data <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len_data) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len_data), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of
! the file (hence the "+1")
READ(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(field_data, len_data, 4_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap field_data array ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_field_data_real32

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_field_data_int32(                                         &
                 ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)     :: ff_id
INTEGER(KIND=int64), INTENT(IN)     :: index
INTEGER(KIND=int32), INTENT(INOUT),                                            &
                     ALLOCATABLE    :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT)    :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL       :: ignore_dtype

LOGICAL                :: ignore_dtype_local
INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: len_data
INTEGER(KIND=int64)    :: pack_type
INTEGER(KIND=int64)    :: data_type
INTEGER(KIND=int64)    :: lookup(f_shum_lookup_dim1_len)

TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

lookup = ff % lookup(:, index)

! Get the type of packing, and the data dimensions from the lookup
pack_type = lookup(lbpack)
start = lookup(lbegin)
len_data = lookup(lblrec)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being read in is correct; in this case the return array
  ! is 32-bit, so any 64-bit types require double the number of 32-bit words
  ! (Note WGDOS fields are reported in 64-bit words despite being 32-bit!)
  IF (MOD(pack_type, 10_int64) /= 2) THEN
    len_data = len_data*2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing type will
  ! supply the data required to return this data (64-bit)
  IF ((MOD(pack_type, 10_int64) /= 2) .AND.                                    &
      (MOD(pack_type, 10_int64) /= 1)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = lookup(lbuser1)
  IF ((MOD(pack_type, 10_int64) == 2) .AND.                                    &
      (data_type /= 2) .AND. (data_type /=3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF

  ! The length reported for WGDOS packed fields is actually in 64-bit words, so
  ! requires double the number of (32-bit) words
  IF ((MOD(pack_type, 10_int64) == 1)) THEN
    len_data = len_data*2
  END IF

END IF

! Check that the addressing info makes sense
IF ((start <= 0) .OR. (len_data <= 0_int64)) THEN
  status = -1_int64
  message = "Lookup does not contain addressing information"
  RETURN
END IF

! If the output array is already allocated it must be deallocated first,
! unless it happens to be exactly the right size already
IF (ALLOCATED(field_data) .AND. SIZE(field_data) /= len_data) THEN
  DEALLOCATE(field_data, STAT=status)
  IF (status /= 0) THEN
    message = "Unable to de-allocate passed field_data array"
    RETURN
  END IF
END IF

IF (.NOT. ALLOCATED(field_data)) THEN
  ALLOCATE(field_data(len_data), STAT=status)
  IF (status /= 0) THEN
    message = "Unable to allocate memory for field_data array"
    RETURN
  END IF
END IF

! Now read in the file data (TODO: replace this with proper "buffin" and
! "setpos" calls once portio makes it into Shumlib)
! "start" (LBEGIN here) is in 64-bit (8-byte) words but "POS" is in bytes
! (hence the "*8") and is offset by one byte so that "POS=1" is the start of
! the file (hence the "+1")
READ(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
IF (status /= 0) THEN
  message = "Failed to read field_data ("//TRIM(message)//")"
  RETURN
END IF

! Apply any required byteswapping
IF (.NOT. ff % native_endian) THEN
  status = f_shum_byteswap(field_data, len_data, 4_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap field_data array ("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_read_field_data_int32

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_fixed_length_header(                                     &
                         ff_id, fixed_length_header, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  ::                                            &
                             fixed_length_header(f_shum_fixed_length_header_len)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
TYPE(ff_type), POINTER :: ff

ff => unique_id_to_ff(ff_id)

status = 0_int64
message = ""
IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Exclude positional elements from this write/copy, as these are controlled
! by the API.  The first positional element is the integer constants start
! and everything above this should be off-limits
ff % fixed_length_header(1:int_const_start - 1) =                              &
                                      fixed_length_header(1:int_const_start - 1)
ff % fixed_length_header(num_prognostic_fields) =                              &
                                      fixed_length_header(num_prognostic_fields)

END FUNCTION f_shum_write_fixed_length_header

!------------------------------------------------------------------------------!

FUNCTION commit_fixed_length_header(ff, message) RESULT(status)
IMPLICIT NONE

TYPE(ff_type)       :: ff
CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

INTEGER(KIND=int64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

IF (ff % native_endian) THEN
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=1, IOSTAT=status, IOMSG=message)                   &
                                                     ff % fixed_length_header(:)
  IF (status /= 0) THEN
    message = "Failed to commit fixed_length_header to disk ("//               &
                                                              TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(f_shum_fixed_length_header_len))
  swap_header = ff % fixed_length_header
  status = f_shum_byteswap(                                                    &
                  swap_header, f_shum_fixed_length_header_len, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap fixed_length_header ("//TRIM(message)//")"
    RETURN
  END IF

  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=1, IOSTAT=status, IOMSG=message) swap_header
  IF (status /= 0) THEN
    message =                                                                  &
            "Failed to commit fixed_length_header to disk ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION commit_fixed_length_header

!------------------------------------------------------------------------------!

FUNCTION get_next_free_position(ff) RESULT(position)
IMPLICIT NONE

TYPE(ff_type)       :: ff
INTEGER(KIND=int64) :: position

position = f_shum_fixed_length_header_len + 1

IF (ff % fixed_length_header(int_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(int_const_start)                   &
                 + ff % fixed_length_header(int_const_dim))
END IF

IF (ff % fixed_length_header(real_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(real_const_start)                  &
                 + ff % fixed_length_header(real_const_dim))
END IF

IF (ff % fixed_length_header(lev_dep_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(lev_dep_const_start)               &
                 + ff % fixed_length_header(lev_dep_const_dim1)                &
                 * ff % fixed_length_header(lev_dep_const_dim2))
END IF

IF (ff % fixed_length_header(row_dep_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(row_dep_const_start)               &
                 + ff % fixed_length_header(row_dep_const_dim1)                &
                 * ff % fixed_length_header(row_dep_const_dim2))
END IF

IF (ff % fixed_length_header(col_dep_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(col_dep_const_start)               &
                 + ff % fixed_length_header(col_dep_const_dim1)                &
                 * ff % fixed_length_header(col_dep_const_dim2))
END IF

IF (ff % fixed_length_header(additional_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(additional_const_start)            &
                 + ff % fixed_length_header(additional_const_dim1)             &
                 * ff % fixed_length_header(additional_const_dim2))
END IF

IF (ff % fixed_length_header(extra_const_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(extra_const_start)                 &
                 + ff % fixed_length_header(extra_const_dim))
END IF

IF (ff % fixed_length_header(temp_histfile_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(temp_histfile_start)               &
                 + ff % fixed_length_header(temp_histfile_dim))
END IF

IF (ff % fixed_length_header(comp_field_index1_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(comp_field_index1_start)           &
                 + ff % fixed_length_header(comp_field_index1_dim))
END IF

IF (ff % fixed_length_header(comp_field_index2_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(comp_field_index2_start)           &
                 + ff % fixed_length_header(comp_field_index2_dim))
END IF

IF (ff % fixed_length_header(comp_field_index3_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(comp_field_index3_start)           &
                 + ff % fixed_length_header(comp_field_index3_dim))
END IF

IF (ff % fixed_length_header(lookup_start) > 0) THEN
  position = MAX(position,                                                     &
                   ff % fixed_length_header(lookup_start)                      &
                 + ff % fixed_length_header(lookup_dim1)                       &
                 * ff % fixed_length_header(lookup_dim2))
END IF

END FUNCTION get_next_free_position

!------------------------------------------------------------------------------!

FUNCTION get_next_populated_position(ff, start) RESULT(position)
IMPLICIT NONE

TYPE(ff_type)       :: ff
INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: position

position = HUGE(0_int64)

IF (f_shum_fixed_length_header_len > start) THEN
  position = f_shum_fixed_length_header_len
END IF

IF (ff % fixed_length_header(int_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(int_const_start))
END IF

IF (ff % fixed_length_header(real_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(real_const_start))
END IF

IF (ff % fixed_length_header(lev_dep_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(lev_dep_const_start))
END IF

IF (ff % fixed_length_header(row_dep_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(row_dep_const_start))
END IF

IF (ff % fixed_length_header(col_dep_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(col_dep_const_start))
END IF

IF (ff % fixed_length_header(additional_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(additional_const_start))
END IF

IF (ff % fixed_length_header(extra_const_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(extra_const_start))
END IF

IF (ff % fixed_length_header(temp_histfile_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(temp_histfile_start))
END IF

IF (ff % fixed_length_header(comp_field_index1_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(comp_field_index1_start))
END IF

IF (ff % fixed_length_header(comp_field_index2_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(comp_field_index2_start))
END IF

IF (ff % fixed_length_header(comp_field_index3_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(comp_field_index3_start))
END IF

IF (ff % fixed_length_header(lookup_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(lookup_start))
END IF

IF (ff % fixed_length_header(data_start) > start) THEN
  position = MIN(position,                                                     &
                 ff % fixed_length_header(data_start))
END IF

END FUNCTION get_next_populated_position

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_integer_constants(                                       &
                           ff_id, integer_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: integer_constants(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

INTEGER(KIND=int64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(int_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(int_const_start)
END IF
dim = SIZE(integer_constants)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim >= next_start)) THEN
  WRITE(message, "(3(A,I0))") "Cannot write integer_constants with size ",     &
    dim," starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(int_const_start) = start
ff % fixed_length_header(int_const_dim) = dim

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                &
                                                               integer_constants
  IF (status /= 0) THEN
    message = "Failed to write integer_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim))
  swap_header = integer_constants
  status = f_shum_byteswap(swap_header, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap integer_constants ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write integer_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_integer_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_real_constants(                                          &
                              ff_id, real_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: real_constants(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(real_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(real_const_start)
END IF
dim = SIZE(real_constants)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim >= next_start)) THEN
  WRITE(message, "(3(A,I0))") "Cannot write real_constants with size ",        &
    dim," starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(real_const_start) = start
ff % fixed_length_header(real_const_dim) = dim

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) real_constants
  IF (status /= 0) THEN
    message = "Failed to write real_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim))
  swap_header = real_constants
  status = f_shum_byteswap(swap_header, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap real_constants ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write real_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_real_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_level_dependent_constants(                               &
                   ff_id, level_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: level_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim1
INTEGER(KIND=int64)    :: dim2
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:,:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(lev_dep_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(lev_dep_const_start)
END IF
dim1 = SIZE(level_dependent_constants, 1)
dim2 = SIZE(level_dependent_constants, 2)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim1*dim2 >= next_start)) THEN
  WRITE(message, "(3(A,I0))")                                                  &
    "Cannot write level_dependent_constants with size ", dim1*dim2,            &
    " starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(lev_dep_const_start) = start
ff % fixed_length_header(lev_dep_const_dim1) = dim1
ff % fixed_length_header(lev_dep_const_dim2) = dim2

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                &
                                                       level_dependent_constants
  IF (status /= 0) THEN
    message = "Failed to write level_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim1,dim2))
  swap_header = level_dependent_constants
  status = f_shum_byteswap(swap_header, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap level_dependent_constants ("//               &
                                                              TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write level_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_level_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_row_dependent_constants(                                 &
                     ff_id, row_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: row_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim1
INTEGER(KIND=int64)    :: dim2
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:,:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(row_dep_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(row_dep_const_start)
END IF
dim1 = SIZE(row_dependent_constants, 1)
dim2 = SIZE(row_dependent_constants, 2)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim1*dim2 >= next_start)) THEN
  WRITE(message, "(3(A,I0))")                                                  &
    "Cannot write row_dependent_constants with size ", dim1*dim2,              &
    " starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(row_dep_const_start) = start
ff % fixed_length_header(row_dep_const_dim1) = dim1
ff % fixed_length_header(row_dep_const_dim2) = dim2

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                &
                                                         row_dependent_constants
  IF (status /= 0) THEN
    message = "Failed to write row_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim1,dim2))
  swap_header = row_dependent_constants
  status = f_shum_byteswap(swap_header, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap row_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write row_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_row_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_column_dependent_constants(                              &
                  ff_id, column_dependent_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: column_dependent_constants(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim1
INTEGER(KIND=int64)    :: dim2
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:,:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(col_dep_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(col_dep_const_start)
END IF
dim1 = SIZE(column_dependent_constants, 1)
dim2 = SIZE(column_dependent_constants, 2)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim1*dim2 >= next_start)) THEN
  WRITE(message, "(3(A,I0))")                                                  &
    "Cannot write column_dependent_constants with size ", dim1*dim2,           &
    " starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(col_dep_const_start) = start
ff % fixed_length_header(col_dep_const_dim1) = dim1
ff % fixed_length_header(col_dep_const_dim2) = dim2

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                &
                                                      column_dependent_constants
  IF (status /= 0) THEN
    message = "Failed to write column_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim1,dim2))
  swap_header = column_dependent_constants
  status = f_shum_byteswap(swap_header, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap column_dependent_constants ("//              &
                                                              TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write column_dependent_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_column_dependent_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_additional_parameters(                                   &
                           ff_id, additional_parameters, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: additional_parameters(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim1
INTEGER(KIND=int64)    :: dim2
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:,:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(additional_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(additional_const_start)
END IF
dim1 = SIZE(additional_parameters, 1)
dim2 = SIZE(additional_parameters, 2)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim1*dim2 >= next_start)) THEN
  WRITE(message, "(3(A,I0))")                                                  &
    "Cannot write additional_parameters with size ", dim1*dim2,                &
    " starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(additional_const_start) = start
ff % fixed_length_header(additional_const_dim1) = dim1
ff % fixed_length_header(additional_const_dim2) = dim2

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)                &
                                                           additional_parameters
  IF (status /= 0) THEN
    message = "Failed to write additional_parameters ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim1,dim2))
  swap_header = additional_parameters
  status = f_shum_byteswap(swap_header, dim1*dim2, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap additional_parameters ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write additional_parameters ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_additional_parameters

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_extra_constants(                                         &
                                 ff_id, extra_constants, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: extra_constants(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(extra_const_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(extra_const_start)
END IF
dim = SIZE(extra_constants)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim >= next_start)) THEN
  WRITE(message, "(3(A,I0))") "Cannot write extra_constants with size ",       &
    dim," starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(extra_const_start) = start
ff % fixed_length_header(extra_const_dim) = dim

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) extra_constants
  IF (status /= 0) THEN
    message = "Failed to write extra_constants ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim))
  swap_header = extra_constants
  status = f_shum_byteswap(swap_header, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap extra_constants ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write extra_constants ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_extra_constants

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_temp_histfile(                                         &
                                 ff_id, temp_histfile, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: temp_histfile(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index, current dimension and start of next section
IF (ff % fixed_length_header(temp_histfile_start) < 0) THEN
  start = get_next_free_position(ff)
ELSE
  start = ff % fixed_length_header(temp_histfile_start)
END IF
dim = SIZE(temp_histfile)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim >= next_start)) THEN
  WRITE(message, "(3(A,I0))") "Cannot write temp_histfile with size ",         &
    dim," starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
ff % fixed_length_header(temp_histfile_start) = start
ff % fixed_length_header(temp_histfile_dim) = dim

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) temp_histfile
  IF (status /= 0) THEN
    message = "Failed to write temp_histfile ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim))
  swap_header = temp_histfile
  status = f_shum_byteswap(swap_header, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap temp_histfile ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write temp_histfile ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_temp_histfile

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_compressed_index(                                        &
                         ff_id, compressed_index, index, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
REAL(KIND=real64),   INTENT(IN)  :: compressed_index(:)
INTEGER(KIND=int64), INTENT(IN)  :: index
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: start
INTEGER(KIND=int64)    :: dim
INTEGER(KIND=int64)    :: next_start
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Get the start position and dimensions from the fixed_length_header
SELECT CASE(index)
  CASE(1_int64)
    start = ff % fixed_length_header(comp_field_index1_start)
  CASE(2_int64)
    start = ff % fixed_length_header(comp_field_index2_start)
  CASE(3_int64)
    start = ff % fixed_length_header(comp_field_index3_start)
  CASE DEFAULT
    status = 1_int64
    message = "Invalid compressed index requested, must be 1, 2 or 3"
    RETURN
END SELECT

! If the start isn't set already, take the next free position
IF (start < 0) THEN
  start = get_next_free_position(ff)
END IF

dim = SIZE(compressed_index)
next_start = get_next_populated_position(ff, start)

IF ((next_start > 0) .AND. (start - 1 + dim >= next_start)) THEN
  WRITE(message, "(3(A,I0))") "Cannot write compressed_index with size ",      &
    dim," starting at ", start," because next component starts at ", next_start
  status = 1_int64
  RETURN
END IF

! Set the start position and dimensions in the fixed_length_header
SELECT CASE(index)
  CASE(1_int64)
    ff % fixed_length_header(comp_field_index1_start) = start
    ff % fixed_length_header(comp_field_index1_dim)= dim
  CASE(2_int64)
    ff % fixed_length_header(comp_field_index2_start) = start
    ff % fixed_length_header(comp_field_index2_dim) = dim
  CASE(3_int64)
    ff % fixed_length_header(comp_field_index3_start) = start
    ff % fixed_length_header(comp_field_index3_dim) = dim
END SELECT

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message) compressed_index
  IF (status /= 0) THEN
    message = "Failed to write compressed_index ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(dim))
  swap_header = compressed_index
  status = f_shum_byteswap(swap_header, dim, 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap compressed_index ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to write compressed_index ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_compressed_index

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_lookup(                                                  &
                         ff_id, lookup, start_index, message) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: lookup(:, :)
INTEGER(KIND=int64), INTENT(IN)  :: start_index
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64)    :: saved_entries(4, SIZE(lookup, 2))
INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: dim1
INTEGER(KIND=int64)    :: dim2
TYPE(ff_type), POINTER :: ff

status = 0_int64
message = ""

! Retrieve stored fixed length header index
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate the size of the passed lookup section
dim1 = SIZE(lookup, 1)
dim2 = SIZE(lookup, 2)

IF (dim1 /= f_shum_lookup_dim1_len) THEN
    WRITE(message, "(2(A,I0))")                                                &
    "Only lookups with length ", f_shum_lookup_dim1_len,                       &
    " are supported but passed lookup has length ", dim1
  status = 1_int64
  RETURN
END IF

IF (start_index + dim2 - 1 > SIZE(ff % lookup, 2)) THEN
  WRITE(message, "(3(A,I0),A)")                                                &
    "Cannot write ", dim2, " lookup table entries starting at lookup ",        &
    start_index, " because file only has ", SIZE(ff % lookup, 2),              &
    " reserved headers available"
  status = 1_int64
  RETURN
END IF

! Store the existing positional header elements from the private lookup array
saved_entries(1, :) = ff % lookup(lblrec,  start_index:start_index + dim2 - 1)
saved_entries(2, :) = ff % lookup(lbnrec,  start_index:start_index + dim2 - 1)
saved_entries(3, :) = ff % lookup(lbegin,  start_index:start_index + dim2 - 1)
saved_entries(4, :) = ff % lookup(lbuser2, start_index:start_index + dim2 - 1)

! Write the given lookups to the private array
ff % lookup(:, start_index:start_index + dim2 - 1) = lookup(:, :)

! Replace the positional values (discard the positions in the user array)
ff % lookup(lblrec,  start_index:start_index + dim2 - 1) = saved_entries(1, :)
ff % lookup(lbnrec,  start_index:start_index + dim2 - 1) = saved_entries(2, :)
ff % lookup(lbegin,  start_index:start_index + dim2 - 1) = saved_entries(3, :)
ff % lookup(lbuser2, start_index:start_index + dim2 - 1) = saved_entries(4, :)

status = 0_int64

END FUNCTION f_shum_write_lookup

!------------------------------------------------------------------------------!

FUNCTION commit_lookup(ff, message) RESULT(status)
IMPLICIT NONE

TYPE(ff_type)       :: ff
CHARACTER(LEN=*)    :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: start

INTEGER(KIND=int64), ALLOCATABLE :: swap_header(:,:)

status = 0_int64
message = ""

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Calculate start index of lookup table if not already written
IF (ff % fixed_length_header(lookup_start) < 0) THEN
  start = get_next_free_position(ff)
  ff % fixed_length_header(lookup_start) = start
ELSE
  start = ff % fixed_length_header(lookup_start)
END IF

! Calculate the dimension of the data (if a sequential file)
IF (ff % next_unwritten_field > 1) THEN
  ff % fixed_length_header(data_dim) =                                         &
    ff % lookup(lbegin, ff % next_unwritten_field -1) +                        &
      ff % lookup(lbnrec, ff % next_unwritten_field -1)
END If

IF (ff % native_endian) THEN
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     ff % lookup
  IF (status /= 0) THEN
    message = "Failed to commit lookup to disk ("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(ff % lookup, 1), SIZE(ff % lookup, 2)))
  swap_header = ff % lookup
  status = f_shum_byteswap(                                                    &
                   swap_header, SIZE(ff % lookup, KIND=int64), 8_int64, message)
  IF (status /= 0) THEN
    message = "Failed to byteswap lookup ("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start-1)*8+1, IOSTAT=status, IOMSG=message)       &
                                                                     swap_header
  IF (status /= 0) THEN
    message = "Failed to commit lookup to disk ("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION commit_lookup

!------------------------------------------------------------------------------!

FUNCTION f_shum_precalc_data_positions(                                        &
         ff_id, max_points, message, n_land_points, n_sea_points) RESULT(status)
IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: max_points
CHARACTER(LEN=*),    INTENT(OUT) :: message
INTEGER(KIND=int64), INTENT(IN),                                               &
                     OPTIONAL    :: n_land_points
INTEGER(KIND=int64), INTENT(IN),                                               &
                     OPTIONAL    :: n_sea_points

INTEGER(KIND=int64)    :: status
INTEGER(KIND=int64)    :: i
INTEGER(KIND=int64)    :: lbpack_n1
INTEGER(KIND=int64)    :: lbpack_n2
INTEGER(KIND=int64)    :: lbpack_n3
INTEGER(KIND=int64)    :: field_size
INTEGER(KIND=int64)    :: field_data_start
TYPE(ff_type), POINTER :: ff

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Check if the lookup start position is set
IF (ff % fixed_length_header(lookup_start) < 0) THEN
  ff % fixed_length_header(lookup_start) = get_next_free_position(ff)
END IF

! To avoid having a test on the first field within the loop, set the start
! position of the first field before the loop
field_data_start =                                                             &
               ff % fixed_length_header(lookup_start) +                        &
               ff % fixed_length_header(lookup_dim1)*                          &
               ff % fixed_length_header(lookup_dim2)

! Round this to a data alignment boundary
field_data_start = data_start_alignment*                                       &
                             ((field_data_start / data_start_alignment) + 1)
ff % fixed_length_header(data_start) = field_data_start + 1

! Loop through the lookups
DO i = 1, SIZE(ff % lookup, 2)

  ! An unmodified header release in a field signifies the end of the valid
  ! fields in the file
  IF (ff % lookup(lbrel, i) == -99) EXIT

  ! Split the N1 - N3 digits from the packing code
  lbpack_n1 = MOD(ff % lookup(lbpack, i), 10_int64)
  lbpack_n2 = MOD(ff % lookup(lbpack, i)/10, 10_int64)
  lbpack_n3 = MOD(ff % lookup(lbpack, i)/100, 10_int64)

  ! The maximum number of points passed is the default for all fields
  field_size = max_points

  ! Unless they are specified as a land/sea packed field, and an alternative
  ! field size for these cases has been provided - it's fine if nothing is
  ! matched here, because the above size should always been big enough for a
  ! smaller field if required
  IF (lbpack_n2 == 2_int64) THEN
    IF (PRESENT(n_land_points) .AND.                                           &
        (lbpack_n3 == 1_int64)) THEN
      field_size = n_land_points
    END IF
    IF (PRESENT(n_sea_points) .AND.                                           &
        (lbpack_n3 == 2_int64)) THEN
      field_size = n_sea_points
    END IF
  END IF

  ! If the positional elements are already set, abort - something isn't right
  IF ((ff % lookup(lbegin, i) /= -99) .OR.                                     &
      (ff % lookup(lbnrec, i) /= -99)) THEN
    status = 1_int64
    WRITE(message, "(A,I0,A)")                                                 &
      "Lookup ", i, " already contains positional information"
    RETURN
  END IF

  ! Set lbegin based on the calculations above (note that the data
  ! start is calculated before the main loop for the first field)
  ff % lookup(lbegin, i) = field_data_start
  ! Set lbuser2 - this is similar to lbegin but is offset so that it starts
  ! from the beginning of the data block
  ff % lookup(lbuser2, i) = field_data_start -                                 &
                            ff % fixed_length_header(data_start) + 2
  ! The the field size for 32-bit truncated fields is in 32-bit words, so
  ! the record size for padding must be halved
  IF (lbpack_n1 == 2) THEN
    field_size = (field_size + 1)/2
  END IF

  ! Round up the field with padding to meet the boundary, and set LBNREC
  ff % lookup(lbnrec, i) = field_padding*((field_size / field_padding) + 1)

  ! Now calculate the start position for the next field
  field_data_start = ff % lookup(lbegin, i) + ff % lookup(lbnrec, i)

END DO

! Once we have hit the end of the populated lookups, set the final size of the
! data dimension from the final field which was written
ff % fixed_length_header(data_dim) =                                           &
                             ff % lookup(lbegin, i-1) + ff % lookup(lbnrec, i-1)

END FUNCTION f_shum_precalc_data_positions

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_direct_real64                                 &
                (ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: index
REAL(KIND=real64),   INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Get start position for write
start = ff % lookup(lbegin, index)

! Check that the file is setup for a direct write
IF (start == imdi) THEN
  status = 1_int64
  WRITE(message, "(A,I0,A)")                                                   &
    "Field ", index, " cannot be written directly; the positional headers "  //&
    "are not set (they must be precalculated to write directly)"
END IF

! Check that the provided data won't overflow the reserved space
IF (SIZE(field_data) > ff % lookup(lbnrec, index)) THEN
  status = 1_int64
  WRITE(message, "(3(A,I0))")                                                  &
    "Field ", index, " data larger than pre-calculated LBNREC value; data "  //&
    "is ", SIZE(field_data), " words, but LBNREC is ",                         &
    ff % lookup(lbnrec, index)
  RETURN
END IF

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of lblrec based on the exact size of the field...
ff % lookup(lblrec, index) = SIZE(field_data)

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being written is correct; in this case the return array
  ! is 64-bit, so any 32-bit types require double the number of 64-bit words
  IF (lbpack_n1 == 2) THEN
    ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 8_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_field_data_direct_real64

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_direct_int64                                  &
                (ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: index
INTEGER(KIND=int64), INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

INTEGER(KIND=int64), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Get start position for write
start = ff % lookup(lbegin, index)

! Check that the file is setup for a direct write
IF (start == imdi) THEN
  status = 1_int64
  WRITE(message, "(A,I0,A)")                                                   &
    "Field ", index, " cannot be written directly; the positional headers "  //&
    "are not set (they must be precalculated to write directly)"
END IF

! Check that the provided data won't overflow the reserved space
IF (SIZE(field_data) > ff % lookup(lbnrec, index)) THEN
  status = 1_int64
  WRITE(message, "(3(A,I0))")                                                  &
    "Field ", index, " data larger than pre-calculated LBNREC value; data "  //&
    "is ", SIZE(field_data), " words, but LBNREC is ",                         &
    ff % lookup(lbnrec, index)
  RETURN
END IF

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of lblrec based on the exact size of the field
ff % lookup(lblrec, index) = SIZE(field_data)

IF (ignore_dtype_local) THEN
  ! If the data type is being ignored, make any required adjustments to ensure
  ! the size of data being written is correct; in this case the return array
  ! is 64-bit, so any 32-bit types require double the number of 64-bit words
  IF (lbpack_n1 == 2) THEN
    ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
  END IF
ELSE
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF ((data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 8_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

END FUNCTION f_shum_write_field_data_direct_int64

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_direct_real32                                 &
                (ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: index
REAL(KIND=real32),   INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

REAL(KIND=real32), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Get start position for write
start = ff % lookup(lbegin, index)

! Check that the file is setup for a direct write
IF (start == imdi) THEN
  status = 1_int64
  WRITE(message, "(A,I0,A)")                                                   &
    "Field ", index, " cannot be written directly; the positional headers "  //&
    "are not set (they must be precalculated to write directly)"
END IF

! Check that the provided data won't overflow the reserved space
IF ((SIZE(field_data) + 1)/2 > ff % lookup(lbnrec, index)) THEN
  status = 1_int64
  WRITE(message, "(3(A,I0))")                                                  &
    "Field ", index, " data larger than pre-calculated LBNREC value; data "  //&
    "is ", (SIZE(field_data) + 1)/2, " words, but LBNREC is ",                 &
    ff % lookup(lbnrec, index)
  RETURN
END IF

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of lblrec based on the exact size of the field
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = SIZE(field_data)
ELSE
  ff % lookup(lblrec, index) = (SIZE(field_data) + 1)/2
END IF

IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 2) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 4_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! For 32-bit data, ensure the amount written out is still divisible by whole
! 64-bit words (as that is the record size of the files) do this by adding
! an extra zero
IF (MOD(SIZE(field_data), 2) /= 0) THEN
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) 0.0_real32
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write extra zero for field ", index, &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_write_field_data_direct_real32

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_direct_int32                                  &
                (ff_id, index, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: index
INTEGER(KIND=int32), INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

INTEGER(KIND=int32), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Get start position for write
start = ff % lookup(lbegin, index)

! Check that the file is setup for a direct write
IF (start == imdi) THEN
  status = 1_int64
  WRITE(message, "(A,I0,A)")                                                   &
    "Field ", index, " cannot be written directly; the positional headers "  //&
    "are not set (they must be precalculated to write directly)"
END IF

! Check that the provided data won't overflow the reserved space
IF ((SIZE(field_data) + 1)/2 > ff % lookup(lbnrec, index)) THEN
  status = 1_int64
  WRITE(message, "(3(A,I0))")                                                  &
    "Field ", index, " data larger than pre-calculated LBNREC value; data "  //&
    "is ", (SIZE(field_data) + 1)/2, " words, but LBNREC is ",                 &
    ff % lookup(lbnrec, index)
  RETURN
END IF

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of lblrec based on the exact size of the field
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = SIZE(field_data)
ELSE
  ff % lookup(lblrec, index) = (SIZE(field_data) + 1)/2
END IF


IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF ((lbpack_n1 /= 2) .AND. (lbpack_n1 /= 1)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF ((lbpack_n1 == 2) .AND. (data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 4_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! For 32-bit data, ensure the amount written out is still divisible by whole
! 64-bit words (as that is the record size of the files) do this by adding
! an extra zero
IF (MOD(SIZE(field_data), 2) /= 0) THEN
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) 0_int32
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write extra zero for field ", index, &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
END IF

END FUNCTION f_shum_write_field_data_direct_int32

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_sequential_real64                             &
               (ff_id, lookup, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: lookup(f_shum_lookup_dim1_len)
REAL(KIND=real64),   INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: index
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

REAL(KIND=real64), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Retrieve the index of the next field to write
index = ff % next_unwritten_field

! Initialise the given lookup entry with the provided lookup data
ff % lookup(:, index) = lookup

! Get the index of the next lookup to be written
IF (index > 1) THEN
  start = ff % lookup(lbegin, index - 1) + ff % lookup(lbnrec, index - 1)
ELSE
  ff % fixed_length_header(lookup_start) = get_next_free_position(ff)
  start = ff % fixed_length_header(lookup_start) +                             &
          ff % fixed_length_header(lookup_dim1)*                               &
          ff % fixed_length_header(lookup_dim2)
  start = data_start_alignment*((start / data_start_alignment) + 1) + 1
  ff % fixed_length_header(data_start) = start
  start = start - 1
END IF

! Save the start value as LBEGIN
ff % lookup(lbegin, index) = start
! Set the value of LBUSER2 (similar to LBEGIN, but offset so it is relative
! to the start of the data block)
ff % lookup(lbuser2, index) = start - ff % fixed_length_header(data_start) + 2

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of LBLREC based on the exact size of the field...
ff % lookup(lblrec, index) = SIZE(field_data)

IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Setup the value of LBNREC, including the padding
ff % lookup(lbnrec, index) =                                                   &
                field_padding*((ff % lookup(lblrec, index) / field_padding) + 1)

! For 32-bit unpacked fields the LBLREC value is given in 32-bit words
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 8_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! Increment the index
ff % next_unwritten_field = index + 1

END FUNCTION f_shum_write_field_data_sequential_real64

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_sequential_int64                              &
               (ff_id, lookup, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: lookup(f_shum_lookup_dim1_len)
INTEGER(KIND=int64), INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: index
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

INTEGER(KIND=int64), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Retrieve the index of the next field to write
index = ff % next_unwritten_field

! Initialise the given lookup entry with the provided lookup data
ff % lookup(:, index) = lookup

! Get the index of the next lookup to be written
IF (index > 1) THEN
  start = ff % lookup(lbegin, index - 1) + ff % lookup(lbnrec, index - 1)
ELSE
  ff % fixed_length_header(lookup_start) = get_next_free_position(ff)
  start = ff % fixed_length_header(lookup_start) +                             &
          ff % fixed_length_header(lookup_dim1)*                               &
          ff % fixed_length_header(lookup_dim2)
  start = data_start_alignment*((start / data_start_alignment) + 1) + 1
  ff % fixed_length_header(data_start) = start
  start = start - 1
END IF

! Save the start value as LBEGIN
ff % lookup(lbegin, index) = start
! Set the value of LBUSER2 (similar to LBEGIN, but offset so it is relative
! to the start of the data block)
ff % lookup(lbuser2, index) = start - ff % fixed_length_header(data_start) + 2

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of LBLREC based on the exact size of the field...
ff % lookup(lblrec, index) = SIZE(field_data)

IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 0) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 64-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF ((data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Setup the value of LBNREC, including the padding
ff % lookup(lbnrec, index) =                                                   &
                field_padding*((ff % lookup(lblrec, index) / field_padding) + 1)

! For 32-bit unpacked fields the LBLREC value is given in 32-bit words
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 8_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! Increment the index
ff % next_unwritten_field = index + 1

END FUNCTION f_shum_write_field_data_sequential_int64

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_sequential_real32                             &
               (ff_id, lookup, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: lookup(f_shum_lookup_dim1_len)
REAL(KIND=real32)  , INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: index
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

REAL(KIND=real32), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Retrieve the index of the next field to write
index = ff % next_unwritten_field

! Initialise the given lookup entry with the provided lookup data
ff % lookup(:, index) = lookup

! Get the index of the next lookup to be written
IF (index > 1) THEN
  start = ff % lookup(lbegin, index - 1) + ff % lookup(lbnrec, index - 1)
ELSE
  ff % fixed_length_header(lookup_start) = get_next_free_position(ff)
  start = ff % fixed_length_header(lookup_start) +                             &
          ff % fixed_length_header(lookup_dim1)*                               &
          ff % fixed_length_header(lookup_dim2)
  start = data_start_alignment*((start / data_start_alignment) + 1) + 1
  ff % fixed_length_header(data_start) = start
  start = start - 1
END IF

! Save the start value as LBEGIN
ff % lookup(lbegin, index) = start
! Set the value of LBUSER2 (similar to LBEGIN, but offset so it is relative
! to the start of the data block)
ff % lookup(lbuser2, index) = start - ff % fixed_length_header(data_start) + 2

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of LBLREC based on the exact size of the field...
ff % lookup(lblrec, index) = (SIZE(field_data) + 1)/2

IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF (lbpack_n1 /= 2) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit REAL array, but "                         //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF (data_type /= 1) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with REAL array, but "                          //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Setup the value of LBNREC, including the padding
ff % lookup(lbnrec, index) =                                                   &
                field_padding*((ff % lookup(lblrec, index) / field_padding) + 1)

! For 32-bit unpacked fields the LBLREC value is given in 32-bit words
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 4_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! For 32-bit data, ensure the amount written out is still divisible by whole
! 64-bit words (as that is the record size of the files) do this by adding
! an extra zero
IF (MOD(SIZE(field_data), 2) /= 0) THEN
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) 0.0_real32
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write extra zero for field ", index, &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
END IF

! Increment the index
ff % next_unwritten_field = index + 1

END FUNCTION f_shum_write_field_data_sequential_real32

!------------------------------------------------------------------------------!

FUNCTION f_shum_write_field_data_sequential_int32                              &
               (ff_id, lookup, field_data, message, ignore_dtype) RESULT(status)
IMPLICIT NONE
INTEGER(KIND=int64), INTENT(IN)  :: ff_id
INTEGER(KIND=int64), INTENT(IN)  :: lookup(f_shum_lookup_dim1_len)
INTEGER(KIND=int32), INTENT(IN)  :: field_data(:)
CHARACTER(LEN=*),    INTENT(OUT) :: message
LOGICAL(KIND=bool),  INTENT(IN),                                               &
                     OPTIONAL    :: ignore_dtype

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: start
INTEGER(KIND=int64) :: pack_type
INTEGER(KIND=int64) :: data_type
INTEGER(KIND=int64) :: index
INTEGER(KIND=int64) :: lbpack_n1
LOGICAL             :: ignore_dtype_local
TYPE(ff_type), POINTER :: ff

INTEGER(KIND=int32), ALLOCATABLE :: swap_header(:)

! Set status for successful exit
status = 0_int64
message = ""

! Retrieve stored lookup
ff => unique_id_to_ff(ff_id)

IF (ff % read_only) THEN
  status = 1_int64
  message = "Attempted write command in read-only mode"
  RETURN
END IF

! Retrieve the index of the next field to write
index = ff % next_unwritten_field

! Initialise the given lookup entry with the provided lookup data
ff % lookup(:, index) = lookup

! Get the index of the next lookup to be written
IF (index > 1) THEN
  start = ff % lookup(lbegin, index - 1) + ff % lookup(lbnrec, index - 1)
ELSE
  ff % fixed_length_header(lookup_start) = get_next_free_position(ff)
  start = ff % fixed_length_header(lookup_start) +                             &
          ff % fixed_length_header(lookup_dim1)*                               &
          ff % fixed_length_header(lookup_dim2)
  start = data_start_alignment*((start / data_start_alignment) + 1) + 1
  ff % fixed_length_header(data_start) = start
  start = start - 1
END IF

! Save the start value as LBEGIN
ff % lookup(lbegin, index) = start
! Set the value of LBUSER2 (similar to LBEGIN, but offset so it is relative
! to the start of the data block)
ff % lookup(lbuser2, index) = start - ff % fixed_length_header(data_start) + 2

! Get the N1 digit of the packing code
pack_type = ff % lookup(lbpack, index)
lbpack_n1 = MOD(pack_type, 10_int64)

! Pickup flag for ignoring data-type if present
ignore_dtype_local = .FALSE.
IF (PRESENT(ignore_dtype)) ignore_dtype_local = ignore_dtype

! Set the value of LBLREC based on the exact size of the field...
ff % lookup(lblrec, index) = (SIZE(field_data) + 1)/2

IF (.NOT. ignore_dtype_local) THEN
  ! If the data type isn't being ignored, first check the packing code agress
  ! with the type of the passed data (64-bit)
  IF ((lbpack_n1 /= 2) .AND. (lbpack_n1 /= 1)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between packing code in lookup and kind of field_data array "//&
      "(called routine with 32-bit INTEGER array, but "                      //&
      TRIM(describe_allowed_kind_based_on_lbpack(pack_type))//")"
    RETURN
  END IF
  ! And then check that the data type itself is correct
  data_type = ff % lookup(lbuser1, index)
  IF ((lbpack_n1 == 2) .AND. (data_type /= 2) .AND. (data_type /= 3)) THEN
    status = 1_int64
    WRITE(message, "(A)")                                                      &
      "Mismatch between data-type code in lookup and type of field_data "    //&
      "array (called routine with INTEGER array, but "                       //&
      TRIM(describe_allowed_type_based_on_lbuser1(data_type))//")"
    RETURN
  END IF
END IF

! Setup the value of LBNREC, including the padding
ff % lookup(lbnrec, index) =                                                   &
                field_padding*((ff % lookup(lblrec, index) / field_padding) + 1)

! For 32-bit unpacked fields the LBLREC value is given in 32-bit words
IF (lbpack_n1 == 2) THEN
  ff % lookup(lblrec, index) = ff % lookup(lblrec, index)*2
END IF

IF (ff % native_endian) THEN
  ! Now write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) field_data
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
ELSE
  ! If the file is to be output in the opposite byte ordering, use a temporary
  ! array to perform a byteswap before outputting to the file
  ALLOCATE(swap_header(SIZE(field_data)))
  swap_header = field_data
  status = f_shum_byteswap(                                                    &
                    swap_header, SIZE(field_data, KIND=int64), 4_int64, message)
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to byteswap data for field ", index,    &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  ! Write the data (TODO: replace this with proper "buffout" and
  ! "setpos" calls once portio makes it into Shumlib)
  WRITE(ff % unique_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message)         &
                                                                     swap_header
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write data for field ", index,       &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
  DEALLOCATE(swap_header)
END IF

! For 32-bit data, ensure the amount written out is still divisible by whole
! 64-bit words (as that is the record size of the files) do this by adding
! an extra zero
IF (MOD(SIZE(field_data), 2) /= 0) THEN
  WRITE(ff_id, POS=(start)*8+1, IOSTAT=status, IOMSG=message) 0_int32
  IF (status /= 0) THEN
    WRITE(message, "(A,I0,A)") "Failed to write extra zero for field ", index, &
                                                         "("//TRIM(message)//")"
    RETURN
  END IF
END IF

! Increment the index
ff % next_unwritten_field = index + 1

END FUNCTION f_shum_write_field_data_sequential_int32

!------------------------------------------------------------------------------!

END MODULE f_shum_fieldsfile_mod
