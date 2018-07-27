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
! Description: Methods for reading the UM STASHmaster.
!
MODULE f_shum_stashmaster_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_DOUBLE, C_FLOAT, C_INT8_T

IMPLICIT NONE 

PRIVATE

PUBLIC ::                                                                      &
  shum_STASHmaster, f_shum_read_stashmaster, f_shum_add_new_stash_record

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
  INTEGER, PARAMETER :: int8   = C_INT8_T
  INTEGER, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PARAMETER :: real32 = C_FLOAT 
!------------------------------------------------------------------------------!

! This type stores the records defined by the stashmaster for a single
! STASH entry
TYPE shum_STASHmaster_record
  INTEGER(KIND=int64) :: model
  INTEGER(KIND=int64) :: section
  INTEGER(KIND=int64) :: item
  CHARACTER(LEN=36)   :: name
  INTEGER(KIND=int64) :: space
  INTEGER(KIND=int64) :: point
  INTEGER(KIND=int64) :: time
  INTEGER(KIND=int64) :: grid
  INTEGER(KIND=int64) :: levelt
  INTEGER(KIND=int64) :: levelf
  INTEGER(KIND=int64) :: levell
  INTEGER(KIND=int64) :: pseudt
  INTEGER(KIND=int64) :: pseudf
  INTEGER(KIND=int64) :: pseudl
  INTEGER(KIND=int64) :: levcom
  INTEGER(KIND=int8)  :: option(30)
  INTEGER(KIND=int8)  :: version_mask(20)
  INTEGER(KIND=int64) :: halo
  INTEGER(KIND=int64) :: datat
  INTEGER(KIND=int64) :: dumpp
  INTEGER(KIND=int64) :: packing_codes(10)
  INTEGER(KIND=int64) :: rotate
  INTEGER(KIND=int64) :: ppfc
  INTEGER(KIND=int64) :: user
  INTEGER(KIND=int64) :: lbvc
  INTEGER(KIND=int64) :: blev
  INTEGER(KIND=int64) :: tlev
  INTEGER(KIND=int64) :: rblevv
  INTEGER(KIND=int64) :: cfll
  INTEGER(KIND=int64) :: cfff
END TYPE shum_STASHmaster_record

! This type provides a pointer to the above point - used to enable the 
! STASHmaster to be an array of pointers (with elements that have no matching
! STASH entry being left unassociated)
TYPE shum_STASHmaster
  TYPE(shum_STASHmaster_record), POINTER :: record => NULL()
END TYPE shum_STASHmaster

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION f_shum_add_new_stash_record(                                          &
  STASHmaster, model, section, item, name, space, point, time, grid, levelt,   &
  levelf, levell, pseudt, pseudf, pseudl, levcom, option, version_mask, halo,  &
  datat, dumpp, packing_codes, rotate, ppfc, user, lbvc, blev, tlev, rblevv,   &
  cfll, cfff, message) RESULT(status)

IMPLICIT NONE 

TYPE(shum_STASHmaster), INTENT(INOUT) :: STASHmaster(99999)
INTEGER(KIND=int64),    INTENT(IN)    :: model, section, item, space, point,   &
                                         time, grid, levelt, levelf, levell,   &
                                         pseudt, pseudf, pseudl, levcom,       &
                                         halo, datat, dumpp, rotate, ppfc,     &
                                         user, lbvc, blev, tlev, rblevv, cfll, &
                                         cfff, packing_codes(10)
INTEGER(KIND=int8),     INTENT(IN)    :: option(30), version_mask(20)
CHARACTER(LEN=36),      INTENT(IN)    :: name
CHARACTER(LEN=*),       INTENT(OUT)   :: message

INTEGER(KIND=int64) :: status
INTEGER(KIND=int64) :: stash

status = 0_int64
message = ""

! Get the stashcode - this is the index into the STASHmaster where the record
! will go
stash = section*1000 + item
IF ((stash < 0) .OR. (stash > SIZE(STASHmaster))) THEN
  message = "STASH number provided too large for STASHmaster array"
  status = 1_int64
  RETURN
END IF
! Get rid of any existing entry in that location
IF (ASSOCIATED(STASHmaster(stash) % record)) THEN
  DEALLOCATE(STASHmaster(stash) % record)
END IF
NULLIFY(STASHmaster(stash) % record)
ALLOCATE(STASHmaster(stash) % record)

STASHmaster(stash) % record % model         = model
STASHmaster(stash) % record % section       = section
STASHmaster(stash) % record % item          = item
STASHmaster(stash) % record % name          = name
STASHmaster(stash) % record % space         = space
STASHmaster(stash) % record % point         = point
STASHmaster(stash) % record % time          = time
STASHmaster(stash) % record % grid          = grid
STASHmaster(stash) % record % levelt        = levelt
STASHmaster(stash) % record % levelf        = levelf
STASHmaster(stash) % record % levell        = levell
STASHmaster(stash) % record % pseudt        = pseudt
STASHmaster(stash) % record % pseudf        = pseudf
STASHmaster(stash) % record % pseudl        = pseudl
STASHmaster(stash) % record % levcom        = levcom
STASHmaster(stash) % record % option        = option
STASHmaster(stash) % record % version_mask  = version_mask
STASHmaster(stash) % record % halo          = halo
STASHmaster(stash) % record % datat         = datat
STASHmaster(stash) % record % dumpp         = dumpp
STASHmaster(stash) % record % packing_codes = packing_codes
STASHmaster(stash) % record % rotate        = rotate
STASHmaster(stash) % record % ppfc          = ppfc
STASHmaster(stash) % record % user          = user
STASHmaster(stash) % record % lbvc          = lbvc
STASHmaster(stash) % record % blev          = blev
STASHmaster(stash) % record % tlev          = tlev
STASHmaster(stash) % record % rblevv        = rblevv
STASHmaster(stash) % record % cfll          = cfll
STASHmaster(stash) % record % cfff          = cfff

END FUNCTION f_shum_add_new_stash_record

!------------------------------------------------------------------------------!

FUNCTION f_shum_read_stashmaster(stashmaster_path, stashmaster, message)       &
                                                                  RESULT(status)

IMPLICIT NONE 

CHARACTER(LEN=*),       INTENT(IN)    :: stashmaster_path
TYPE(shum_STASHmaster), INTENT(INOUT) :: STASHmaster(99999)
CHARACTER(LEN=*),       INTENT(OUT)   :: message

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64) :: i, j
INTEGER(KIND=int64) :: file_unit
LOGICAL             :: is_open
REAL(KIND=real64)   :: rand
CHARACTER(LEN=2)    :: record_start

INTEGER(KIND=int64) :: model, section, item, space, point, time, grid, levelt, &
                       levelf, levell, pseudt, pseudf, pseudl, levcom,         &
                       halo, datat, dumpp, rotate, ppfc, user,                 &
                       lbvc, blev, tlev, rblevv, cfll, cfff, packing_codes(10)
INTEGER(KIND=int8)  :: option(30), version_mask(20)
CHARACTER(LEN=36)   :: name

! Find an un-used file unit (use the same range as the rest of the API)
is_open = .TRUE.
DO WHILE (is_open)
  CALL RANDOM_NUMBER(rand)
  file_unit = 50000 + FLOOR(10000*rand)
  INQUIRE(UNIT=file_unit, OPENED=is_open)
END DO

! Open the file
OPEN(UNIT=file_unit, FILE=stashmaster_path, ACTION='READ', IOSTAT=status,      &
     IOMSG=message)
IF (status /= 0) THEN
  message = "Failed to open file ("//TRIM(message)//")"
  RETURN
END IF

! Start reading the file
DO
  ! Read the first 2 characters on the lines, a legitimate record will start
  ! with the text "1|"
  READ(file_unit, '(A2)') record_start

  IF (record_start == '1|') THEN
    ! Back-up to re-read the record properly
    BACKSPACE file_unit
    
    ! Record line 1 
    READ(file_unit, "(2X,3(I5,2X),A36)", IOSTAT=status, IOMSG=message)         &
                    model, section, item, name
    IF (status /= 0) THEN
      message = "Failed to read line 1 of record, message: "//TRIM(message)
      RETURN
    END IF

    ! Detect end of file record
    IF ((model == -1) .AND. (section == -1) .AND. (item == -1) .AND.           &
         TRIM(name) == "END OF FILE MARK") THEN
      EXIT
    END IF

    ! Record line 2
    READ(file_unit, "(2X,11(I5,2X))", IOSTAT=status, IOMSG=message)            &
                    space, point, time, grid, levelt, levelf, levell, pseudt,  &
                    pseudf, pseudl, levcom
    IF (status /= 0) THEN
      message = "Failed to read line 2 of record, message: "//TRIM(message)
      RETURN
    END IF

    ! Record line 3
    READ(file_unit,                                                            &
         "(3X,30(I1),3X,20(I1),2X,I5)", IOSTAT=status, IOMSG=message)          &
                    (option(i), i=1,30), (version_mask(j), j=1,20), halo
    IF (status /= 0) THEN
      message = "Failed to read line 3 of record, message: "//TRIM(message)
      RETURN
    END IF

    ! Record line 4
    READ(file_unit,                                                            &
         "(2X,I5,2X,I5,3X,I3,9(2X,i3))", IOSTAT=status, IOMSG=message)         &
                    datat, dumpp, (packing_codes(i), i=1,10)
    IF (status /= 0) THEN
      message = "Failed to read line 4 of record, message: "//TRIM(message)
      RETURN
    END IF

    ! Record line 5
    READ(file_unit, "(2X,9(I5,2X))", IOSTAT=status, IOMSG=message)             &
                    rotate, ppfc, user, lbvc, blev, tlev, rblevv, cfll, cfff
    IF (status /= 0) THEN
      message = "Failed to read line 5 of record, message: "//TRIM(message)
      RETURN
    END IF

    ! Now populate the STASHmaster type
    status = f_shum_add_new_stash_record(                                      &
      STASHmaster, model, section, item, name, space, point, time, grid,       &
      levelt, levelf, levell, pseudt, pseudf, pseudl, levcom, option,          &
      version_mask, halo, datat, dumpp, packing_codes, rotate, ppfc, user,     &
      lbvc, blev, tlev, rblevv, cfll, cfff, message)

    IF (status /= 0) THEN
      RETURN
    END IF
  END IF
END DO

CLOSE(file_unit, IOSTAT=status, IOMSG=message)

IF (status /= 0) THEN
  message = "Failed to close STASHmaster, message: "//TRIM(message)
END IF

END FUNCTION f_shum_read_stashmaster

END MODULE f_shum_stashmaster_mod

!------------------------------------------------------------------------------!
