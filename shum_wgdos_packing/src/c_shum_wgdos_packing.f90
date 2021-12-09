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

MODULE c_shum_wgdos_packing_mod

USE f_shum_string_conv_mod,   ONLY: f_shum_f2c_string
USE f_shum_wgdos_packing_mod, ONLY:                                            &
    f_shum_wgdos_pack, f_shum_wgdos_unpack, f_shum_read_wgdos_header

USE, INTRINSIC :: iso_c_binding, ONLY: &
    C_F_POINTER, C_LOC, C_INT64_T, C_INT32_T, C_CHAR, C_FLOAT, C_DOUBLE, C_PTR

IMPLICIT NONE 

! Note - this module (intentionally) has nothing set to PUBLIC - this is because
! it shouldn't ever be accessed from Fortran and only exists to provide the
! interface to access the routines from C (which is unaffected by Fortran's
! PRIVATE attribute)
PRIVATE

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

!------------------------------------------------------------------------------!

FUNCTION c_shum_read_wgdos_header(                                             &
    comp_field_ptr, num_words, accuracy, cols, rows, cmessage, message_len)    &
    RESULT(status) BIND(c, NAME="c_shum_read_wgdos_header")

IMPLICIT NONE

INTEGER(KIND=C_INT64_T)      :: status

TYPE(C_PTR), VALUE,            INTENT(IN)  :: comp_field_ptr
INTEGER(KIND=C_INT64_T),       INTENT(OUT) :: num_words
INTEGER(KIND=C_INT64_T),       INTENT(OUT) :: accuracy
INTEGER(KIND=C_INT64_T),       INTENT(OUT) :: cols
INTEGER(KIND=C_INT64_T),       INTENT(OUT) :: rows
INTEGER(KIND=C_INT64_T),       INTENT(IN)  :: message_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT) :: cmessage(message_len + 1)

CHARACTER(LEN=message_len)   :: message
INTEGER(KIND=int32), POINTER :: comp_field(:)

CALL C_F_POINTER(comp_field_ptr, comp_field, [3])

message = ""
status = f_shum_read_wgdos_header(                                             &
    comp_field, num_words, accuracy, cols, rows, message)

IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(TRIM(message))
END IF

END FUNCTION c_shum_read_wgdos_header

!------------------------------------------------------------------------------!

FUNCTION c_shum_wgdos_pack(field, cols, rows, acc, rmdi, comp_field, len_comp, &
                           num_words, cmessage, message_len)                   &
                           RESULT(status)                                      &
                           BIND(c, NAME="c_shum_wgdos_pack")
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) :: status

INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: cols
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: rows
REAL(KIND=C_DOUBLE),           INTENT(IN), TARGET :: field(cols*rows)
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: acc
REAL(KIND=C_DOUBLE),           INTENT(IN)         :: rmdi
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: len_comp
INTEGER(KIND=C_INT32_T),       INTENT(OUT)        :: comp_field(len_comp)
INTEGER(KIND=C_INT64_T),       INTENT(OUT)        :: num_words
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: message_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT)        :: cmessage(message_len + 1)

CHARACTER(LEN=message_len) :: message

REAL(KIND=real64), POINTER :: field2d(:,:)
TYPE(C_PTR)                :: field_cptr

! Need to associate the fortan pointer to the field ("field2d") with the
! target of the C pointer ("field") which was passed in, since cmps_all is
! going to need to be able to read from that address
field_cptr = C_LOC(field)
CALL C_F_POINTER (field_cptr, field2d, [cols,rows])

message = ""
status = f_shum_wgdos_pack(field2d, acc, rmdi, comp_field,                     &
                           num_words, message)
NULLIFY(field2d)

! If something went wrong allow the calling program to catch the non-zero 
! exit code and error message then act accordingly
IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(TRIM(message))
END IF

END FUNCTION c_shum_wgdos_pack

!------------------------------------------------------------------------------!

FUNCTION c_shum_wgdos_unpack(comp_field, len_comp, cols, rows, rmdi,           &
                             field, cmessage, message_len)                     &
                             RESULT(status)                                    &
                             BIND(c, NAME="c_shum_wgdos_unpack")
IMPLICIT NONE

INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: len_comp
INTEGER(KIND=C_INT32_T),       INTENT(IN)          :: comp_field(len_comp)
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: cols
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: rows
REAL(KIND=C_DOUBLE),           INTENT(IN)          :: rmdi
REAL(KIND=C_DOUBLE),           INTENT(OUT), TARGET :: field(cols*rows)
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: message_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT)         :: cmessage(message_len +1)

INTEGER(KIND=C_INT64_T) :: status

CHARACTER(LEN=message_len) :: message

REAL(KIND=real64), POINTER :: field2d(:,:)
TYPE(C_PTR)                :: field_cptr

! Need to associate the fortan pointer to the field ("field2d") with the
! target of the C pointer ("field") which was passed in, since wgdos_unpack
! is going to need to be able to write to that address
field_cptr = C_LOC(field)
CALL C_F_POINTER (field_cptr, field2d, [cols,rows])

message = ""
status = f_shum_wgdos_unpack(comp_field, rmdi, field2d, message)
NULLIFY(field2d)

! If something went wrong allow the calling program to catch the non-zero 
! exit code and error message then act accordingly
IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(TRIM(message))
END IF

END FUNCTION c_shum_wgdos_unpack

!------------------------------------------------------------------------------!

END MODULE c_shum_wgdos_packing_mod
