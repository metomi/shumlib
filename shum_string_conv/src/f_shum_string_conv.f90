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
! This module contains the interfaces for interoperability of c and
! fortran characters/strings

MODULE f_shum_string_conv_mod

! Fortran character kind conversions are transparent
! i.e. characters will be mapped to the correct coresponding
! character upon assignment, even if they are not in the same
! positions in default fortran set and the C_CHAR set.

! Using TRANSFER to change between charater sets is not portable
! (depends on the default character set(s) of the machine)
! It will result in the character of the same *position* in the
! set, rather than the character with the same *meaning*

IMPLICIT NONE

PRIVATE

PUBLIC :: f_shum_strlen, f_shum_f2c_string, f_shum_c2f_string

INTERFACE f_shum_strlen
MODULE PROCEDURE                                                               &
  c_strlen_integer_cstr
END INTERFACE

INTERFACE f_shum_c2f_string
MODULE PROCEDURE                                                               &
  c2f_string_cptr,                                                             &
  c2f_string_cstr,                                                             &
  c2f_string_cstr_nolen
END INTERFACE

INTERFACE
FUNCTION c_std_strlen(cstr) BIND(c, NAME="strlen")

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_SIZE_T

IMPLICIT NONE

CHARACTER(KIND=C_CHAR,LEN=1), INTENT(IN)  :: cstr(*)
INTEGER(KIND=C_SIZE_T)   :: c_std_strlen

END FUNCTION c_std_strlen
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION c_strlen_integer_cstr(cstr)

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_INT64_T

IMPLICIT NONE

CHARACTER(KIND=C_CHAR,LEN=1), TARGET :: cstr(*)
INTEGER(KIND=C_INT64_T)              :: c_strlen_integer_cstr

c_strlen_integer_cstr = INT(c_std_strlen(cstr), KIND=C_INT64_T)

END FUNCTION c_strlen_integer_cstr

!------------------------------------------------------------------------------!

!Trivially turn a c string (a \0 terminated array of LEN=1 characters)
!into a single character (LEN=something) variable.
FUNCTION c2f_string_cstr(cstr, cstr_len)

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_INT64_T

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) :: cstr_len, i
CHARACTER(KIND=C_CHAR,LEN=1), INTENT(IN) :: cstr(cstr_len)
CHARACTER(LEN=cstr_len)                  :: c2f_string_cstr

DO i=1,cstr_len
  c2f_string_cstr(i:i) = cstr(i)
END DO

END FUNCTION c2f_string_cstr

!------------------------------------------------------------------------------!

FUNCTION c2f_string_cptr(cptr, cstr_len)

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_INT64_T, C_PTR, C_F_POINTER

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) :: cstr_len, i
TYPE(C_PTR), INTENT(IN)                  :: cptr
CHARACTER(KIND=C_CHAR, LEN=1), POINTER   :: fptr(:)
CHARACTER(LEN=cstr_len)                  :: c2f_string_cptr

CALL C_F_POINTER(cptr,fptr,[cstr_len])

DO i=1,cstr_len
  c2f_string_cptr(i:i) = fptr(i)
END DO

END FUNCTION c2f_string_cptr

!------------------------------------------------------------------------------!

FUNCTION c2f_string_cstr_nolen(cstr)

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_INT64_T

IMPLICIT NONE

CHARACTER(KIND=C_CHAR,LEN=1), INTENT(IN) :: cstr(*)
CHARACTER(LEN=:), ALLOCATABLE            :: c2f_string_cstr_nolen

INTEGER(KIND=C_INT64_T) :: cstr_len

cstr_len = f_shum_strlen(cstr)

ALLOCATE ( CHARACTER(LEN=cstr_len) :: c2f_string_cstr_nolen )

c2f_string_cstr_nolen = c2f_string_cstr(cstr,cstr_len)

END FUNCTION c2f_string_cstr_nolen

!------------------------------------------------------------------------------!

FUNCTION f_shum_f2c_string(fstr)

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_CHAR, C_INT64_T, C_NULL_CHAR

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(IN)               :: fstr
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: f_shum_f2c_string(:)

INTEGER(KIND=C_INT64_T) :: strlen,i

strlen=LEN(fstr)

ALLOCATE(f_shum_f2c_string(strlen+1))
DO i=1,strlen
  f_shum_f2c_string(i)=fstr(i:i)
END DO
f_shum_f2c_string(strlen+1)=C_NULL_CHAR

END FUNCTION f_shum_f2c_string

!------------------------------------------------------------------------------!

END MODULE f_shum_string_conv_mod

