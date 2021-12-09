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
! This module contains the interfaces to call c code within fortran.
!
MODULE f_shum_data_conv_mod

! DEPENDS ON: c_shum_data_conv.o

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_LOC, C_PTR, C_INT64_T, C_INT32_T, C_CHAR, C_FLOAT, C_DOUBLE

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string, f_shum_f2c_string

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  f_shum_ibm2ieee,                                                             &
  f_shum_ieee2ibm,                                                             &
  f_shum_ieee2ieg,                                                             &
  f_shum_ieg2ieee,                                                             &
  f_shum_data_types,                                                           &
  f_shum_typenull,                                                             &
  f_shum_typeless,                                                             &
  f_shum_integer_type,                                                         &
  f_shum_real_type,                                                            &
  f_shum_complex_type,                                                         &
  f_shum_logical_type,                                                         &
  f_shum_character_type

! -----------------------------------------------------------------------------!

ENUM, BIND(c)
ENUMERATOR ::                                                                  &
  f_shum_typenull,                                                             &
  f_shum_typeless,                                                             &
  f_shum_integer_type,                                                         &
  f_shum_real_type,                                                            &
  f_shum_complex_type,                                                         &
  f_shum_logical_type,                                                         &
  f_shum_character_type
END ENUM

INTEGER, PARAMETER :: f_shum_data_types = KIND(f_shum_typenull)

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
! Interfaces
! C Interfaces
INTERFACE
FUNCTION c_ieg2ieee (data_type, num, ieg_num_in, offset_in, cri_num_out,       &
                     stride, size_num_out, size_num_in, message, message_len)  &
                     BIND(c,NAME="c_shum_ieg2ieee")

IMPORT :: f_shum_data_types, C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  c_ieg2ieee

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  num,                                                                         &
  offset_in,                                                                   &
  stride,                                                                      &
  size_num_out,                                                                &
  size_num_in

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
  ieg_num_in,                                                                  &
  cri_num_out

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT) :: message(*)

INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: message_len

END FUNCTION c_ieg2ieee
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
FUNCTION c_ieee2ieg (data_type, num, ieg_num_out, offset_out, cri_num_in,      &
                     stride, size_num_in, size_num_out, message, message_len)  &
                     BIND(c,NAME="c_shum_ieee2ieg")

IMPORT :: f_shum_data_types, C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  c_ieee2ieg

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
  cri_num_in,                                                                  &
  ieg_num_out

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT) :: message(*)

INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: message_len

END FUNCTION c_ieee2ieg
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
FUNCTION c_ieee2ibm (data_type, num, ibm_num_out, offset_out, cri_num_in,      &
                     stride, size_num_in, size_num_out, message, message_len)  &
                     BIND(c,NAME="c_shum_ieee2ibm")

IMPORT :: f_shum_data_types, C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  c_ieee2ibm

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
  cri_num_in,                                                                  &
  ibm_num_out

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT) :: message(*)

INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: message_len

END FUNCTION c_ieee2ibm
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
FUNCTION c_ibm2ieee (data_type, num, ibm_num_in, offset_in, cri_num_out,       &
                     stride, size_num_out, size_num_in, message, message_len)  &
                     BIND(c,NAME="c_shum_ibm2ieee")

IMPORT :: f_shum_data_types, C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  c_ibm2ieee

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
  ibm_num_in,                                                                  &
  cri_num_out

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT) :: message(*)

INTEGER(KIND=C_INT64_T), INTENT(IN), VALUE :: message_len

END FUNCTION c_ibm2ieee
END INTERFACE

!------------------------------------------------------------------------------!
! Generic (Fortran) Interfaces

INTERFACE f_shum_ieee2ieg
MODULE PROCEDURE                                                               &
  ieee2ieg_real,                                                               &
  ieee2ieg_real_scalar,                                                        &
  ieee2ieg_real_to_int,                                                        &
  ieee2ieg_int_to_real,                                                        &
  ieee2ieg_int_to_real_scalar,                                                 &
  ieee2ieg_64
END INTERFACE

INTERFACE f_shum_ieee2ibm
MODULE PROCEDURE                                                               &
  ieee2ibm_scalar,                                                             &
  ieee2ibm_int_to_real_scalar,                                                 &
  ieee2ibm_int_to_real32_scalar,                                               &
  ieee2ibm_int_to_int32_scalar,                                                &
  ieee2ibm_real_to_int,                                                        &
  ieee2ibm_64,                                                                 &
  ieee2ibm_real,                                                               &
  ieee2ibm_real_scalar,                                                        &
  ieee2ibm_32_scalar_32_to_64
END INTERFACE

INTERFACE f_shum_ibm2ieee
MODULE PROCEDURE                                                               &
  ibm2ieee_scalar,                                                             &
  ibm2ieee_real_to_int_scalar,                                                 &
  ibm2ieee_real32_to_int_scalar,                                               &
  ibm2ieee_int32_to_int_scalar,                                                &
  ibm2ieee_int_to_real,                                                        &
  ibm2ieee_real32_real64,                                                      &
  ibm2ieee_real32_to_real64_2d,                                                &
  ibm2ieee_64,                                                                 &
  ibm2ieee_32_scalar_64_to_32
END INTERFACE

INTERFACE f_shum_ieg2ieee
MODULE PROCEDURE                                                               &
  ieg2ieee_64
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!
! ieg2ieee

FUNCTION ieg2ieee_64 (data_type, num, ieg_num_in, offset_in, cri_num_out,      &
                      stride, size_num_out, size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieg2ieee_64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out(num)

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  ieg_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieg2ieee_64 = c_ieg2ieee(data_type, num, C_LOC(ieg_num_in), offset_in,         &
                         C_LOC(cri_num_out), stride, size_num_out,             &
                         size_num_in, cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieg2ieee_64

!------------------------------------------------------------------------------!
! ieee2ieg

FUNCTION ieee2ieg_64 (data_type, num, ieg_num_out, offset_out,  cri_num_in,    &
                      stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ieg_num_out(num)

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_64 = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out), offset_out,       &
                         C_LOC(cri_num_in), stride, size_num_in,               &
                         size_num_out, cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_64

!------------------------------------------------------------------------------!

FUNCTION ieee2ieg_real (data_type, num, ieg_num_out, offset_out, cri_num_in,   &
                        stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_real

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ieg_num_out(num)

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_real = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out), offset_out,     &
                           C_LOC(cri_num_in), stride, size_num_in,             &
                           size_num_out, cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_real

!------------------------------------------------------------------------------!

FUNCTION ieee2ieg_int_to_real (data_type, num, ieg_num_out, offset_out,        &
                               cri_num_in, stride, size_num_in, size_num_out,  &
                               message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_int_to_real

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ieg_num_out(num)

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_int_to_real = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out),          &
                                  offset_out, C_LOC(cri_num_in), stride,       &
                                  size_num_in, size_num_out, cmessage,         &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_int_to_real

!------------------------------------------------------------------------------!

FUNCTION ieee2ieg_real_to_int (data_type, num, ieg_num_out, offset_out,        &
                               cri_num_in, stride, size_num_in,                &
                               size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_real_to_int

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ieg_num_out(num)

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_real_to_int = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out),          &
                                  offset_out, C_LOC(cri_num_in), stride,       &
                                  size_num_in, size_num_out, cmessage,         &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_real_to_int

!------------------------------------------------------------------------------!

FUNCTION ieee2ieg_real_scalar (data_type, num, ieg_num_out, offset_out,        &
                               cri_num_in,  stride, size_num_in,               &
                               size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_real_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ieg_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_real_scalar = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out),          &
                                  offset_out, C_LOC(cri_num_in), stride,       &
                                  size_num_in, size_num_out, cmessage,         &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_real_scalar

!------------------------------------------------------------------------------!

FUNCTION ieee2ieg_int_to_real_scalar(data_type, num, ieg_num_out, offset_out,  &
                                 cri_num_in,  stride, size_num_in,             &
                                 size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ieg_int_to_real_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ieg_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ieg_int_to_real_scalar = c_ieee2ieg(data_type, num, C_LOC(ieg_num_out),   &
                                         offset_out, C_LOC(cri_num_in), stride,&
                                         size_num_in, size_num_out, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ieg_int_to_real_scalar

!------------------------------------------------------------------------------!
! ieee2ibm

FUNCTION ieee2ibm_64 (data_type, num, ibm_num_out, offset_out, cri_num_in,     &
                      stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ibm_num_out(num)

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_64 = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out), offset_out,       &
                         C_LOC(cri_num_in), stride, size_num_in, size_num_out, &
                         cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_64

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_real_scalar (data_type, num, ibm_num_out, offset_out,        &
                               cri_num_in, stride, size_num_in, size_num_out,  &
                               message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_real_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ibm_num_out

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_real_scalar = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out),          &
                                  offset_out, C_LOC(cri_num_in), stride,       &
                                  size_num_in, size_num_out, cmessage,         &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_real_scalar

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_real (data_type, num, ibm_num_out, offset_out, cri_num_in,   &
                        stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_real

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num, stride, offset_out, size_num_in, size_num_out

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ibm_num_out(num)

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_real = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out), offset_out,     &
                           C_LOC(cri_num_in), stride, size_num_in,             &
                           size_num_out, cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_real

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_real_to_int (data_type, num, ibm_num_out, offset_out,        &
                               cri_num_in, stride, size_num_in, size_num_out,  &
                               message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_real_to_int

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ibm_num_out(num)

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  cri_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_real_to_int = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out),          &
                                  offset_out, C_LOC(cri_num_in), stride,       &
                                  size_num_in, size_num_out, cmessage,         &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_real_to_int

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_scalar (data_type, num, ibm_num_out, offset_out, cri_num_in, &
                          stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ibm_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_scalar = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out), offset_out,   &
                             C_LOC(cri_num_in), stride, size_num_in,           &
                             size_num_out, cmessage,                           &
                             LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_scalar

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_32_scalar_32_to_64 (data_type, num, ibm_num_out, offset_out, &
                            cri_num_in, stride, size_num_in, size_num_out,     &
                            message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32) ::                                                         &
  ieee2ibm_32_scalar_32_to_64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int32), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int32), INTENT(IN), TARGET ::                                     &
  cri_num_in

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  ibm_num_out

INTEGER(KIND=int64) ::                                                         &
  c_ieee2ibm_ret64

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

c_ieee2ibm_ret64 = c_ieee2ibm(data_type, INT(num,kind=int64),                  &
                                         C_LOC(ibm_num_out),                   &
                                         INT(offset_out,kind=int64),           &
                                         C_LOC(cri_num_in),                    &
                                         INT(stride,kind=int64),               &
                                         INT(size_num_in,kind=int64),          &
                                         INT(size_num_out,kind=int64),         &
                                         cmessage, LEN(message, KIND=int64) + 1)

ieee2ibm_32_scalar_32_to_64 = INT(c_ieee2ibm_ret64,KIND=int32)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_32_scalar_32_to_64

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_int_to_real_scalar (data_type, num, ibm_num_out, offset_out, &
                                      cri_num_in, stride, size_num_in,         &
                                      size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_int_to_real_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  ibm_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_int_to_real_scalar = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out),   &
                                         offset_out, C_LOC(cri_num_in), stride,&
                                         size_num_in, size_num_out, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_int_to_real_scalar

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_int_to_real32_scalar (data_type, num, ibm_num_out,           &
     offset_out,cri_num_in, stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_int_to_real32_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in

REAL(KIND=real32), INTENT(INOUT), TARGET ::                                    &
  ibm_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_int_to_real32_scalar = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out), &
                                         offset_out, C_LOC(cri_num_in), stride,&
                                         size_num_in, size_num_out, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_int_to_real32_scalar

!------------------------------------------------------------------------------!

FUNCTION ieee2ibm_int_to_int32_scalar (data_type, num, ibm_num_out,            &
     offset_out,cri_num_in, stride, size_num_in, size_num_out, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ieee2ibm_int_to_int32_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_out,                                                                  &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  cri_num_in

INTEGER(KIND=int32), INTENT(INOUT), TARGET ::                                  &
  ibm_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ieee2ibm_int_to_int32_scalar = c_ieee2ibm(data_type, num, C_LOC(ibm_num_out),  &
                                         offset_out, C_LOC(cri_num_in), stride,&
                                         size_num_in, size_num_out, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ieee2ibm_int_to_int32_scalar

!------------------------------------------------------------------------------!
! ibm2ieee

FUNCTION ibm2ieee_64 (data_type, num, ibm_num_in, offset_in, cri_num_out,      &
                      stride, size_num_out, size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  ibm_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out(num)

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_64 = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in), offset_in,         &
                         C_LOC(cri_num_out), stride, size_num_out,             &
                         size_num_in, cmessage, LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_64

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_scalar (data_type, num, ibm_num_in, offset_in, cri_num_out,  &
                          stride, size_num_out, size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  ibm_num_in

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_scalar = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in), offset_in,     &
                             C_LOC(cri_num_out), stride, size_num_out,         &
                             size_num_in, cmessage,                            &
                             LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_scalar

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_32_scalar_64_to_32 (data_type, num, ibm_num_in, offset_in,   &
                                      cri_num_out, stride, size_num_out,       &
                                      size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32) ::                                                         &
  ibm2ieee_32_scalar_64_to_32

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int32), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  ibm_num_in

INTEGER(KIND=int32), INTENT(INOUT), TARGET ::                                  &
  cri_num_out

INTEGER(KIND=int64) ::                                                         &
  c_ibm2ieee_ret64

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

c_ibm2ieee_ret64 = c_ibm2ieee(data_type, INT(num,kind=int64),                  &
                                         C_LOC(ibm_num_in),                    &
                                         INT(offset_in,kind=int64),            &
                                         C_LOC(cri_num_out),                   &
                                         INT(stride,kind=int64),               &
                                         INT(size_num_out,kind=int64),         &
                                         INT(size_num_in,kind=int64),          &
                                         cmessage, LEN(message, KIND=int64) + 1)

ibm2ieee_32_scalar_64_to_32 = INT(c_ibm2ieee_ret64,KIND=int32)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_32_scalar_64_to_32

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_real32_real64 (data_type, num, ibm_num_in, offset_in,        &
                                 cri_num_out, stride, size_num_out,            &
                                 size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_real32_real64

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real32), INTENT(IN), TARGET ::                                       &
  ibm_num_in(CEILING(REAL(num*size_num_in)*2.0/REAL(size_num_out)))

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  cri_num_out(num)

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_real32_real64 = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in),         &
                                    offset_in, C_LOC(cri_num_out), stride,     &
                                    size_num_out, size_num_in, cmessage,       &
                                    LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_real32_real64

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_real_to_int_scalar (data_type, num, ibm_num_in, offset_in,   &
                                      cri_num_out, stride, size_num_out,       &
                                      size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_real_to_int_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out

REAL(KIND=real64), INTENT(IN), TARGET ::                                       &
  ibm_num_in

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_real_to_int_scalar = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in),    &
                                         offset_in, C_LOC(cri_num_out), stride,&
                                         size_num_out, size_num_in, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_real_to_int_scalar

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_real32_to_int_scalar (data_type, num, ibm_num_in, offset_in, &
                                        cri_num_out, stride, size_num_out,     &
                                        size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_real32_to_int_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out

REAL(KIND=real32), INTENT(IN), TARGET ::                                       &
  ibm_num_in

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_real32_to_int_scalar = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in),  &
                                         offset_in, C_LOC(cri_num_out), stride,&
                                         size_num_out, size_num_in, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_real32_to_int_scalar

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_int32_to_int_scalar (data_type, num, ibm_num_in, offset_in,  &
                                       cri_num_out, stride, size_num_out,      &
                                       size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_int32_to_int_scalar

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

INTEGER(KIND=int64), INTENT(INOUT), TARGET ::                                  &
  cri_num_out

INTEGER(KIND=int32), INTENT(IN), TARGET ::                                     &
  ibm_num_in

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_int32_to_int_scalar = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in),   &
                                         offset_in, C_LOC(cri_num_out), stride,&
                                         size_num_out, size_num_in, cmessage,  &
                                         LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_int32_to_int_scalar

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_real32_to_real64_2d (data_type, num, ibm_num_in, offset_in,  &
                                       cri_num_out, stride, size_num_out,      &
                                       size_num_in, message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_real32_to_real64_2d

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  cri_num_out(:,:)

REAL(KIND=real32), INTENT(IN), TARGET ::                                       &
  ibm_num_in(CEILING(REAL(num*size_num_in)*2.0/REAL(size_num_out)))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

! C_LOC does not work with assumed shape arrays (may be an array slice
! with non-unitary stride, or stored discontiguously in memory)

! Thererefore use address of (1,1)th element of the array

!NB: this means the user must ensure the array is contiguos in memory
!    and stride one

ibm2ieee_real32_to_real64_2d = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in),   &
                                          offset_in, C_LOC(cri_num_out(1,1)),  &
                                          stride, size_num_out, size_num_in,   &
                                          cmessage,                            &
                                          LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_real32_to_real64_2d

!------------------------------------------------------------------------------!

FUNCTION ibm2ieee_int_to_real (data_type, num, ibm_num_in, offset_in,          &
                               cri_num_out, stride, size_num_out, size_num_in, &
                               message)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  ibm2ieee_int_to_real

INTEGER(KIND=f_shum_data_types), INTENT(IN) ::                                 &
  data_type

INTEGER(KIND=int64), INTENT(IN) ::                                             &
  num,                                                                         &
  stride,                                                                      &
  offset_in,                                                                   &
  size_num_in,                                                                 &
  size_num_out

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  cri_num_out(num)

INTEGER(KIND=int64), INTENT(IN), TARGET ::                                     &
  ibm_num_in(CEILING(num*REAL(size_num_in, KIND=real64)/size_num_out))

CHARACTER(LEN=*), INTENT(OUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

ibm2ieee_int_to_real = c_ibm2ieee(data_type, num, C_LOC(ibm_num_in), offset_in,&
                                  C_LOC(cri_num_out), stride, size_num_out,    &
                                  size_num_in, cmessage,                       &
                                  LEN(message, KIND=int64) + 1)

message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION ibm2ieee_int_to_real

!------------------------------------------------------------------------------!

END MODULE f_shum_data_conv_mod

