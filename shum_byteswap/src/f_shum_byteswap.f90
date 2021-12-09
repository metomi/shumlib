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
MODULE f_shum_byteswap_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_LOC, C_PTR, C_INT64_T, C_INT32_T, C_CHAR, C_FLOAT, C_DOUBLE

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string, f_shum_f2c_string

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  f_shum_byteswap,                                                             &
  f_shum_get_machine_endianism,                                                &
  f_shum_endianness,                                                           &
  f_shum_bigendian,                                                            &
  f_shum_littleendian,                                                         &
  f_shum_numendians

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

ENUM, BIND(c)
ENUMERATOR ::                                                                  &
  f_shum_bigendian,                                                            &
  f_shum_littleendian,                                                         &
  f_shum_numendians
END ENUM

INTEGER, PARAMETER :: f_shum_endianness = KIND(f_shum_bigendian)

! Interfaces to the C routines
!------------------------------------------------------------------------------!

INTERFACE
FUNCTION c_shum_byteswap(bytes, swap_words, word_len, message, message_len)    &
                                  BIND(c, NAME="c_shum_byteswap") RESULT(status)

IMPORT :: C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

TYPE(C_PTR),                   INTENT(IN), VALUE :: bytes
INTEGER(KIND=C_INT64_T),       INTENT(IN), VALUE :: swap_words
INTEGER(KIND=C_INT64_T),       INTENT(IN), VALUE :: word_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT)       :: message(*)
INTEGER(KIND=C_INT64_T),       INTENT(IN), VALUE :: message_len
INTEGER(KIND=C_INT64_T)                          :: status

END FUNCTION c_shum_byteswap
END INTERFACE

!------------------------------------------------------------------------------!

INTERFACE
FUNCTION f_shum_get_machine_endianism ()                                       &
                   BIND(c,NAME="c_shum_get_machine_endianism") RESULT(endianism)

IMPORT :: f_shum_endianness

IMPLICIT NONE

INTEGER(KIND=f_shum_endianness) :: endianism

END FUNCTION f_shum_get_machine_endianism
END INTERFACE

!------------------------------------------------------------------------------!
! Interfaces; the C code which performs the byte-swapping treats the input data
! as a stream of contiguous bytes; the type is irrelevant. So for Fortran we
! can overload the interface as many times as is needed to cope with any type
! a user might want to pass it.  We also provide interfaces with both 32-bit
! and 64-bit versions of the other arguments, for easy integration.
!------------------------------------------------------------------------------!
INTERFACE f_shum_byteswap
MODULE PROCEDURE                                                               &
  shum_byteswap_cptr_int64args,                                                &
  shum_byteswap_cptr_int32args,                                                &
  shum_byteswap_1d_int64_int64args,                                            &
  shum_byteswap_2d_int64_int64args,                                            &
  shum_byteswap_1d_int64_int32args,                                            &
  shum_byteswap_2d_int64_int32args,                                            &
  shum_byteswap_1d_int32_int64args,                                            &
  shum_byteswap_2d_int32_int64args,                                            &
  shum_byteswap_1d_int32_int32args,                                            &
  shum_byteswap_2d_int32_int32args,                                            &
  shum_byteswap_1d_real64_int64args,                                           &
  shum_byteswap_2d_real64_int64args,                                           &
  shum_byteswap_1d_real64_int32args,                                           &
  shum_byteswap_2d_real64_int32args,                                           &
  shum_byteswap_1d_real32_int64args,                                           &
  shum_byteswap_2d_real32_int64args,                                           &
  shum_byteswap_1d_real32_int32args,                                           &
  shum_byteswap_2d_real32_int32args
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_cptr_int64args                                          &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR, C_PTR

IMPLICIT NONE

TYPE(C_PTR),         INTENT(INOUT)         :: bytes
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

status = c_shum_byteswap(bytes,                                                &
                         swap_words,                                           &
                         word_len,                                             &
                         cmessage,                                             &
                         LEN(message, KIND=int64) + 1)
message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION shum_byteswap_cptr_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_cptr_int32args                                          &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR, C_PTR

IMPLICIT NONE

TYPE(C_PTR),         INTENT(INOUT)         :: bytes
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

ALLOCATE(cmessage(LEN(message)+1))
cmessage(1) = C_NULL_CHAR

status = INT(c_shum_byteswap(bytes,                                            &
                             INT(swap_words, KIND=int64),                      &
                             INT(word_len, KIND=int64),                        &
                             cmessage,                                         &
                             LEN(message, KIND=int64) + 1)                     &
             , KIND=int32)
message = f_shum_c2f_string(cmessage)

DEALLOCATE(cmessage)

END FUNCTION shum_byteswap_cptr_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_int64_int64args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1)),                                    &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_int64_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_int64_int64args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1, 1)),                                 &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_int64_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_int64_int32args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1)),                                &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_int64_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_int64_int32args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1,1)),                              &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_int64_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_int32_int64args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32), INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1)),                                    &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_int32_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_int32_int64args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32), INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1,1)),                                  &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_int32_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_int32_int32args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32), INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1)),                                &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_int32_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_int32_int32args                                      &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

INTEGER(KIND=int32), INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1,1)),                              &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_int32_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_real64_int64args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real64),   INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1)),                                    &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_real64_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_real64_int64args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real64),   INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1,1)),                                  &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_real64_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_real64_int32args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real64),   INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1)),                                &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_real64_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_real64_int32args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real64),   INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1,1)),                              &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_real64_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_real32_int64args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real32),   INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1)),                                    &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_real32_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_real32_int64args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real32),   INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int64), INTENT(IN)            :: swap_words
INTEGER(KIND=int64), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int64)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = c_shum_byteswap(C_LOC(bytes(1,1)),                                  &
                           swap_words,                                         &
                           word_len,                                           &
                           cmessage,                                           &
                           LEN(message, KIND=int64) + 1)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_real32_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_1d_real32_int32args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real32),   INTENT(INOUT), TARGET :: bytes(:)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1)),                                &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_1d_real32_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_2d_real32_int32args                                     &
                           (bytes, swap_words, word_len, message) RESULT(status)

USE, INTRINSIC :: iso_c_binding, ONLY: C_NULL_CHAR

IMPLICIT NONE

REAL(KIND=real32),   INTENT(INOUT), TARGET :: bytes(:, :)
INTEGER(KIND=int32), INTENT(IN)            :: swap_words
INTEGER(KIND=int32), INTENT(IN)            :: word_len
CHARACTER(LEN=*),    INTENT(OUT)           :: message

INTEGER(KIND=int32)                        :: status
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  status = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE

  ALLOCATE(cmessage(LEN(message)+1))
  cmessage(1) = C_NULL_CHAR

  status = INT(c_shum_byteswap(C_LOC(bytes(1,1)),                              &
                               INT(swap_words, KIND=int64),                    &
                               INT(word_len, KIND=int64),                      &
                               cmessage,                                       &
                               LEN(message, KIND=int64) + 1)                   &
               , KIND=int32)
  message = f_shum_c2f_string(cmessage)

  DEALLOCATE(cmessage)

END IF

END FUNCTION shum_byteswap_2d_real32_int32args

!------------------------------------------------------------------------------!

END MODULE f_shum_byteswap_mod
