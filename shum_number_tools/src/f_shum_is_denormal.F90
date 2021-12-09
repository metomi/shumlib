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

! Description:
! A simple module to wrap, or to provide equivalent functionality for,
! the ieee_is_normal function. This allows a fully portable
! infinity test to be performed in both 32 and 64 bit reals.

! Method:
! Use the ieee_is_normal function if available, otherwise test value by
! inspecting the bits.

MODULE f_shum_is_denormal_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
    C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

IMPLICIT NONE

PRIVATE
PUBLIC :: f_shum_is_denormal, f_shum_has_denormal
INTERFACE f_shum_is_denormal
  MODULE PROCEDURE f_shum_is_denormal64, f_shum_is_denormal32
END INTERFACE
INTERFACE f_shum_has_denormal
  MODULE PROCEDURE f_shum_has_denormal64,    f_shum_has_denormal32,            &
                   f_shum_has_denormal64_2d, f_shum_has_denormal32_2d,         &
                   f_shum_has_denormal64_3d, f_shum_has_denormal32_3d,         &
                   f_shum_has_denormal64_4d, f_shum_has_denormal32_4d,         &
                   f_shum_has_denormal64_5d, f_shum_has_denormal32_5d
END INTERFACE

!------------------------------------------------------------------------------!
! We are going to use the types from the ISO_C_BINDING module, since although  !
! the REALs are not 100% guaranteed to correspond to the sizes we want to      !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
INTEGER, PARAMETER :: INT64  = C_INT64_T
INTEGER, PARAMETER :: INT32  = C_INT32_T
INTEGER, PARAMETER :: REAL64 = C_DOUBLE
INTEGER, PARAMETER :: REAL32 = C_FLOAT
INTEGER, PARAMETER :: bool   = C_BOOL

CONTAINS

!***************************************************************************
! Scalar 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_is_denormal64(x)
#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
USE, INTRINSIC :: ieee_arithmetic, ONLY:                                       &
  IEEE_SUPPORT_DENORMAL,                                                       &
  IEEE_IS_NORMAL,                                                              &
  IEEE_SUPPORT_INF,                                                            &
  IEEE_SUPPORT_NAN,                                                            &
  IEEE_IS_FINITE
#endif

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x

INTEGER(KIND=INT64) :: int_repr
! End of header

#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
! Use the ieee version if supported
IF (IEEE_SUPPORT_DENORMAL(x)) THEN
  f_shum_is_denormal64 = .NOT. IEEE_IS_NORMAL(x)
  ! Non-normal numbers include INF and NaN, which are not finite.
  ! Check for those if we are not normal.
  IF ((IEEE_SUPPORT_INF(x) .OR. IEEE_SUPPORT_NAN(x))                           &
      .AND. f_shum_is_denormal64) THEN
    f_shum_is_denormal64 = IEEE_IS_FINITE(x)
  END IF
ELSE
#endif
  int_repr = TRANSFER(x,INT(0,KIND=INT64))
  ! Otherwise we cook up our own version
  IF (IBITS(int_repr, 52, 11) == 0_int64) THEN
    IF (int_repr == 0_int64) THEN
      f_shum_is_denormal64 = .FALSE.
    ELSE
      f_shum_is_denormal64 = .TRUE.
    END IF
  ELSE
    f_shum_is_denormal64 = .FALSE.
  END IF

#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
END IF
#endif

END FUNCTION f_shum_is_denormal64

!***************************************************************************
! Scalar 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_is_denormal32(x)
#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
USE, INTRINSIC :: ieee_arithmetic, ONLY:                                       &
  IEEE_SUPPORT_DENORMAL,                                                       &
  IEEE_IS_NORMAL,                                                              &
  IEEE_SUPPORT_INF,                                                            &
  IEEE_SUPPORT_NAN,                                                            &
  IEEE_IS_FINITE
#endif

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x

INTEGER(KIND=INT32) :: int_repr

! End of header

#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
! Use the ieee version if supported
IF (IEEE_SUPPORT_DENORMAL(x)) THEN
  f_shum_is_denormal32 = .NOT. IEEE_IS_NORMAL(x)
  ! Non-normal numbers include INF and NaN, which are not finite.
  ! Check for those if we are not normal.
  IF ((IEEE_SUPPORT_INF(x) .OR. IEEE_SUPPORT_NAN(x))                           &
      .AND. f_shum_is_denormal32) THEN
    f_shum_is_denormal32 = IEEE_IS_FINITE(x)
  END IF
ELSE
#endif
  int_repr = TRANSFER(x,INT(0,KIND=INT32))
  ! Otherwise we cook up our own version
  IF (IBITS(int_repr, 23, 8) == 0_int32) THEN
    IF (int_repr == 0_int32) THEN
      f_shum_is_denormal32 = .FALSE.
    ELSE
      f_shum_is_denormal32 = .TRUE.
    END IF
  ELSE
    f_shum_is_denormal32 = .FALSE.
  END IF

#if defined(HAS_IEEE_ARITHMETIC) && !defined(EVAL_DENORMAL_BY_BITS)
END IF
#endif

END FUNCTION f_shum_is_denormal32

!***************************************************************************
! 1D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal64(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:)

! Local data
INTEGER                     :: ix

! End of header

! Loop over elements of x and determine if any are infinite
! Exit immediately if any are found
DO ix=1,SIZE(x)
  f_shum_has_denormal64 = f_shum_is_denormal64(x(ix))
  IF (f_shum_has_denormal64) EXIT
END DO

END FUNCTION f_shum_has_denormal64

!***************************************************************************
! 1D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal32(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:)

! Local data
INTEGER                     :: ix

! End of header

! Loop over elements of x and determine if any are infinite
! Exit immediately if any are found
DO ix=1,SIZE(x)
  f_shum_has_denormal32 = f_shum_is_denormal32(x(ix))
  IF (f_shum_has_denormal32) EXIT
END DO

END FUNCTION f_shum_has_denormal32

! To use for multi-dimensional arrays you can call f_shum_has_denormal with the
! array reshaped, e.g. f_shum_has_denormal(RESHAPE(x, (/SIZE(x)/)))

!***************************************************************************
! 2D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal64_2d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal64_2d = f_shum_has_denormal64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal64_2d

!***************************************************************************
! 2D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal32_2d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal32_2d = f_shum_has_denormal32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal32_2d

!***************************************************************************
! 3D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal64_3d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal64_3d = f_shum_has_denormal64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal64_3d

!***************************************************************************
! 3D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal32_3d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal32_3d = f_shum_has_denormal32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal32_3d

!***************************************************************************
! 4D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal64_4d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal64_4d = f_shum_has_denormal64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal64_4d

!***************************************************************************
! 4D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal32_4d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal32_4d = f_shum_has_denormal32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal32_4d

!***************************************************************************
! 5D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal64_5d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal64_5d = f_shum_has_denormal64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal64_5d

!***************************************************************************
! 5D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_denormal32_5d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_denormal32_5d = f_shum_has_denormal32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_denormal32_5d

END MODULE f_shum_is_denormal_mod
