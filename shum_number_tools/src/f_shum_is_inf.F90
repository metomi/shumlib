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
! the ieee_is_finite function. This allows a fully portable
! infinity test to be performed in both 32 and 64 bit reals.

! Method:
! Use the ieee_is_finite function if available, otherwise test value for
! being larger than HUGE for datatype

MODULE f_shum_is_inf_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
    C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

IMPLICIT NONE

PRIVATE
PUBLIC :: f_shum_is_inf, f_shum_has_inf
INTERFACE f_shum_is_inf
  MODULE PROCEDURE f_shum_is_inf64, f_shum_is_inf32
END INTERFACE
INTERFACE f_shum_has_inf
  MODULE PROCEDURE f_shum_has_inf64,    f_shum_has_inf32,                      &
                   f_shum_has_inf64_2d, f_shum_has_inf32_2d,                   &
                   f_shum_has_inf64_3d, f_shum_has_inf32_3d,                   &
                   f_shum_has_inf64_4d, f_shum_has_inf32_4d,                   &
                   f_shum_has_inf64_5d, f_shum_has_inf32_5d
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
LOGICAL FUNCTION f_shum_is_inf64(x)
#if defined(HAS_IEEE_ARITHMETIC)
USE, INTRINSIC :: ieee_arithmetic, ONLY:                                       &
  IEEE_IS_FINITE, IEEE_SUPPORT_INF, IEEE_IS_NAN, IEEE_SUPPORT_NAN,             &
  IEEE_NEGATIVE_INF, IEEE_POSITIVE_INF,  OPERATOR(==), IEEE_CLASS
#endif

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x

! End of header

#if defined(HAS_IEEE_ARITHMETIC)
! Use the ieee version if supported
IF (IEEE_SUPPORT_INF(x)) THEN
  ! Check if normal or denomal class number
  f_shum_is_inf64 = (IEEE_CLASS(x) == IEEE_POSITIVE_INF) .OR.                  &
                    (IEEE_CLASS(x) == IEEE_NEGATIVE_INF)
ELSE
#endif
  ! Otherwise we cook up our own version
  IF (ABS(x) > HUGE(x)) THEN
    f_shum_is_inf64 = .TRUE.
  ELSE
    f_shum_is_inf64 = .FALSE.
  END IF
#if defined(HAS_IEEE_ARITHMETIC)
END IF
#endif

END FUNCTION f_shum_is_inf64

!***************************************************************************
! Scalar 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_is_inf32(x)
#if defined(HAS_IEEE_ARITHMETIC)
USE, INTRINSIC :: ieee_arithmetic, ONLY:                                       &
  IEEE_IS_FINITE, IEEE_SUPPORT_INF, IEEE_IS_NAN, IEEE_SUPPORT_NAN,             &
  IEEE_NEGATIVE_INF, IEEE_POSITIVE_INF,  OPERATOR(==), IEEE_CLASS
#endif

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x

! End of header

#if defined(HAS_IEEE_ARITHMETIC)
! Use the ieee version if supported
IF (IEEE_SUPPORT_INF(x)) THEN
  ! Check if normal or denomal class number
  f_shum_is_inf32 = (IEEE_CLASS(x) == IEEE_POSITIVE_INF) .OR.                  &
                    (IEEE_CLASS(x) == IEEE_NEGATIVE_INF)
ELSE
#endif
  ! Otherwise we cook up our own version
  IF (ABS(x) > HUGE(x)) THEN
    f_shum_is_inf32 = .TRUE.
  ELSE
    f_shum_is_inf32 = .FALSE.
  END IF
#if defined(HAS_IEEE_ARITHMETIC)
END IF
#endif

END FUNCTION f_shum_is_inf32

!***************************************************************************
! 1D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf64(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:)

! Local data
INTEGER                     :: ix

! End of header

! Loop over elements of x and determine if any are infinite
! Exit immediately if any are found
DO ix=1,SIZE(x)
  f_shum_has_inf64 = f_shum_is_inf64(x(ix))
  IF (f_shum_has_inf64) EXIT
END DO

END FUNCTION f_shum_has_inf64

!***************************************************************************
! 1D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf32(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:)

! Local data
INTEGER                     :: ix

! End of header

! Loop over elements of x and determine if any are infinite
! Exit immediately if any are found
DO ix=1,SIZE(x)
  f_shum_has_inf32 = f_shum_is_inf32(x(ix))
  IF (f_shum_has_inf32) EXIT
END DO

END FUNCTION f_shum_has_inf32

! To use for multi-dimensional arrays you can call f_shum_has_inf with the array
! reshaped, e.g. f_shum_has_inf(RESHAPE(x, (/SIZE(x)/)))

!***************************************************************************
! 2D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf64_2d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf64_2d = f_shum_has_inf64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf64_2d

!***************************************************************************
! 2D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf32_2d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf32_2d = f_shum_has_inf32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf32_2d

!***************************************************************************
! 3D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf64_3d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf64_3d = f_shum_has_inf64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf64_3d

!***************************************************************************
! 3D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf32_3d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf32_3d = f_shum_has_inf32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf32_3d

!***************************************************************************
! 4D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf64_4d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf64_4d = f_shum_has_inf64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf64_4d

!***************************************************************************
! 4D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf32_4d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf32_4d = f_shum_has_inf32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf32_4d

!***************************************************************************
! 5D Array 64-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf64_5d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL64), INTENT(IN) :: x(:,:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf64_5d = f_shum_has_inf64(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf64_5d

!***************************************************************************
! 5D Array 32-bit version
!***************************************************************************
LOGICAL FUNCTION f_shum_has_inf32_5d(x)

IMPLICIT NONE

! Function argument
REAL (KIND=REAL32), INTENT(IN) :: x(:,:,:,:,:)

! End of header

! Reshape array and pass through 1d array version
f_shum_has_inf32_5d = f_shum_has_inf32(RESHAPE(x, (/SIZE(x)/)))

END FUNCTION f_shum_has_inf32_5d

END MODULE f_shum_is_inf_mod
