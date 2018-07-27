! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE f_shum_ff_status_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                        &
  C_INT64_T, C_INT32_T

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_ff_status_type, OPERATOR(==), OPERATOR(>), OPERATOR(<),        &
                               OPERATOR(>=), OPERATOR(<=), OPERATOR(/=),      &
                               SHUMLIB_SUCCESS

INTEGER, PARAMETER :: int32  = C_INT32_T
INTEGER, PARAMETER :: int64  = C_INT64_T

INTEGER(KIND=int64), PARAMETER :: SHUMLIB_SUCCESS = 0

TYPE shum_ff_status_type
  INTEGER(KIND=int64):: icode = -1    ! Return code
  CHARACTER(LEN=1024) :: message = '' ! Status message
END TYPE shum_ff_status_type

!-------------------------------------------------------------------------------

INTERFACE OPERATOR(==)
  MODULE PROCEDURE eq_status_first_64
  MODULE PROCEDURE eq_status_second_64
  MODULE PROCEDURE eq_status_first_32
  MODULE PROCEDURE eq_status_second_32
  MODULE PROCEDURE eq_status_both
END INTERFACE
INTERFACE OPERATOR(>)
  MODULE PROCEDURE gt_status_first_64
  MODULE PROCEDURE gt_status_second_64
  MODULE PROCEDURE gt_status_first_32
  MODULE PROCEDURE gt_status_second_32
  MODULE PROCEDURE gt_status_both
END INTERFACE
INTERFACE OPERATOR(<)
  MODULE PROCEDURE lt_status_first_64
  MODULE PROCEDURE lt_status_second_64
  MODULE PROCEDURE lt_status_first_32
  MODULE PROCEDURE lt_status_second_32
  MODULE PROCEDURE lt_status_both
END INTERFACE
INTERFACE OPERATOR(>=)
  MODULE PROCEDURE ge_status_first_64
  MODULE PROCEDURE ge_status_second_64
  MODULE PROCEDURE ge_status_first_32
  MODULE PROCEDURE ge_status_second_32
  MODULE PROCEDURE ge_status_both
END INTERFACE
INTERFACE OPERATOR(<=)
  MODULE PROCEDURE le_status_first_64
  MODULE PROCEDURE le_status_second_64
  MODULE PROCEDURE le_status_first_32
  MODULE PROCEDURE le_status_second_32
  MODULE PROCEDURE le_status_both
END INTERFACE
INTERFACE OPERATOR(/=)
  MODULE PROCEDURE ne_status_first_64
  MODULE PROCEDURE ne_status_second_64
  MODULE PROCEDURE ne_status_first_32
  MODULE PROCEDURE ne_status_second_32
  MODULE PROCEDURE ne_status_both
END INTERFACE

!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------

FUNCTION eq_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode == int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION eq_status_first_64

!-------------------------------------------------------------------------------

FUNCTION eq_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode == int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION eq_status_second_64

!-------------------------------------------------------------------------------

FUNCTION eq_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode == int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION eq_status_first_32

!-------------------------------------------------------------------------------

FUNCTION eq_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode == int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION eq_status_second_32

!-------------------------------------------------------------------------------

FUNCTION eq_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode == obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION eq_status_both

!-------------------------------------------------------------------------------

FUNCTION gt_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode > int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION gt_status_first_64

!-------------------------------------------------------------------------------

FUNCTION gt_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode > int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION gt_status_second_64

!-------------------------------------------------------------------------------

FUNCTION gt_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode > int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION gt_status_first_32

!-------------------------------------------------------------------------------

FUNCTION gt_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode > int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION gt_status_second_32

!-------------------------------------------------------------------------------

FUNCTION gt_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode > obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION gt_status_both

!-------------------------------------------------------------------------------

FUNCTION lt_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode < int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION lt_status_first_64

!-------------------------------------------------------------------------------

FUNCTION lt_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode < int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION lt_status_second_64

!-------------------------------------------------------------------------------

FUNCTION lt_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode < int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION lt_status_first_32

!-------------------------------------------------------------------------------

FUNCTION lt_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode < int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION lt_status_second_32

!-------------------------------------------------------------------------------

FUNCTION lt_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode < obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION lt_status_both

!-------------------------------------------------------------------------------

FUNCTION ge_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode >= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ge_status_first_64

!-------------------------------------------------------------------------------

FUNCTION ge_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode >= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ge_status_second_64

!-------------------------------------------------------------------------------

FUNCTION ge_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode >= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ge_status_first_32

!-------------------------------------------------------------------------------

FUNCTION ge_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode >= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ge_status_second_32

!-------------------------------------------------------------------------------

FUNCTION ge_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode >= obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ge_status_both

!-------------------------------------------------------------------------------

FUNCTION le_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode <= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION le_status_first_64

!-------------------------------------------------------------------------------

FUNCTION le_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode <= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION le_status_second_64

!-------------------------------------------------------------------------------

FUNCTION le_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode <= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION le_status_first_32

!-------------------------------------------------------------------------------

FUNCTION le_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode <= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION le_status_second_32

!-------------------------------------------------------------------------------

FUNCTION le_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode <= obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION le_status_both

!-------------------------------------------------------------------------------

FUNCTION ne_status_first_64(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int64), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode /= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ne_status_first_64

!-------------------------------------------------------------------------------

FUNCTION ne_status_second_64(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int64), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode /= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ne_status_second_64

!-------------------------------------------------------------------------------

FUNCTION ne_status_first_32(obj, int1) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  INTEGER(KIND=int32), INTENT(IN) :: int1
  LOGICAL :: eq
  
  IF (obj%icode /= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ne_status_first_32

!-------------------------------------------------------------------------------

FUNCTION ne_status_second_32(int1, obj) RESULT(eq)
  IMPLICIT NONE
  INTEGER(KIND=int32), INTENT(IN) :: int1
  CLASS(shum_ff_status_type), INTENT(IN) :: obj
  LOGICAL :: eq
  
  IF (obj%icode /= int1) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ne_status_second_32

!-------------------------------------------------------------------------------

FUNCTION ne_status_both(obj1, obj2) RESULT(eq)
  IMPLICIT NONE
  CLASS(shum_ff_status_type), INTENT(IN) :: obj1, obj2
  LOGICAL :: eq
  
  IF (obj1%icode /= obj2%icode) THEN
    eq = .TRUE.
  ELSE
    eq = .FALSE.
  END IF

END FUNCTION ne_status_both

!-------------------------------------------------------------------------------

END MODULE f_shum_ff_status_mod

