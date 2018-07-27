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
! Description: Functions to Transform Lat/Long values and Rotate wind vector 
!              values to Rotated Pole Equatorial Lat/Long coordinates.
!
MODULE f_shum_latlon_eq_grids_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

USE f_shum_conversions_mod, ONLY:                                              &
                                 shum_pi_over_180_const,                       &
                                 shum_180_over_pi_const
IMPLICIT NONE 

PRIVATE

PUBLIC :: f_shum_latlon_to_eq, f_shum_eq_to_latlon,                            &
          f_shum_latlon_to_eq_vector, f_shum_eq_to_latlon_vector,              &
          f_shum_latlon_eq_vector_coeff

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

INTERFACE f_shum_latlon_to_eq
  MODULE PROCEDURE                                                             &
      f_shum_lltoeq_arg64,                                                     &
      f_shum_lltoeq_arg64_single,                                              &
      f_shum_lltoeq_arg32,                                                     &
      f_shum_lltoeq_arg32_single
END INTERFACE

INTERFACE f_shum_eq_to_latlon
  MODULE PROCEDURE                                                             &
      f_shum_eqtoll_arg64,                                                     &
      f_shum_eqtoll_arg64_single,                                              &
      f_shum_eqtoll_arg32,                                                     &
      f_shum_eqtoll_arg32_single
END INTERFACE

INTERFACE f_shum_latlon_eq_vector_coeff
  MODULE PROCEDURE                                                             &
      f_shum_w_coeff_arg64, f_shum_w_coeff_arg32
END INTERFACE

INTERFACE f_shum_latlon_to_eq_vector
  MODULE PROCEDURE                                                             &
      f_shum_w_lltoeq_arg64, f_shum_w_lltoeq_arg32
END INTERFACE

INTERFACE f_shum_eq_to_latlon_vector
  MODULE PROCEDURE                                                             &
      f_shum_w_eqtoll_arg64, f_shum_w_eqtoll_arg32
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!
! Calculates latitude and longitude on equatorial latitude-longitude (eq) grid
! used in regional models from input arrays of latitude and longitude on
! standard grid. Both input and output latitudes and longitudes are in degrees
FUNCTION f_shum_lltoeq_arg64                                                   &
 (phi, lambda, phi_eq, lambda_eq, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: phi(:)               ! Lat (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: lambda(SIZE(phi))    ! Long (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: phi_pole             ! Lat pole (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_pole          ! Long pole (eq)
REAL(KIND=real64), INTENT(OUT) :: phi_eq(SIZE(phi))    ! Lat (eq)
REAL(KIND=real64), INTENT(OUT) :: lambda_eq(SIZE(phi)) ! Long (eq)

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

REAL(KIND=real64)   :: a_lambda
REAL(KIND=real64)   :: a_phi
REAL(KIND=real64)   :: e_lambda
REAL(KIND=real64)   :: e_phi
REAL(KIND=real64)   :: sin_phi_pole
REAL(KIND=real64)   :: cos_phi_pole
REAL(KIND=real64)   :: arg
REAL(KIND=real64)   :: term1
REAL(KIND=real64)   :: term2
REAL(KIND=real64)   :: lambda_zero
REAL(KIND=real64)   :: lambda_pole_local
INTEGER(KIND=int64) :: i

REAL(KIND=real64), PARAMETER :: small = 1.0e-5_real64

! Check pole co-ordinates are valid values
IF (ABS(lambda_pole) > 360.0_real64) THEN
  message = "Pole longitude invalid (must be between +/- 360.0)"
  status = 1
  RETURN
END IF

IF (ABS(phi_pole) > 90.0_real64) THEN
  message = "Pole latitude invalid (must be between +/- 90.0)"
  status = 1
  RETURN
END IF

! Ensure longitude of pole is between -180.0 and 180.0
lambda_pole_local = MOD(lambda_pole, 360.0_real64)
IF (ABS(lambda_pole_local) > 180.0_real64) THEN
  lambda_pole_local = lambda_pole_local - SIGN(360.0_real64, lambda_pole_local)
END IF

! Latitude of zeroth meridian
lambda_zero = lambda_pole_local + 180.0_real64

! Sine and cosine of latitude of eq pole
IF (ABS(phi_pole) == 90.0_real64) THEN
  sin_phi_pole = SIGN(1.0_real64, phi_pole)
  cos_phi_pole = SIGN(0.0_real64, phi_pole)
ELSE
  sin_phi_pole = SIN(shum_pi_over_180_const*phi_pole)
  cos_phi_pole = COS(shum_pi_over_180_const*phi_pole)
END IF

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i, a_lambda, a_phi, arg, term1, term2, e_lambda, e_phi)         &
!$OMP& SHARED(lambda, phi, lambda_zero, cos_phi_pole, sin_phi_pole,            &
!$OMP&        phi_eq, lambda_eq)
DO i=1,SIZE(phi)

  ! Scale longitude to range -180 to +180 degs
  a_lambda = MOD(lambda(i) - lambda_zero, 360.0_real64)
  IF (ABS(a_lambda) > 180.0_real64) THEN
    a_lambda = a_lambda - SIGN(360.0_real64, a_lambda)
  END IF

  ! Convert latitude & longitude to radians
  a_lambda = shum_pi_over_180_const*a_lambda
  a_phi    = shum_pi_over_180_const*phi(i)

  ! Compute eq latitude
  arg = -cos_phi_pole*COS(a_lambda)*COS(a_phi)                                 &
      + SIN(a_phi)*sin_phi_pole

  arg   = MIN(arg, 1.0_real64)
  arg   = MAX(arg,-1.0_real64)
  e_phi = ASIN(arg)
  phi_eq(i) = shum_180_over_pi_const*e_phi

  ! Compute eq longitude
  term1 = (COS(a_phi)*COS(a_lambda)*sin_phi_pole                               &
        + SIN(a_phi)*cos_phi_pole)
  term2 = COS(e_phi)
  IF (term2 < small) THEN
    e_lambda = shum_180_over_pi_const*a_lambda
  ELSE
    arg = term1/term2
    arg = MIN(arg, 1.0_real64)
    arg = MAX(arg,-1.0_real64)
    e_lambda = shum_180_over_pi_const*ACOS(arg)
    e_lambda = SIGN(e_lambda,a_lambda)
  END IF

  ! Scale equatorial longitude to range 0 to 360 degs for use
  e_lambda = MOD(e_lambda, 360.0_real64)
  IF (ABS(e_lambda) < small) THEN
    e_lambda = 0.0_real64
  ELSE IF (e_lambda < 0.0_real64) THEN
    e_lambda = e_lambda + 360.0_real64
  END IF

  lambda_eq(i)=e_lambda

END DO
!$OMP END PARALLEL DO

status = 0_int64

END FUNCTION f_shum_lltoeq_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_lltoeq_arg64_single                                            &
 (phi, lambda, phi_eq, lambda_eq, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: phi         ! Lat (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: lambda      ! Long (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: phi_pole    ! Lat pole (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_pole ! Long pole (eq)
REAL(KIND=real64), INTENT(OUT) :: phi_eq      ! Lat (eq)
REAL(KIND=real64), INTENT(OUT) :: lambda_eq   ! Long (eq)

REAL(KIND=real64) :: phi_arr(1)    
REAL(KIND=real64) :: lambda_arr(1) 
REAL(KIND=real64) :: phi_eq_arr(1) 
REAL(KIND=real64) :: lambda_eq_arr(1)

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

phi_arr(1) = phi
lambda_arr(1) = lambda

status = f_shum_lltoeq_arg64(phi_arr, lambda_arr, phi_eq_arr, lambda_eq_arr,   &
                             phi_pole, lambda_pole, message)

IF (status == 0_int64) THEN
  phi_eq = phi_eq_arr(1)
  lambda_eq = lambda_eq_arr(1)
END IF

END FUNCTION f_shum_lltoeq_arg64_single

!------------------------------------------------------------------------------!

FUNCTION f_shum_lltoeq_arg32                                                   &
 (phi, lambda, phi_eq, lambda_eq, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: phi(:)               ! Lat (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: lambda(SIZE(phi))    ! Long (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: phi_pole             ! Lat pole (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_pole          ! Long pole (eq)
REAL(KIND=real32), INTENT(OUT) :: phi_eq(SIZE(phi))    ! Lat (eq)
REAL(KIND=real32), INTENT(OUT) :: lambda_eq(SIZE(phi)) ! Long (eq)

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

REAL(KIND=real64)   :: phi64(SIZE(phi))
REAL(KIND=real64)   :: lambda64(SIZE(phi))
REAL(KIND=real64)   :: phi_pole64
REAL(KIND=real64)   :: lambda_pole64
REAL(KIND=real64)   :: phi_eq64(SIZE(phi))
REAL(KIND=real64)   :: lambda_eq64(SIZE(phi))
INTEGER(KIND=int64) :: i

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i) SHARED(phi, phi64, lambda, lambda64)
DO i = 1, SIZE(phi)
  phi64(i) = REAL(phi(i), KIND=real64)
  lambda64(i) = REAL(lambda(i), KIND=real64)
END DO
!$OMP END PARALLEL DO

phi_pole64 = REAL(phi_pole, KIND=real64)
lambda_pole64 = REAL(lambda_pole, KIND=real64)

status = f_shum_lltoeq_arg64(phi64, lambda64, phi_eq64, lambda_eq64,           &
                             phi_pole64, lambda_pole64, message)

IF (status == 0_int32) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(phi_eq, phi_eq64, lambda_eq, lambda_eq64, phi)
  DO i = 1, SIZE(phi)
    phi_eq(i) = REAL(phi_eq64(i), KIND=real32)
    lambda_eq(i) = REAL(lambda_eq64(i), KIND=real32)
  END DO
  !$OMP END PARALLEL DO
END IF

END FUNCTION f_shum_lltoeq_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_lltoeq_arg32_single                                            &
 (phi, lambda, phi_eq, lambda_eq, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: phi         ! Lat (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: lambda      ! Long (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: phi_pole    ! Lat pole (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_pole ! Long pole (eq)
REAL(KIND=real32), INTENT(OUT) :: phi_eq      ! Lat (eq)
REAL(KIND=real32), INTENT(OUT) :: lambda_eq   ! Long (eq)

REAL(KIND=real64) :: phi_arr64(1)    
REAL(KIND=real64) :: lambda_arr64(1) 
REAL(KIND=real64) :: phi_eq_arr64(1) 
REAL(KIND=real64) :: lambda_eq_arr64(1)
REAL(KIND=real64) :: phi_pole64
REAL(KIND=real64) :: lambda_pole64

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

phi_arr64(1) = REAL(phi, KIND=real64)
lambda_arr64(1) = REAL(lambda, KIND=real64)

phi_pole64 = REAL(phi_pole, KIND=real64)
lambda_pole64 = REAL(lambda_pole, KIND=real64)

status = f_shum_lltoeq_arg64(phi_arr64, lambda_arr64,                          &
                             phi_eq_arr64, lambda_eq_arr64,                    &
                             phi_pole64, lambda_pole64, message)

IF (status == 0_int32) THEN
  phi_eq = REAL(phi_eq_arr64(1), KIND=real32)
  lambda_eq = REAL(lambda_eq_arr64(1), KIND=real32)
END IF

END FUNCTION f_shum_lltoeq_arg32_single

!------------------------------------------------------------------------------!
! Calculates latitude and longitude on standard grid from input arrays of
! latitude and longitude on equatorial latitude-longitude (eq) grid used in
! regional models. Both input and output latitudes and longitudes are in
! degrees
FUNCTION f_shum_eqtoll_arg64                                                   &
 (phi_eq, lambda_eq, phi, lambda, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: phi_eq(:)               ! Lat (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_eq(SIZE(phi_eq)) ! Long (eq)
REAL(KIND=real64), INTENT(IN)  :: phi_pole                ! Lat pole (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_pole             ! Long pole (eq)
REAL(KIND=real64), INTENT(OUT) :: phi(SIZE(phi_eq))       ! Lat (lat-lon)
REAL(KIND=real64), INTENT(OUT) :: lambda(SIZE(phi_eq))    ! Long (lat-lon)

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

REAL(KIND=real64)   :: a_lambda
REAL(KIND=real64)   :: a_phi
REAL(KIND=real64)   :: e_lambda
REAL(KIND=real64)   :: e_phi
REAL(KIND=real64)   :: sin_phi_pole
REAL(KIND=real64)   :: cos_phi_pole
REAL(KIND=real64)   :: arg
REAL(KIND=real64)   :: term1
REAL(KIND=real64)   :: term2
REAL(KIND=real64)   :: lambda_zero
REAL(KIND=real64)   :: lambda_pole_local
INTEGER(KIND=int64) :: i

REAL(KIND=real64), PARAMETER :: small = 1.0e-5_real64

! Check pole co-ordinates are valid values
IF (ABS(lambda_pole) > 360.0_real64) THEN
  message = "Pole longitude invalid (must be between +/- 360.0)"
  status = 1
  RETURN
END IF

IF (ABS(phi_pole) > 90.0_real64) THEN
  message = "Pole latitude invalid (must be between +/- 90.0)"
  status = 1
  RETURN
END IF

! Ensure longitude of pole is between -180.0 and 180.0
lambda_pole_local = MOD(lambda_pole, 360.0_real64)
IF (ABS(lambda_pole_local) > 180.0_real64) THEN
  lambda_pole_local = lambda_pole_local - SIGN(360.0_real64, lambda_pole_local)
END IF

! Latitude of zeroth meridian
lambda_zero = lambda_pole_local + 180.0_real64

! Sine and cosine of latitude of eq pole
IF (ABS(phi_pole) == 90.0_real64) THEN
  sin_phi_pole = SIGN(1.0_real64, phi_pole)
  cos_phi_pole = SIGN(0.0_real64, phi_pole)
ELSE
  sin_phi_pole = SIN(shum_pi_over_180_const*phi_pole)
  cos_phi_pole = COS(shum_pi_over_180_const*phi_pole)
END IF

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i, a_lambda, a_phi, arg, term1, term2, e_lambda, e_phi)         &
!$OMP& SHARED(lambda, phi, lambda_zero, cos_phi_pole, sin_phi_pole,            &
!$OMP&        phi_eq, lambda_eq)
DO i=1,SIZE(phi_eq)

  ! Scale eq longitude to range -180 to +180 degs
  e_lambda = MOD(lambda_eq(i), 360.0_real64)
  IF (ABS(e_lambda) > 180.0_real64) THEN
    e_lambda = e_lambda - SIGN(360.0_real64, e_lambda)
  END IF
 
  ! Convert eq latitude & longitude to radians
  e_lambda = shum_pi_over_180_const*e_lambda
  e_phi    = shum_pi_over_180_const*phi_eq(i)

  ! Compute latitude
  arg = cos_phi_pole*COS(e_lambda)*COS(e_phi)                                  &
      + SIN(e_phi)*sin_phi_pole

  arg    = MIN(arg, 1.0_real64)
  arg    = MAX(arg,-1.0_real64)
  a_phi  = ASIN(arg)
  phi(i) = shum_180_over_pi_const*a_phi

  ! Compute longitude
  term1 = (COS(e_phi)*COS(e_lambda)*sin_phi_pole                               &
        - SIN(e_phi)*cos_phi_pole)
  term2 = COS(a_phi)
  IF (term2 < small) THEN
    a_lambda = 0.0_real64
  ELSE
    arg = term1/term2
    arg = MIN(arg, 1.0_real64)
    arg = MAX(arg,-1.0_real64)
    a_lambda = shum_180_over_pi_const*ACOS(arg)
    a_lambda = SIGN(a_lambda, e_lambda)
    a_lambda = a_lambda + lambda_zero
  END IF

  ! Scale longitude to range 0 to 360 degs
  a_lambda = MOD(a_lambda, 360.0_real64)
  IF (ABS(a_lambda) < small) THEN
    a_lambda = 0.0_real64
  ELSE IF (a_lambda < 0.0_real64) THEN
    a_lambda = a_lambda + 360.0_real64
  END IF
  lambda(i) = a_lambda

END DO
!$OMP END PARALLEL DO

status = 0_int64

END FUNCTION f_shum_eqtoll_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_eqtoll_arg64_single                                            &
 (phi_eq, lambda_eq, phi, lambda, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: phi_eq      ! Lat (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_eq   ! Long (eq)
REAL(KIND=real64), INTENT(IN)  :: phi_pole    ! Lat pole (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_pole ! Long pole (eq)
REAL(KIND=real64), INTENT(OUT) :: phi         ! Lat (lat-lon)
REAL(KIND=real64), INTENT(OUT) :: lambda      ! Long (lat-lon)   

REAL(KIND=real64) :: phi_eq_arr(1) 
REAL(KIND=real64) :: lambda_eq_arr(1)
REAL(KIND=real64) :: phi_arr(1)    
REAL(KIND=real64) :: lambda_arr(1) 

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

phi_eq_arr(1) = phi_eq
lambda_eq_arr(1) = lambda_eq

status = f_shum_eqtoll_arg64(phi_eq_arr, lambda_eq_arr, phi_arr, lambda_arr,   &
                             phi_pole, lambda_pole, message)

IF (status == 0_int64) THEN
  phi = phi_arr(1)
  lambda = lambda_arr(1)
END IF

END FUNCTION f_shum_eqtoll_arg64_single

!------------------------------------------------------------------------------!

FUNCTION f_shum_eqtoll_arg32                                                   &
 (phi_eq, lambda_eq, phi, lambda, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: phi_eq(:)               ! Lat (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_eq(SIZE(phi_eq)) ! Long (eq)
REAL(KIND=real32), INTENT(IN)  :: phi_pole                ! Lat pole (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_pole             ! Long pole (eq)
REAL(KIND=real32), INTENT(OUT) :: phi(SIZE(phi_eq))       ! Lat (lat-lon)
REAL(KIND=real32), INTENT(OUT) :: lambda(SIZE(phi_eq))    ! Long (lat-lon)

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

REAL(KIND=real64)   :: phi64(SIZE(phi_eq))
REAL(KIND=real64)   :: lambda64(SIZE(phi_eq))
REAL(KIND=real64)   :: phi_pole64
REAL(KIND=real64)   :: lambda_pole64
REAL(KIND=real64)   :: phi_eq64(SIZE(phi_eq))
REAL(KIND=real64)   :: lambda_eq64(SIZE(phi_eq))
INTEGER(KIND=int64) :: i

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i) SHARED(phi_eq, phi_eq64, lambda_eq, lambda_eq64)
DO i = 1, SIZE(phi_eq)
  phi_eq64(i) = REAL(phi_eq(i), KIND=real64)
  lambda_eq64(i) = REAL(lambda_eq(i), KIND=real64)
END DO
!$OMP END PARALLEL DO

phi_pole64 = REAL(phi_pole, KIND=real64)
lambda_pole64 = REAL(lambda_pole, KIND=real64)

status = f_shum_eqtoll_arg64(phi_eq64, lambda_eq64, phi64, lambda64,           &
                             phi_pole64, lambda_pole64, message)

IF (status == 0_int32) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(phi, phi64, lambda, lambda64, phi_eq)
  DO i = 1, SIZE(phi_eq)
    phi(i) = REAL(phi64(i), KIND=real32)
    lambda(i) = REAL(lambda64(i), KIND=real32)
  END DO
  !$OMP END PARALLEL DO
END IF

END FUNCTION f_shum_eqtoll_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_eqtoll_arg32_single                                            &
 (phi_eq, lambda_eq, phi, lambda, phi_pole, lambda_pole, message) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: phi_eq      ! Lat (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_eq   ! Long (eq)
REAL(KIND=real32), INTENT(IN)  :: phi_pole    ! Lat pole (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_pole ! Long pole (eq)
REAL(KIND=real32), INTENT(OUT) :: phi         ! Lat (lat-lon)
REAL(KIND=real32), INTENT(OUT) :: lambda      ! Long (lat-lon)

REAL(KIND=real64) :: phi_arr64(1)    
REAL(KIND=real64) :: lambda_arr64(1) 
REAL(KIND=real64) :: phi_eq_arr64(1) 
REAL(KIND=real64) :: lambda_eq_arr64(1)
REAL(KIND=real64) :: phi_pole64
REAL(KIND=real64) :: lambda_pole64

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

phi_eq_arr64(1) = REAL(phi_eq, KIND=real64)
lambda_eq_arr64(1) = REAL(lambda_eq, KIND=real64)

phi_pole64 = REAL(phi_pole, KIND=real64)
lambda_pole64 = REAL(lambda_pole, KIND=real64)

status = f_shum_eqtoll_arg64(phi_eq_arr64, lambda_eq_arr64,                    &
                             phi_arr64, lambda_arr64,                          &
                             phi_pole64, lambda_pole64, message)

IF (status == 0_int32) THEN
  phi = REAL(phi_arr64(1), KIND=real32)
  lambda = REAL(lambda_arr64(1), KIND=real32)
END IF

END FUNCTION f_shum_eqtoll_arg32_single

!------------------------------------------------------------------------------!
! Calculates coefficients used to translate u and v components of wind between
! equatorial (eq) latitude-longitude grid and standard (ll) latitude-longitude
! grid (or vice versa).  Input latitudes and longitudes are in degrees.
FUNCTION f_shum_w_coeff_arg64                                                  &
           (coeff1, coeff2, lambda, lambda_eq, phi_pole, lambda_pole, message) &
                                                                RESULT(status)
IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: lambda(:)               ! Long (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: lambda_eq(SIZE(lambda)) ! Long (eq)
REAL(KIND=real64), INTENT(IN)  :: phi_pole                ! Lat pole (eq)
REAL(KIND=real64), INTENT(IN)  :: lambda_pole             ! Long pole (eq)
REAL(KIND=real64), INTENT(OUT) :: coeff1(SIZE(lambda))    ! Rotation coeff 1
REAL(KIND=real64), INTENT(OUT) :: coeff2(SIZE(lambda))    ! Rotation coeff 2

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

REAL(KIND=real64)   :: a_lambda
REAL(KIND=real64)   :: e_lambda
REAL(KIND=real64)   :: sin_e_lambda
REAL(KIND=real64)   :: sin_phi_pole
REAL(KIND=real64)   :: cos_phi_pole
REAL(KIND=real64)   :: c1
REAL(KIND=real64)   :: c2
REAL(KIND=real64)   :: lambda_zero
REAL(KIND=real64)   :: lambda_pole_local
INTEGER(KIND=int64) :: i

! Check pole co-ordinates are valid values
IF (ABS(lambda_pole) > 360.0_real64) THEN
  message = "Pole longitude invalid (must be between +/- 360.0)"
  status = 1
  RETURN
END IF

IF (ABS(phi_pole) > 90.0_real64) THEN
  message = "Pole latitude invalid (must be between +/- 90.0)"
  status = 1
  RETURN
END IF

! Ensure longitude of pole is between -180.0 and 180.0
lambda_pole_local = MOD(lambda_pole, 360.0_real64)
IF (ABS(lambda_pole_local) > 180.0_real64) THEN
  lambda_pole_local = lambda_pole_local - SIGN(360.0_real64, lambda_pole_local)
END IF

! Longitude of zeroth meridian
lambda_zero = lambda_pole_local + 180.0_real64

! Sine and cosine of latitude of eq pole
IF (ABS(phi_pole) == 90.0_real64) THEN
  sin_phi_pole = SIGN(1.0_real64, phi_pole)
  cos_phi_pole = SIGN(0.0_real64, phi_pole)
ELSE
  sin_phi_pole = SIN(shum_pi_over_180_const*phi_pole)
  cos_phi_pole = COS(shum_pi_over_180_const*phi_pole)
END IF


!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i, a_lambda, e_lambda, sin_e_lambda, c1, c2)                    &
!$OMP& SHARED(lambda, lambda_zero, lambda_eq, coeff1, coeff2, sin_phi_pole,    &
!$OMP&        cos_phi_pole)
DO i=1,SIZE(lambda)

  ! Convert longitudes to radians (and ensure range is correct)
  a_lambda = MOD(lambda(i) - lambda_zero, 360.0_real64)
  IF (ABS(a_lambda) > 180.0_real64) THEN
    a_lambda = a_lambda - SIGN(360.0_real64, a_lambda)
  END IF

  a_lambda = shum_pi_over_180_const*a_lambda

  e_lambda = MOD(lambda_eq(i), 360.0_real64)
  IF (ABS(e_lambda) > 180.0_real64) THEN
    e_lambda = e_lambda - SIGN(360.0_real64, e_lambda)
  END IF

  e_lambda = e_lambda*shum_pi_over_180_const

  ! Take sine of eq lambda
  IF (ABS(lambda_eq(i)) == 180.0_real64) THEN
    sin_e_lambda = 0.0_real64  ! to avoid spurious c1 result
  ELSE
    sin_e_lambda = SIN(e_lambda)
  END IF

  c1 = SIN(a_lambda) * sin_e_lambda * sin_phi_pole                             &
     + COS(a_lambda) * COS(e_lambda)

  ! Avoid rounding error problems
  IF (ABS(c1) >= 1.0_real64) THEN
    c1 = SIGN(1.0_real64, c1)
    c2 = 0.0_real64
  ELSE
    c2 = SQRT(1.0_real64 - c1*c1)
  END IF

  coeff1(i) = c1

  ! Set the sign of C2 - the 0.0 case represents an edge-case where there
  ! are more than one solution; ensure we always get the same answer
  IF (ABS(sin_e_lambda*cos_phi_pole) == 0.0_real64) THEN
    coeff2(i) = c2
  ELSE
    coeff2(i) = SIGN(c2, sin_e_lambda*cos_phi_pole)
  END IF


END DO
!$OMP END PARALLEL DO

status = 0_int64

END FUNCTION f_shum_w_coeff_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_w_coeff_arg32                                                  &
           (coeff1, coeff2, lambda, lambda_eq, phi_pole, lambda_pole, message) &
                                                                RESULT(status)
IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: lambda(:)               ! Long (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: lambda_eq(SIZE(lambda)) ! Long (eq)
REAL(KIND=real32), INTENT(IN)  :: phi_pole                ! Lat pole (eq)
REAL(KIND=real32), INTENT(IN)  :: lambda_pole             ! Long pole (eq)
REAL(KIND=real32), INTENT(OUT) :: coeff1(SIZE(lambda))    ! Rotation coeff 1
REAL(KIND=real32), INTENT(OUT) :: coeff2(SIZE(lambda))    ! Rotation coeff 2

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

REAL(KIND=real64)   :: lambda64(SIZE(lambda))
REAL(KIND=real64)   :: lambda_eq64(SIZE(lambda))
REAL(KIND=real64)   :: phi_pole64
REAL(KIND=real64)   :: lambda_pole64
REAL(KIND=real64)   :: coeff1_64(SIZE(lambda))
REAL(KIND=real64)   :: coeff2_64(SIZE(lambda))
INTEGER(KIND=int64) :: i

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i) SHARED(lambda, lambda64, lambda_eq, lambda_eq64)
DO i = 1, SIZE(lambda)
  lambda64(i) = REAL(lambda(i), KIND=real64)
  lambda_eq64(i) = REAL(lambda_eq(i), KIND=real64)
END DO
!$OMP END PARALLEL DO

phi_pole64 = REAL(phi_pole, KIND=real64)
lambda_pole64 = REAL(lambda_pole, KIND=real64)

status = f_shum_w_coeff_arg64(coeff1_64, coeff2_64, lambda64, lambda_eq64,     &
                              phi_pole64, lambda_pole64, message)

IF (status == 0_int32) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(coeff1, coeff1_64, coeff2, coeff2_64, lambda)
  DO i = 1, SIZE(lambda)
    coeff1(i) = REAL(coeff1_64(i), KIND=real32)
    coeff2(i) = REAL(coeff2_64(i), KIND=real32)
  END DO
  !$OMP END PARALLEL DO
END IF

END FUNCTION f_shum_w_coeff_arg32
!------------------------------------------------------------------------------!
! Calculates u and v components of wind on standard latitude-longitude (ll) grid
! by rotating wind components on equatorial latitude-longitude (eq) grid.
FUNCTION f_shum_w_eqtoll_arg64                                                 &
                 (coeff1, coeff2, u_eq, v_eq, u, v, message, mdi) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: coeff1(:)            ! Rotation coeff 1
REAL(KIND=real64), INTENT(IN)  :: coeff2(SIZE(coeff1)) ! Rotation coeff 2
REAL(KIND=real64), INTENT(IN)  :: u_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real64), INTENT(IN)  :: v_eq(SIZE(coeff1))   ! Wind V compt (eq)
REAL(KIND=real64), INTENT(OUT) :: u(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real64), INTENT(OUT) :: v(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real64), INTENT(IN), OPTIONAL :: mdi         ! Missing data value

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

LOGICAL             :: l_mdi ! Was an mdi value provided?
INTEGER(KIND=int64) :: i

! Check if an mdi value has been provided
l_mdi = PRESENT(mdi)

! Apply coefficients (omitting mdi values if required)
IF (l_mdi) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(coeff1, coeff2, u, v, u_eq, v_eq, mdi)
  DO i=1, SIZE(coeff1)
    IF ( u_eq(i) == mdi .OR. v_eq(i) == mdi ) THEN
      u(i) = mdi
      v(i) = mdi
    ELSE
      u(i) = coeff1(i)*u_eq(i) + coeff2(i)*v_eq(i)
      v(i) = coeff1(i)*v_eq(i) - coeff2(i)*u_eq(i)
    END IF
  END DO
  !$OMP END PARALLEL DO
ELSE
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(coeff1, coeff2, u, v, u_eq, v_eq)
  DO i=1, SIZE(coeff1)
    u(i) = coeff1(i)*u_eq(i) + coeff2(i)*v_eq(i)
    v(i) = coeff1(i)*v_eq(i) - coeff2(i)*u_eq(i)
  END DO
  !$OMP END PARALLEL DO
END IF

status = 0_int64

END FUNCTION f_shum_w_eqtoll_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_w_eqtoll_arg32                                                 &
                 (coeff1, coeff2, u_eq, v_eq, u, v, message, mdi) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: coeff1(:)            ! Rotation coeff 1
REAL(KIND=real32), INTENT(IN)  :: coeff2(SIZE(coeff1)) ! Rotation coeff 2
REAL(KIND=real32), INTENT(IN)  :: u_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real32), INTENT(IN)  :: v_eq(SIZE(coeff1))   ! Wind V compt (eq)
REAL(KIND=real32), INTENT(OUT) :: u(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real32), INTENT(OUT) :: v(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real32), INTENT(IN), OPTIONAL :: mdi         ! Missing data value

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

REAL(KIND=real64)   :: coeff1_64(SIZE(coeff1))
REAL(KIND=real64)   :: coeff2_64(SIZE(coeff1))
REAL(KIND=real64)   :: u_eq_64(SIZE(coeff1))
REAL(KIND=real64)   :: v_eq_64(SIZE(coeff1))
REAL(KIND=real64)   :: u_64(SIZE(coeff1))
REAL(KIND=real64)   :: v_64(SIZE(coeff1))
REAL(KIND=real64)   :: mdi_64  
LOGICAL             :: l_mdi 
INTEGER(KIND=int64) :: i

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i) SHARED(coeff1, coeff1_64, coeff2, coeff2_64, u_eq, u_eq_64,  &
!$OMP&                   v_eq, v_eq_64)
DO i = 1, SIZE(coeff1)
  coeff1_64(i) = REAL(coeff1(i), KIND=real64)
  coeff2_64(i) = REAL(coeff2(i), KIND=real64)
  u_eq_64(i) = REAL(u_eq(i), KIND=real64)
  v_eq_64(i) = REAL(v_eq(i), KIND=real64)
END DO
!$OMP END PARALLEL DO

IF (PRESENT(mdi)) THEN
  mdi_64 = REAL(mdi, KIND=real64)
  status = f_shum_w_eqtoll_arg64(coeff1_64, coeff2_64, u_eq_64, v_eq_64,       &
                                 u_64, v_64, message, mdi_64)
ELSE
  status = f_shum_w_eqtoll_arg64(coeff1_64, coeff2_64, u_eq_64, v_eq_64,       &
                                 u_64, v_64, message)
END IF

IF (status == 0_int32) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(u, u_64, v, v_64, coeff1)
  DO i = 1, SIZE(coeff1)
    u(i) = REAL(u_64(i), KIND=real32)
    v(i) = REAL(v_64(i), KIND=real32)
  END DO
  !$OMP END PARALLEL DO
END IF

END FUNCTION f_shum_w_eqtoll_arg32

!------------------------------------------------------------------------------!
! Calculates u and v components of wind on equatorial (eq) latitude-longitude
! grid by rotating wind components on standard latitude-longitude (ll) grid.
FUNCTION f_shum_w_lltoeq_arg64                                                 &
                 (coeff1, coeff2, u, v, u_eq, v_eq, message, mdi) RESULT(status)

IMPLICIT NONE

REAL(KIND=real64), INTENT(IN)  :: coeff1(:)            ! Rotation coeff 1
REAL(KIND=real64), INTENT(IN)  :: coeff2(SIZE(coeff1)) ! Rotation coeff 2
REAL(KIND=real64), INTENT(IN)  :: u(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real64), INTENT(IN)  :: v(SIZE(coeff1))      ! Wind V compt (lat-lon)
REAL(KIND=real64), INTENT(OUT) :: u_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real64), INTENT(OUT) :: v_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real64), INTENT(IN), OPTIONAL :: mdi         ! Missing data value

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int64) :: status

LOGICAL             :: l_mdi ! Was an mdi value provided?
INTEGER(KIND=int64) :: i

! Check if an mdi value has been provided
l_mdi = PRESENT(mdi)

! Apply coefficients (omitting mdi values if required)
IF (l_mdi) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(coeff1, coeff2, u, v, u_eq, v_eq, mdi)
  DO i=1, SIZE(coeff1)
    IF ( u(i) == mdi .OR. v(i) == mdi ) THEN
      u_eq(i) = mdi
      v_eq(i) = mdi
    ELSE
      u_eq(i) = coeff1(i)*u(i)-coeff2(i)*v(i)
      v_eq(i) = coeff1(i)*v(i)+coeff2(i)*u(i)
    END IF
  END DO
  !$OMP END PARALLEL DO
ELSE
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(coeff1, coeff2, u, v, u_eq, v_eq)
  DO i=1, SIZE(coeff1)
    u_eq(i) = coeff1(i)*u(i)-coeff2(i)*v(i)
    v_eq(i) = coeff1(i)*v(i)+coeff2(i)*u(i)
  END DO
  !$OMP END PARALLEL DO
END IF

status = 0_int64

END FUNCTION f_shum_w_lltoeq_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_w_lltoeq_arg32                                                 &
                 (coeff1, coeff2, u, v, u_eq, v_eq, message, mdi) RESULT(status)

IMPLICIT NONE

REAL(KIND=real32), INTENT(IN)  :: coeff1(:)            ! Rotation coeff 1
REAL(KIND=real32), INTENT(IN)  :: coeff2(SIZE(coeff1)) ! Rotation coeff 2
REAL(KIND=real32), INTENT(IN)  :: u(SIZE(coeff1))      ! Wind U compt (lat-lon)
REAL(KIND=real32), INTENT(IN)  :: v(SIZE(coeff1))      ! Wind V compt (lat-lon)
REAL(KIND=real32), INTENT(OUT) :: u_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real32), INTENT(OUT) :: v_eq(SIZE(coeff1))   ! Wind U compt (eq)
REAL(KIND=real32), INTENT(IN), OPTIONAL :: mdi         ! Missing data value

CHARACTER(LEN=*)    :: message
INTEGER(KIND=int32) :: status

REAL(KIND=real64)   :: coeff1_64(SIZE(coeff1))
REAL(KIND=real64)   :: coeff2_64(SIZE(coeff1))
REAL(KIND=real64)   :: u_eq_64(SIZE(coeff1))
REAL(KIND=real64)   :: v_eq_64(SIZE(coeff1))
REAL(KIND=real64)   :: u_64(SIZE(coeff1))
REAL(KIND=real64)   :: v_64(SIZE(coeff1))
REAL(KIND=real64)   :: mdi_64  
LOGICAL             :: l_mdi 
INTEGER(KIND=int64) :: i

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(i) SHARED(coeff1, coeff1_64, coeff2, coeff2_64, u, u_64, v, v_64)
DO i = 1, SIZE(coeff1)
  coeff1_64(i) = REAL(coeff1(i), KIND=real64)
  coeff2_64(i) = REAL(coeff2(i), KIND=real64)
  u_64(i) = REAL(u(i), KIND=real64)
  v_64(i) = REAL(v(i), KIND=real64)
END DO
!$OMP END PARALLEL DO

IF (PRESENT(mdi)) THEN
  mdi_64 = REAL(mdi, KIND=real64)
  status = f_shum_w_lltoeq_arg64(coeff1_64, coeff2_64, u_64, v_64,             &
                                 u_eq_64, v_eq_64, message, mdi_64)
ELSE
  status = f_shum_w_lltoeq_arg64(coeff1_64, coeff2_64, u_64, v_64,             &
                                 u_eq_64, v_eq_64, message)  
END IF

IF (status == 0_int32) THEN
  !$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                            &
  !$OMP& PRIVATE(i) SHARED(u_eq, u_eq_64, v_eq, v_eq_64, coeff1)
  DO i = 1, SIZE(coeff1)
    u_eq(i) = REAL(u_eq_64(i), KIND=real32)
    v_eq(i) = REAL(v_eq_64(i), KIND=real32)
  END DO
  !$OMP END PARALLEL DO
END IF

END FUNCTION f_shum_w_lltoeq_arg32

!------------------------------------------------------------------------------!

END MODULE f_shum_latlon_eq_grids_mod
