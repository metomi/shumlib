! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
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
!
!*******************************************************************************
!
! Description : Routines to perform bi-liner interpolation on a horizontal
!             : field. Calculating the coefficients and then using them to
!             : calculate the new value.
!
MODULE f_shum_horizontal_field_interp_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

IMPLICIT NONE

PRIVATE

PUBLIC :: f_shum_horizontal_field_bi_lin_interp_get_coeffs                     &
        , f_shum_horizontal_field_bi_lin_interp_calc                           &
        , f_shum_find_source_box_indices                                       &
        , f_shum_calc_weights                                                  &
        , f_shum_cart_horizontal_field_bi_lin_interp_get_coeffs                &
        , f_shum_find_source_cart_box_indices                                  &
        , f_shum_calc_cart_weights


!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
INTEGER, PARAMETER :: shum_int64  = C_INT64_T
INTEGER, PARAMETER :: shum_int32  = C_INT32_T
INTEGER, PARAMETER :: shum_real64 = C_DOUBLE
INTEGER, PARAMETER :: shum_real32 = C_FLOAT
INTEGER, PARAMETER :: shum_bool   = C_BOOL
!------------------------------------------------------------------------------!

INTEGER(KIND=shum_int64), PARAMETER :: zero64 = 0_shum_int64
INTEGER(KIND=shum_int64), PARAMETER :: one64  = 1_shum_int64
INTEGER(KIND=shum_int64), PARAMETER :: two64  = 2_shum_int64

!------------------------------------------------------------------------------!

CONTAINS

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_horizontal_field_bi_lin_interp_calc                          &
                  ( rows_in, row_length_in, len_field_out                      &
                  , index_b_l, index_b_r, index_t_l, index_t_r, data_in        &
                  , weight_b_l, weight_b_r, weight_t_l, weight_t_r             &
                  , data_out )

USE f_shum_is_denormal_mod, ONLY: f_shum_is_denormal

IMPLICIT NONE

! Description:
!   Carries out bi-linear horizontal interpolation using coefficients
!   and gather indices calculated in subroutine
!   f_shum_horizontal_field_bi-lin_interp_get_coeffs

! Subroutine arguments
! Scalar arguments
INTEGER(KIND=shum_int64), INTENT(IN) :: rows_in
                                 ! Number of P rows on source grid
INTEGER(KIND=shum_int64), INTENT(IN) :: row_length_in
                                 ! Number of pts per row on source grid
INTEGER(KIND=shum_int64), INTENT(IN) :: len_field_out
                                 ! Number of points on target grid

! Array arguments
INTEGER(KIND=shum_int64), INTENT(IN) :: index_b_l(len_field_out)
                                 ! Index of bottom left
                                 ! corner of source gridbox
INTEGER(KIND=shum_int64), INTENT(IN) :: index_b_r(len_field_out)
                                 ! Index of bottom right
                                 ! corner of source gridbox
INTEGER(KIND=shum_int64), INTENT(IN) :: index_t_l(len_field_out)
                                 ! Index of top left
                                 ! corner of source gridbox
INTEGER(KIND=shum_int64), INTENT(IN) :: index_t_r(len_field_out)
                                 ! Index of top right
                                 ! corner of source gridbox

REAL(KIND=shum_real64), INTENT(IN) :: data_in(rows_in*row_length_in)
                               ! Data before interpolation

REAL(KIND=shum_real64), INTENT(IN) :: weight_b_l(len_field_out)
                               ! Weight applied to value at bot.
                               ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(IN) :: weight_b_r(len_field_out)
                               ! Weight applied to value at bot.
                               ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(IN) :: weight_t_l(len_field_out)
                               ! Weight applied to value at top
                               ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(IN) :: weight_t_r(len_field_out)
                               ! Weight applied to value at top
                               ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: data_out(len_field_out)
                                ! Data after interpolation

! Local scalars:
INTEGER(KIND=shum_int64) :: i  ! loop index

! Local reals
REAL(KIND=shum_real64) ::                                                      &
  data_out_a,                                                                  &
  data_out_b,                                                                  &
  data_out_c,                                                                  &
  data_out_d

! Data after interpolation

! 1. Carry out horizontal interpolation

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP          SHARED(len_field_out, data_out, weight_b_l, weight_b_r,         &
!$OMP                 weight_t_l, weight_t_r, index_b_l, index_b_r,            &
!$OMP                 index_t_l, index_t_r,                                    &
!$OMP                 data_in)                                                 &
!$OMP          PRIVATE(i, data_out_a, data_out_b, data_out_c, data_out_d )
DO i=1, len_field_out

  ! Weights should be in the range 0.0 <= weight <= 1.0
  ! This means the result cannot overflow. We still need to check for
  ! underflow.

  ! b_l component

  IF (f_shum_is_denormal(data_in(index_b_l(i))) .OR.                           &
      f_shum_is_denormal(weight_b_l(i)) .OR.                                   &
      weight_b_l(i) < TINY(data_out_a) ) THEN
    data_out_a = 0.0
  ELSE
    IF (ABS(data_in(index_b_l(i))) < TINY(data_out_a) / weight_b_l(i)) THEN
      data_out_a = 0.0
    ELSE
      data_out_a = weight_b_l(i)*data_in(index_b_l(i))
    END IF
  END IF

  ! b_r component

  IF (f_shum_is_denormal(data_in(index_b_r(i))) .OR.                           &
      f_shum_is_denormal(weight_b_r(i)) .OR.                                   &
      weight_b_r(i) < TINY(data_out_b) ) THEN
    data_out_b = 0.0
  ELSE
    IF (ABS(data_in(index_b_r(i))) < TINY(data_out_b) / weight_b_r(i)) THEN
      data_out_b = 0.0
    ELSE
      data_out_b = weight_b_r(i)*data_in(index_b_r(i))
    END IF
  END IF

  ! t_l component

  IF (f_shum_is_denormal(data_in(index_t_l(i))) .OR.                           &
      f_shum_is_denormal(weight_t_l(i)) .OR.                                   &
      weight_t_l(i) < TINY(data_out_c) ) THEN
    data_out_c = 0.0
  ELSE
    IF (ABS(data_in(index_t_l(i))) < TINY(data_out_c) / weight_t_l(i)) THEN
      data_out_c = 0.0
    ELSE
      data_out_c = weight_t_l(i)*data_in(index_t_l(i))
    END IF
  END IF

  ! t_r component

  IF (f_shum_is_denormal(data_in(index_t_r(i))) .OR.                           &
      f_shum_is_denormal(weight_t_r(i)) .OR.                                   &
      weight_t_r(i) < TINY(data_out_d) ) THEN
    data_out_d = 0.0
  ELSE
    IF (ABS(data_in(index_t_r(i))) < TINY(data_out_d) / weight_t_r(i)) THEN
      data_out_d = 0.0
    ELSE
      data_out_d = weight_t_r(i)*data_in(index_t_r(i))
    END IF
  END IF

  ! combined components

  data_out(i) = data_out_a + data_out_b + data_out_c + data_out_d

END DO
!$OMP END PARALLEL DO

RETURN

END SUBROUTINE f_shum_horizontal_field_bi_lin_interp_calc

!------------------------------------------------------------------------------!

!    SUBROUTINE f_shum_horizontal_field_bi-lin_interp_get_coeffs -----
!
!    Purpose:  Calculates bi-linear horizontal interpolation
!              coefficients and gather indices for interpolating
!              between generalised latitude-longitude grids (eg
!              global, regional or rotated lat-lon grid) in which the
!              gridlength may vary with latitude and/or longitude. The
!              interpolation is carried out by subroutine
!              f_shum_horizontal_field_bi_lin_interp_calc.
!              Gather indices point to bottom left hand corner and bottom
!              right hand corner of each grid box on source grid enclosing a
!              target point. Two indices are needed to cater for east-west
!              (lambda direction) cyclic boundaries when the source data is
!              global. If a target point falls outside the domain of the source
!              data, one sided differencing is used. The source latitude
!              coordinates must be supplied in decreasing order. The source
!              long- itude coordinates must be supplied in increasing order,
!              starting at any value, but not wrapping round. The target
!              points may be specified in any order.
!
!   Vector Machines : The original versions of this code had sections designed
!              for improved performance on vector machines controlled by
!              compiler defs. Please see UM code at 10.9 or early revisions of
!              the branch that introduced this code.

SUBROUTINE f_shum_horizontal_field_bi_lin_interp_get_coeffs                    &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic )

IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_l(points)
                                  ! Index of top left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_r(points)
                                  ! Index of top right corner
                                  ! of source gridbox

REAL(KIND=shum_real64), INTENT(IN)  :: lambda_targ(points)
                                ! Lambda coords of target grid in degrees
                                ! using same rotation as source grid
REAL(KIND=shum_real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=shum_real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

LOGICAL(KIND=shum_bool), INTENT(IN) :: cyclic ! =T, then source data is cyclic
                                              ! =F, then source data is
                                              !     non-cyclic

!--- Local variables:---------------------------------------------------
REAL(KIND=shum_real64)   :: t_lambda(points) ! Local value of target
                                                 ! longitude

INTEGER(KIND=shum_int64) :: ixp1(points)     ! Longitudinal index plus 1
INTEGER(KIND=shum_int64) :: ix(points)       ! Longitudinal index
INTEGER(KIND=shum_int64) :: iy(points)       ! Latitudinal index
! ----------------------------------------------------------------------

CALL f_shum_find_source_box_indices                                            &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic        &
                  , t_lambda, ixp1, ix, iy )

CALL f_shum_calc_weights                                                       &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, phi_targ                            &
                  , points_lambda_srce, points_phi_srce, points                &
                  , t_lambda, ixp1, ix, iy )

END SUBROUTINE f_shum_horizontal_field_bi_lin_interp_get_coeffs

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_find_source_box_indices                                      &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic        &
                  , t_lambda, ixp1, ix, iy )

IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_l(points)
                                  ! Index of top left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_r(points)
                                  ! Index of top right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: ixp1(points)
                                  ! Longitudinal index plus 1
INTEGER(KIND=shum_int64), INTENT(OUT) :: ix(points)
                                  ! Longitudinal index
INTEGER(KIND=shum_int64), INTENT(OUT) :: iy(points)
                                  ! Latitudinal index

REAL(KIND=shum_real64), INTENT(IN)  :: lambda_targ(points)
                                ! Lambda coords of target grid in degrees
                                ! using same rotation as source grid
REAL(KIND=shum_real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=shum_real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(OUT) :: t_lambda(points)
                                ! Local value of target longitude

LOGICAL(KIND=shum_bool), INTENT(IN) :: cyclic ! =T, then source data is cyclic
                                              ! =F, then source data is
                                              !     non-cyclic

!--- Local variables:---------------------------------------------------
INTEGER(KIND=shum_int64)  :: i      ! Loop index

! Variables for divide-and-conquer search of latitude/longitude arrays
INTEGER(KIND=shum_int64) :: iupr ! Uppermost-point
INTEGER(KIND=shum_int64) :: ilwr ! Lowermost-point
INTEGER(KIND=shum_int64) :: imid ! Mid-point
! ----------------------------------------------------------------------

! 1. Initialise arrays

! 1.1 Scale target longitude so that it falls between
!     lambda_srce(1) and lambda_srce(1) + 360
DO i=1, points
  t_lambda(i) = MOD(((lambda_targ(i)-lambda_srce(1))+720.0_shum_real64),       &
                      360.0_shum_real64) + lambda_srce(1)
END DO

IF (cyclic) THEN
  DO i=one64, points
    ix(i) = zero64
    iy(i) = one64
  END DO
ELSE
  DO i=one64, points
    ix(i) = one64
    iy(i) = one64
  END DO
END IF

!  2. Calculate lat and lon index of bottom left hand corner of
!     source grid box enclosing each target point.

! Longitude

!$OMP PARALLEL DO SCHEDULE(STATIC)                                             &
!$OMP  SHARED(points,points_lambda_srce,t_lambda,lambda_srce)                  &
!$OMP  SHARED(points_phi_srce,phi_srce,phi_targ,ix,iy)                         &
!$OMP  PRIVATE(i,iupr,ilwr,imid) DEFAULT(NONE)
DO i=one64, points

  ! Divide and conquer should more efficient than a brute-force loop over
  ! all i

  ilwr = zero64                    ! first point (-1 in case we are cyclic)
  iupr = points_lambda_srce + one64 ! last point  (+1 in case we are cyclic)
  DO WHILE ( (iupr-ilwr) > one64 )
    imid = (ilwr+iupr)/two64
    IF ( lambda_srce(imid) > t_lambda(i) ) THEN
      iupr = imid
    END IF
    IF ( lambda_srce(imid) <= t_lambda(i) ) THEN
      ilwr = imid
    END IF
  END DO

  ix(i) = ilwr

  ilwr = one64                !first point
  iupr = points_phi_srce      !last point
  DO WHILE ( (iupr-ilwr) > one64 )
    imid = (ilwr+iupr)/two64
    IF ( phi_srce(imid) > phi_targ(i) ) THEN
      iupr = imid
    END IF
    IF ( phi_srce(imid) <= phi_targ(i) ) THEN
      ilwr = imid
    END IF
  END DO

  iy(i) = ilwr

END DO
!$OMP END PARALLEL DO

! 3. Correct 1-D indices for wrap around etc and then calculate
!    2-D indices of bottom left and bottom right hand corner
!    of each grid box.

IF (cyclic) THEN
  ! 3.1 Cyclic case

  DO i=1, points

    ! Set index for cyclic wrap around (not sure this is ever met since
    ! t_lambda always runs from
    !      lambda_srce(1) -> lambda_srce(points_lambda_srce)
    ! but we shall keep it here in case).
    IF (ix(i) <  1) THEN
      ix(i) = points_lambda_srce
      t_lambda(i) = t_lambda(i) + 360.0_shum_real64
    END IF

    ! Set index for one sided difference if target point to north or
    ! south of source area.
    iy(i) = MAX(iy(i),one64)
    iy(i) = MIN(iy(i),points_phi_srce-one64)

    ! 2-D indices
    index_b_l(i) = ix(i)+(iy(i)-one64)*points_lambda_srce
    index_b_r(i) = index_b_l(i)+one64

    ! Correct for cyclic boundaries if target point outside source grid.
    ixp1(i) = ix(i) + one64
    IF (ix(i) == points_lambda_srce) THEN
      index_b_r(i) = index_b_r(i) - points_lambda_srce
      ixp1(i)      = ixp1(i)      - points_lambda_srce
    END IF

    index_t_l(i) = index_b_l(i) + points_lambda_srce
    index_t_r(i) = index_b_r(i) + points_lambda_srce
  END DO

ELSE

  ! 3.2 Non cyclic case
  DO i=1, points

    ! Check that the nearest source grid point is really the nearest.  Earlier
    ! we made sure t_lambda is between lambda_srce(1) and lambda_srce(1)+360.
    ! If we are a LAM the first boundary might be nearer so make sure t_lambda
    ! reflects this.
    ! Update 2017-12 : Move to SHUMLib : It's not clear to me this check and
    ! silent fix is required. This side of the Else indicates that the source
    ! grid is non-cyclic. The target grid already resides from  lambda_srce(1)
    ! to lambda_srce(1)+360 so to subtract 360 degrees from it would make it
    ! less than lambda_srce(1). I can't think of a legitimate case where this
    ! would be the case and it needs further investigating.
    IF (ix(i) == points_lambda_srce) THEN
      IF (ABS(t_lambda(i)-lambda_srce(1)-360.0_shum_real64) <                  &
          t_lambda(i)-lambda_srce(ix(i))) THEN
        t_lambda(i) = t_lambda(i) - 360.0_shum_real64
        ix(i) = 1
      END IF
    END IF

    ! Set index for one sided difference if outside source area
    ix(i) = MAX(ix(i),one64)
    ix(i) = MIN(ix(i),points_lambda_srce-one64)
    IF (ix(i) <  one64) THEN ! IX(I) < 1 if POINTS_LAMBDA_SRCE = 1
      ix(i) = one64
    END IF

    ixp1(i) = ix(i) + one64
    ixp1(i) = MIN(ixp1(i),points_lambda_srce)

    ! Set index for one sided difference if outside source area
    iy(i) = MAX(iy(i),one64)
    iy(i) = MIN(iy(i),points_phi_srce-one64)
    IF (iy(i) <  one64) THEN ! IY(I) < 1 if POINTS_PHI_SRCE = 1
      iy(i) = one64
    END IF

    ! 2-D indices
    index_b_l(i) = ix(i)   + (iy(i)-one64)*points_lambda_srce
    index_b_r(i) = ixp1(i) + (iy(i)-one64)*points_lambda_srce

    index_t_l(i) = index_b_l(i) + points_lambda_srce
    index_t_r(i) = index_b_r(i) + points_lambda_srce
  END DO

END IF

END SUBROUTINE f_shum_find_source_box_indices

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_calc_weights                                                 &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, phi_targ                            &
                  , points_lambda_srce, points_phi_srce, points                &
                  , t_lambda, ixp1, ix, iy )

IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: ixp1(points)
                                  ! Longitudinal index plus 1
INTEGER(KIND=shum_int64), INTENT(IN)  :: ix(points)
                                  ! Longitudinal index
INTEGER(KIND=shum_int64), INTENT(IN)  :: iy(points)
                                  ! Latitudinal index

REAL(KIND=shum_real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=shum_real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=shum_real64), INTENT(IN)  :: t_lambda(points)
                                ! Local value of target longitude
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

!--- Local variables:---------------------------------------------------
REAL(KIND=shum_real64)    :: a      ! Longitudinal weight
REAL(KIND=shum_real64)    :: b      ! Latitudinal weight
INTEGER(KIND=shum_int64)  :: i      ! Loop index
! ----------------------------------------------------------------------

!  1. Compute interpolation weights
DO i=1, points

  ! Calculate basic weights
  a = MOD(360.0_shum_real64 + lambda_srce(ixp1(i)) - lambda_srce(ix(i)),       &
          360.0_shum_real64)
  IF (a /= 0.0_shum_real64) THEN
    ! If t_lambda - lambda_source is negative then just copy last value across.
    a = (MAX(t_lambda(i)-lambda_srce(ix(i)),0.0_shum_real64))/a
  ELSE
    a = 0.0_shum_real64
  END IF

  ! If we only have 1 row then we need to make sure we can cope.
  b = ABS(phi_srce(iy(i))-phi_srce(MIN(iy(i)+1,points_phi_srce)))
  IF (b /= 0.0_shum_real64) THEN
    b = MAX(phi_targ(i)-phi_srce(iy(i)),0.0_shum_real64)/b
  ELSE
    b = 0.0_shum_real64
  END IF

  ! Calculate bi-linear interpolation weights
  weight_t_r(i) = a*b
  weight_b_l(i) = (1.0_shum_real64-a)*(1.0_shum_real64-b)
  weight_t_l(i) = (1.0_shum_real64-a)*b
  weight_b_r(i) = a*(1.0_shum_real64-b)

END DO

RETURN

END SUBROUTINE f_shum_calc_weights
!------------------------------------------------------------------------------!

!    SUBROUTINE f_shum_cart_horizontal_field_bi-lin_interp_get_coeffs -----
!
!    Purpose:  Calculates bi-linear horizontal interpolation
!              coefficients and gather indices for interpolating
!              between generalised cartesian grids (i.e. bicyclic, channel
!              or nowrap LAM) in which the x and y grids lengths are fixed.
!              The interpolation is carried out by subroutine
!              f_shum_horizontal_field_bi_lin_interp_calc.
!              Gather indices point to bottom left hand corner, bottom
!              right hand corner, top left hand corner and top right hand
!              corner of each grid box on source grid enclosing a
!              target point.
!              If a target point falls outside the domain of the source
!              data, one sided differencing is used. The source Y
!              coordinates must be supplied in decreasing order. The source
!              X coordinates must be supplied in increasing order,
!              starting at any value, but not wrapping round. The target
!              points may be specified in any order.
!
!   Vector Machines : The original versions of this code had sections designed
!              for improved performance on vector machines controlled by
!              compiler defs. Please see UM code at 10.9 or early revisions of
!              the branch that introduced this code.

SUBROUTINE f_shum_cart_horizontal_field_bi_lin_interp_get_coeffs               &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , x_srce, y_srce, x_targ, y_targ                             &
                  , points_x_srce, points_y_srce, points, grid_type , icode    &
                  , cmessage )

IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_x_srce
                                  ! Number of X on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_y_srce
                                  ! Number of Y points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: grid_type
                                  ! Source grid type
                                  ! 3 - LAM no wray around
                                  ! 4 - LAM bicylic wrap in X and Y
                                  !   - LAM channel wrap in X only
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_l(points)
                                  ! Index of top left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_r(points)
                                  ! Index of top right corner
                                  ! of source gridbox

REAL(KIND=shum_real64), INTENT(IN)  :: x_targ(points)
                                ! X coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_targ(points)
                                ! Y coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: x_srce(points_x_srce)
                                ! X coords of source grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_srce(points_y_srce)
                                ! Y coords of source grid in m
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

INTEGER(KIND=shum_int64), INTENT(OUT) :: icode    ! error code

CHARACTER(LEN=*), INTENT(OUT) :: cmessage  ! error message


!--- Local variables:---------------------------------------------------

INTEGER(KIND=shum_int64) :: ixp1(points)     ! X index plus 1
INTEGER(KIND=shum_int64) :: ix(points)       ! X index
INTEGER(KIND=shum_int64) :: iyp1(points)     ! Y index plus 1
INTEGER(KIND=shum_int64) :: iy(points)       ! Y index
! ----------------------------------------------------------------------

CALL f_shum_find_source_cart_box_indices                                       &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , x_srce, y_srce, x_targ, y_targ                             &
                  , points_x_srce, points_y_srce, points, grid_type            &
                  , ixp1, ix, iyp1,iy, icode, cmessage )

IF (icode > 0) THEN  ! Problem error detected in find_source_cart_box_indices
  RETURN
END IF

CALL f_shum_calc_cart_weights                                                  &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , x_srce, y_srce, x_targ, y_targ                             &
                  , points_x_srce, points_y_srce, points                       &
                  , ixp1, ix, iyp1, iy )

END SUBROUTINE f_shum_cart_horizontal_field_bi_lin_interp_get_coeffs

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_find_source_cart_box_indices                                 &
                  ( index_b_l, index_b_r, index_t_l, index_t_r                 &
                  , x_srce, y_srce, x_targ, y_targ                             &
                  , points_x_srce, points_y_srce, points, grid_type            &
                  , ixp1, ix, iyp1,iy, icode, cmessage )

IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_x_srce
                                  ! Number of x points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_y_srce
                                  ! Number of y points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: grid_type
                                  ! Source grid type
                                  ! 3 - LAM no wray around
                                  ! 4 - LAM bicylic wrap in X and Y
                                  !   - LAM channel wrap in X only
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_l(points)
                                  ! Index of top left corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: index_t_r(points)
                                  ! Index of top right corner
                                  ! of source gridbox
INTEGER(KIND=shum_int64), INTENT(OUT) :: ixp1(points)
                                  ! X index plus 1
INTEGER(KIND=shum_int64), INTENT(OUT) :: ix(points)
                                  ! X index
INTEGER(KIND=shum_int64), INTENT(OUT) :: iyp1(points)
                                  ! Y index plus 1
INTEGER(KIND=shum_int64), INTENT(OUT) :: iy(points)
                                  ! Y index
INTEGER(KIND=shum_int64), INTENT(OUT) :: icode     ! error code

CHARACTER(LEN=*), INTENT(OUT) :: cmessage  ! error message


REAL(KIND=shum_real64), INTENT(IN)  :: x_targ(points)
                                ! x coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_targ(points)
                                ! y coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: x_srce(points_x_srce)
                                ! x coords of source grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_srce(points_y_srce)
                                ! y coords of source grid in m

!--- Local variables:---------------------------------------------------
INTEGER(KIND=shum_int64)  :: i      ! Loop index
INTEGER(KIND=shum_int64)  :: j      ! Loop index

! Grid types
INTEGER(KIND=shum_int64), PARAMETER  :: bicylic = 4
INTEGER(KIND=shum_int64), PARAMETER  :: channel = 3
INTEGER(KIND=shum_int64), PARAMETER  :: nowrap  = 2
! ----------------------------------------------------------------------

! Set error code to zero
icode = zero64

! 1. Initialise arrays
IF (grid_type == bicylic) THEN

  DO i = one64, points
    DO j = one64, points_x_srce-one64
      IF ( x_targ(i) >= x_srce(j) .AND. x_targ(i) < x_srce(j+one64) ) THEN
        ix(i)   = j
        ixp1(i) = j+one64
      END IF
    END DO
    ! Wrap around grid
    ! Note as at present all Cartesian grids start at x= 0.0 but the first
    ! P grid point will be 0.5 gridbox width offset from zero.
    ! The locations of the source and target grids covering the same area
    ! all have values between 0.0 and grid max dimension which is the number
    ! of rows or columns multiplied by the grid length.
    !
    !   x - >  1           2           3          4      End grid
    !    |  A  X           X           X          X  B   |
    !   0.0                                             max_x_srce
    !
    !  Test for points in locations A or B
    IF ( x_targ(i) < x_srce(one64) .OR.                                        &
                                  x_targ(i) >= x_srce(points_x_srce) ) THEN
      ix(i)   = points_x_srce
      ixp1(i) = one64
    END IF
    DO j = one64, points_y_srce-one64
      IF ( y_targ(i) >= y_srce(j) .AND. y_targ(i) < y_srce(j+1) ) THEN
        iy(i)   = j
        iyp1(i) = j+one64
      END IF
    END DO
    ! Wrap around  tests as for X
    IF ( y_targ(i) < y_srce(one64) .OR.                                        &
                                  y_targ(i) >= y_srce(points_y_srce) ) THEN
      iy(i)   = points_y_srce
      iyp1(i) = one64
    END IF
  END DO

ELSE IF (grid_type == nowrap) THEN

  DO i = one64, points
    DO j = one64, points_x_srce-one64
      IF ( x_targ(i) >= x_srce(j) .AND. x_targ(i) < x_srce(j+one64) ) THEN
        ix(i)   = j
        ixp1(i) = j+one64
      END IF
    END DO
    IF ( x_targ(i) < x_srce(one64) ) THEN
      ix(i)   = one64
      ixp1(i) = one64
    END IF
    IF ( x_targ(i) >= x_srce(points_x_srce) ) THEN
      ix(i)   = points_x_srce
      ixp1(i) = points_x_srce
    END IF
    DO j = one64, points_y_srce-1
      IF ( y_targ(i) >= y_srce(j) .AND. y_targ(i) < y_srce(j+one64) ) THEN
        iy(i)   = j
        iyp1(i) = j+one64
      END IF
    END DO
    IF ( y_targ(i) < y_srce(one64) ) THEN
      iy(i)   = one64
      iyp1(i) = one64
    END IF
    IF ( y_targ(i) >= y_srce(points_y_srce) ) THEN
      iy(i)   = points_y_srce
      iyp1(i) = points_y_srce
    END IF
  END DO

ELSE IF (grid_type == channel) THEN ! wrap in x direction only

  DO i = one64, points
    DO j = one64, points_x_srce-one64
      IF ( x_targ(i) >= x_srce(j) .AND. x_targ(i) < x_srce(j+one64) ) THEN
        ix(i)   = j
        ixp1(i) = j+one64
      END IF
    END DO
    ! Wrap around tests as for bicyclic case
    IF ( x_targ(i) < x_srce(1) .OR. x_targ(i) >= x_srce(points_x_srce) ) THEN

      ix(i)   = points_x_srce
      ixp1(i) = one64
    END IF
    ! No wrap around
    DO j = one64, points_y_srce-1
      IF ( y_targ(i) >= y_srce(j) .AND. y_targ(i) < y_srce(j+one64) ) THEN
        iy(i)   = j
        iyp1(i) = j+one64
      END IF
    END DO
    IF (y_targ(i) < y_srce(one64) ) THEN
      iy(i)   = one64
      iyp1(i) = one64
    END IF
    IF (y_targ(i) >= y_srce(points_y_srce) ) THEN
      iy(i)   = points_y_srce
      iyp1(i) = points_y_srce
    END IF
  END DO

ELSE     ! unrecognised grid type, should never reach here

  icode = 10
  WRITE(cmessage,'(A,I0)') 'Unrecognised grid type ',grid_type
  RETURN

END IF   ! test on input grid type

!---------------------------------------------------------------------------
! 2. From ix and iy work out four surrounding grid point locations in full
!    grid. Assumes input and target grids are cartesian, and have regular
!    grid spacing in X and Y.
!---------------------------------------------------------------------------

! location   in 1d array is  (row-1)* row length + column number
! Left hand bottom   corner coordinates ix,   iy
! Left hand top      corner coordinates ix,   iyp1
! Right hand bottom  corner coordinates ixp1, iy
! Right hand top     corner coordinates ixp1, iyp1

DO i = one64, points
  index_b_l(i) = ix(i)   + (iy(i)-one64)*points_x_srce
  index_b_r(i) = ixp1(i) + (iy(i)-one64)*points_x_srce
  index_t_l(i) = ix(i)   + (iyp1(i)-one64)*points_x_srce
  index_t_r(i) = ixp1(i) + (iyp1(i)-one64)*points_x_srce
END DO


END SUBROUTINE f_shum_find_source_cart_box_indices

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_calc_cart_weights                                            &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , x_srce, y_srce, x_targ, y_targ                             &
                  , points_x_srce, points_y_srce, points                       &
                  , ixp1, ix, iyp1,iy )


IMPLICIT NONE

INTEGER(KIND=shum_int64), INTENT(IN)  :: points_x_srce
                                  ! Number of x points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points_y_srce
                                  ! Number of y points on source grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=shum_int64), INTENT(IN)  :: ixp1(points)
                                  ! X index plus 1
INTEGER(KIND=shum_int64), INTENT(IN)  :: ix(points)
                                  ! X index
INTEGER(KIND=shum_int64), INTENT(IN)  :: iyp1(points)
                                  ! Y index plus 1
INTEGER(KIND=shum_int64), INTENT(IN)  :: iy(points)
                                  ! Y index

REAL(KIND=shum_real64), INTENT(IN)  :: x_targ(points)
                                ! X coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_targ(points)
                                ! y coords of target grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: x_srce(points_x_srce)
                                ! x coords of source grid in m
REAL(KIND=shum_real64), INTENT(IN)  :: y_srce(points_y_srce)
                                ! y coords of source grid in m

REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=shum_real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

!--- Local variables:---------------------------------------------------
REAL(KIND=shum_real64)    :: a      ! x weight
REAL(KIND=shum_real64)    :: b      ! y weight
REAL(KIND=shum_real64)    :: dx_srce  ! x grid length on source grid (m)
REAL(KIND=shum_real64)    :: dy_srce  ! y grid length on source grid (m)
REAL(KIND=shum_real64)    :: max_x_srce  ! x length of source grid (m)
REAL(KIND=shum_real64)    :: max_y_srce  ! y length of source grid (m)
INTEGER(KIND=shum_int64)  :: i      ! Loop index

! ----------------------------------------------------------------------
! Cartesian grid spacing assumed regular
dx_srce = x_srce(two64) - x_srce(one64)
dy_srce = y_srce(two64) - y_srce(one64)

! Required for wrap around cases
max_x_srce = points_x_srce*dx_srce
max_y_srce = points_y_srce*dy_srce

!  1. Compute interpolation weights
DO i=one64, points

  ! Calculate basic weights need different ix values
  IF (ixp1(i) > ix(i)) THEN
    a = (MAX(x_targ(i)-x_srce(ix(i)),0.0_shum_real64))/dx_srce
  ELSE IF (ixp1(i) < ix(i)) THEN ! Wrap around case
    ! Note first grid point X is not always 0.0 depends on whether U, V or P
    ! grid so target grid value can be > final source_grid_y  but less than
    ! grid max so orignal code ok  (poistion B) or in position A
    !
    !   x - >  1           2           3          4      End grid
    !    |  A  X           X           X          X  B   |
    !   0.0                                             max_x_srce
    ! Case of wrap around with a finer grid, X coarse source grid point centre
    ! The new point in the wrap around region can be in position A or B

    IF ( x_targ(i) < x_srce(ix(i))) THEN   ! target in position A
      a = (MAX(x_targ(i)-(x_srce(ix(i))-max_x_srce),0.0_shum_real64))/dx_srce
    ELSE   ! Target in position B
      a = (MAX(x_targ(i)-x_srce(ix(i)),0.0_shum_real64))/dx_srce
    END IF
  ELSE   ! values equal so zero
    a = 0.0_shum_real64
  END IF

  ! Calculate basic weights need different iy values
  IF (iyp1(i) > iy(i)) THEN
    b = (MAX(y_targ(i)-y_srce(iy(i)),0.0_shum_real64))/dy_srce
  ELSE IF (iyp1(i) < iy(i)) THEN ! Wrap around case
    ! Same problems in Y direction as X direction.
    IF ( y_targ(i) < y_srce(iy(i))) THEN   ! Like position A
      b = (MAX(y_targ(i)-(y_srce(iy(i))-max_y_srce),0.0_shum_real64))/dy_srce
    ELSE        ! like position B
      b = (MAX(y_targ(i)-y_srce(iy(i)),0.0_shum_real64))/dy_srce
    END IF
  ELSE  ! Corners the same so zero
    b = 0.0_shum_real64
  END IF

  ! Calculate bi-linear interpolation weights
  weight_t_r(i) = a*b
  weight_b_l(i) = (1.0_shum_real64-a)*(1.0_shum_real64-b)
  weight_t_l(i) = (1.0_shum_real64-a)*b
  weight_b_r(i) = a*(1.0_shum_real64-b)

END DO

RETURN

END SUBROUTINE f_shum_calc_cart_weights

END MODULE f_shum_horizontal_field_interp_mod
