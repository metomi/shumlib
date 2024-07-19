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
! Description: Function to resolve a set of 'unresolved' grid-points
!              using an independent unbiased spiral search algorithm
!              with optional range constraint.
!
MODULE f_shum_spiral_search_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

USE f_shum_conversions_mod, ONLY:                                              &
                                 shum_pi_const,                                &
                                 shum_pi_over_180_const,                       &
                                 shum_pi_const_32,                             &
                                 shum_pi_over_180_const_32

IMPLICIT NONE

PRIVATE

PUBLIC :: f_shum_spiral_search_algorithm

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
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
!------------------------------------------------------------------------------!

REAL(KIND=REAL64), PARAMETER :: rMDI      = -32768.0_real64*32768.0_real64
REAL(KIND=REAL32), PARAMETER :: rMDI_32b  = -32768.0_real32*32768.0_real32

INTERFACE f_shum_spiral_search_algorithm
  MODULE PROCEDURE                                                             &
      f_shum_spiral_arg32, f_shum_spiral_arg64
END INTERFACE

INTERFACE calc_distance
  MODULE PROCEDURE                                                             &
      calc_distance_arg32, calc_distance_arg64
END INTERFACE

CONTAINS

! Description:
! Calculate the values for unresolved points.
! Uses a spiral method, which finds the closest point
! by distance (in m)
!
! Method:
! Uses the Haversine formula to calculate the distances.
! Searches in steps of 3*minimum local distance for each
! unresolved point until it finds a resolved point.  For land
! points in a field that is not "land only" a distance constraint
! is applied, i.e. if there isn't a resolved land point within
! this distance it uses the closest resolved sea point value
! instead.
! For Global domains (i.e. cyclic) if hit an edge it does
! calculate the distances of every point in the domain as it can't
! cope with looping over the edges.  This will cause the scheme to
! take much longer to run, however this is unlikely to happen
! often.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Reconfiguration
!

FUNCTION f_shum_spiral_arg64                                                   &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius,                                            &
            cmessage) RESULT(istat)

IMPLICIT NONE

                                          ! Number of rows in grid
INTEGER(KIND=INT64), INTENT(IN)    :: points_phi
                                          ! Number of columns in grid
INTEGER(KIND=INT64), INTENT(IN)    :: points_lambda
                                          ! Land sea mask
LOGICAL(KIND=bool),  INTENT(IN)    :: lsm(points_lambda*points_phi)
                                          ! Number of unresolved points
INTEGER(KIND=INT64), INTENT(IN)    :: no_point_unres
                                          ! Index to unresolved pts
INTEGER(KIND=INT64), INTENT(IN)    :: index_unres(no_point_unres)
                                          ! Latitudes
REAL(KIND=REAL64),   INTENT(IN)    :: lats(points_phi)
                                          ! Longitudes
REAL(KIND=REAL64),   INTENT(IN)    :: lons(points_lambda)
                                          ! False for sea, True for land field
LOGICAL(KIND=bool),  INTENT(IN)    :: is_land_field
                                          ! True to apply constraint distance
LOGICAL(KIND=bool),  INTENT(IN)    :: constrained
                                          ! Contraint distance (m)
REAL(KIND=REAL64),   INTENT(IN)    :: constrained_max_dist
                                          ! Step size modifier for search
REAL(KIND=REAL64),   INTENT(IN)    :: dist_step
                                          ! True if covering complete lat circle
LOGICAL(KIND=bool),  INTENT(IN)    :: cyclic_domain
                                          ! False for a point that is resolved,
                                          ! True for an unresolved point
LOGICAL(KIND=bool),  INTENT(IN)    :: unres_mask(points_lambda*points_phi)
                                          ! Indices to resolved pts
INTEGER(KIND=INT64), INTENT(OUT)   :: indices(no_point_unres)
                                          ! Radius of planet (in m)
REAL(KIND=REAL64),   INTENT(IN)    :: planet_radius
                                          ! Error message
CHARACTER(LEN=*),    INTENT(IN OUT) :: cmessage
                                          ! Return status
INTEGER(KIND=INT64)                :: istat

! LOCAL VARIABLES

INTEGER(KIND=INT64) :: i,j,k,l ! Loop counters
INTEGER(KIND=INT64) :: north ! Number of points to search north
INTEGER(KIND=INT64) :: south ! Number of points to search south
INTEGER(KIND=INT64) :: east  ! Number of points to search east
INTEGER(KIND=INT64) :: west  ! Number of points to search west
INTEGER(KIND=INT64) :: unres_i ! Index for i for unres
INTEGER(KIND=INT64) :: unres_j ! Index for j for unres
INTEGER(KIND=INT64) :: curr_i_valid_min ! Posn of valid min distance in E-W dir
INTEGER(KIND=INT64) :: curr_j_valid_min ! Posn of valid min distance in S-N dir
INTEGER(KIND=INT64) :: curr_i_invalid_min ! Posn of invalid min dist in E-W dir
INTEGER(KIND=INT64) :: curr_j_invalid_min ! Posn of invalid min dist in S-N dir
LOGICAL :: found ! resolved point fitting critirea has been found
LOGICAL :: allsametype ! no resolved points of same type in lsm
LOGICAL(KIND=bool) :: tmp_lsm(points_lambda*points_phi) ! temp land sea mask
REAL(KIND=REAL64) :: search_dist ! distance to search
REAL(KIND=REAL64) :: step ! step size to loop over
REAL(KIND=REAL64) :: tempdist ! temporary distance
REAL(KIND=REAL64) :: min_loc_spc ! minimum of the local spacings
REAL(KIND=REAL64) :: max_dist ! maximum distance to search
REAL(KIND=REAL64) :: distance(points_lambda) ! Vector of calculation.
REAL(KIND=REAL64) :: curr_dist_valid_min   ! Current distance to valid point
REAL(KIND=REAL64) :: curr_dist_invalid_min ! Current distance to invalid point
LOGICAL(KIND=bool) :: is_the_same(SIZE(tmp_lsm,1))

! End of header

! Initialise
istat = 0
indices(:) = -1
cmessage = ' '
IF (constrained) THEN
  ! use supplied maximum distance to search
  max_dist=constrained_max_dist
ELSE
  ! reset the max distance to half the circumference of earth
  max_dist=planet_radius*shum_pi_const
END IF

! Test to see if all resolved points are of the opposite type
tmp_lsm=lsm

tmp_lsm(index_unres(1:no_point_unres))= .NOT. is_land_field
WHERE (tmp_lsm .EQV. is_land_field)
  is_the_same = .TRUE.
ELSE WHERE
  is_the_same = .FALSE.
END WHERE

IF (ANY (is_the_same) ) THEN
  allsametype=.FALSE.
ELSE
  allsametype=.TRUE.
END IF

! Loop over every unresolved point.
DO k=1, no_point_unres
  ! Find i/j index for point.
  unres_j  = (index_unres(k)-1_int64)/points_lambda + 1_int64
  unres_i  = index_unres(k)-(unres_j-1_int64)*points_lambda

  ! Calculate minimum local grid spacing

  min_loc_spc=99999999_int64

  IF (unres_j > 1) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j-1_int64),       &
                    lons(unres_i), planet_radius))
  END IF
  IF (unres_j < points_phi) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j+1_int64),       &
                    lons(unres_i), planet_radius))
  END IF
  IF (unres_i > 1) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j),               &
                    lons(unres_i-1_int64), planet_radius))
  END IF
  IF (unres_i < points_lambda) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j),               &
                    lons(unres_i+1_int64), planet_radius))
  END IF

  ! Do in steps dist_step*minimum local grid spacing

  step=dist_step*min_loc_spc
  search_dist=step
  IF (search_dist > max_dist) THEN
    search_dist = max_dist
  END IF

  ! Assume if we don't find a valid min the value is at planet_radius*pi+1.
  curr_dist_valid_min = (planet_radius*shum_pi_const)+1.0_real64
  ! We want nearest one so we dont want to limit search to max_dist.
  curr_dist_invalid_min = rMDI

  found=.FALSE.

  ! Loop in steps of dist_step*minimum local grid spacing until max
  ! search trying to find a valid point

  curr_i_valid_min= 0_int64
  curr_j_valid_min= 0_int64
  curr_i_invalid_min= 0_int64
  curr_j_invalid_min= 0_int64

  DO WHILE (search_dist <= max_dist .AND. .NOT. found)

    south=-1_int64
    north=-1_int64
    west=-1_int64
    east=-1_int64

    ! Find how many points can go south and be inside search_dist
    DO j = 1, unres_j-1
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j-j),lons(unres_i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        south=j-1_int64
        EXIT
      END IF
    END DO
    ! Find how many points can go north and be inside search_dist
    DO j = 1, points_phi-unres_j
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j+j),lons(unres_i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        north=j-1_int64
        EXIT
      END IF
    END DO
    ! Find how many points can go west and be inside search_dist
    DO i = 1, unres_i-1
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j),lons(unres_i-i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        west=i-1_int64
        EXIT
      END IF
    END DO
    ! Find how many points can go east and be inside search_dist
    DO i = 1, points_lambda-unres_i
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j),lons(unres_i+i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        east=i-1_int64
        EXIT
      END IF
    END DO
    ! If LAM can't go past a boundary
    IF (.NOT. cyclic_domain) THEN
      IF ( south == -1 ) THEN
        south=unres_j-1_int64
      END IF
      IF ( north == -1 ) THEN
        north=points_phi-unres_j
      END IF
      IF ( west == -1 ) THEN
        west=unres_i-1_int64
      END IF
      IF ( east == -1 ) THEN
        east=points_lambda-unres_i
      END IF
    END IF

    IF (south == -1_int64 .OR. north == -1_int64 .OR.                          &
         west == -1_int64 .OR.  east == -1_int64) THEN
      ! have hit an edge of a cyclic domain so will have to do the whole domain
      ! Want to avoid doing the whole domain as much as possible
      DO j = 1, points_phi
        DO i = 1, points_lambda
          l = i+(j - 1_int64)*points_lambda
          ! Check to see if it is a resolved point
          IF (.NOT. unres_mask(l)) THEN
            ! Calculate distance from point
            distance(i) = calc_distance(lats(unres_j), lons(unres_i),          &
                                          lats(j), lons(i), planet_radius)
          END IF
        END DO

        DO i = 1, points_lambda
          l = i+(j - 1_int64)*points_lambda
          ! Check to see if it is a resolved point
          IF (.NOT. unres_mask(l)) THEN
            ! Same type of point.
            IF (lsm(l) .EQV. is_land_field) THEN
              ! If current distance is less than any previous min store it.
              IF (distance(i) < curr_dist_valid_min) THEN
                curr_dist_valid_min = distance(i)
                curr_i_valid_min    = i
                curr_j_valid_min    = j
              END IF
            ELSE IF (constrained .OR. allsametype) THEN
              IF (distance(i) < curr_dist_invalid_min .OR.                     &
                curr_dist_invalid_min == rMDI) THEN
                curr_dist_invalid_min = distance(i)
                curr_i_invalid_min=i
                curr_j_invalid_min=j
              END IF
            END IF
          END IF
        END DO
      END DO

      ! Set the unresolved data to something sensible if possible.  Need to keep this
      ! independent due to we dont want it to interact with future searches.
      IF (curr_dist_valid_min <= max_dist) THEN
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int64)*points_lambda
      ELSE IF (allsametype .AND.                                               &
               curr_dist_invalid_min /= rMDI .AND.                             &
               curr_dist_invalid_min > max_dist) THEN
        cmessage = 'There are no resolved points of this type, setting '       &
        // 'to closest resolved point'
        istat = -5
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int64)*points_lambda
      ELSE IF (curr_dist_invalid_min /= rMDI .AND.                             &
               curr_dist_invalid_min <= max_dist) THEN
        indices(k) = curr_i_invalid_min+(curr_j_invalid_min - 1_int64)*points_lambda
      ELSE IF (curr_dist_invalid_min /= rMDI .AND.                             &
               curr_dist_invalid_min > max_dist) THEN
        ! Though hit the maximum search distance there has not been a
        ! resolved point of either type found, therefore use the closest
        ! resolved point of the same type which will be greater than max_dist
        cmessage = 'Despite being constrained there were no resolved points '  &
        // 'of any type within the limit, will just use closest resolved '     &
        // 'point of the same type'
        istat = -10
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int64)*points_lambda
      ELSE
        cmessage = 'A point has been left as still unresolved'
        istat = 47
      END IF
      found=.TRUE.

    ELSE ! s,n,w,e /= -1 i.e. not hit edge of a cyclic domain

      IF (south+north /= 0 .OR. east+west /= 0) THEN
        DO j = unres_j-south, unres_j+north
          DO i = unres_i-west, unres_i+east
            l = i+(j - 1_int64)*points_lambda
            ! Check to see if it is a resolved point
            IF (.NOT. unres_mask(l)) THEN
              ! Calculate distance from point
              distance(i) = calc_distance(lats(unres_j),lons(unres_i),         &
                                            lats(j),lons(i), planet_radius)
            END IF
          END DO

          DO i = unres_i-west, unres_i+east
            l = i+(j - 1_int64)*points_lambda
            ! Check to see if it is a resolved point
            IF (.NOT. unres_mask(l)) THEN
              ! Same type of point and distance less than maximum distance.
              IF ((lsm(l) .EQV. is_land_field) .AND.                           &
                       (distance(i) < search_dist)) THEN
                ! If current distance is less than any previous min store it.
                IF (distance(i) < curr_dist_valid_min) THEN
                  curr_dist_valid_min = distance(i)
                  curr_i_valid_min    = i
                  curr_j_valid_min    = j
                END IF
              ELSE IF (constrained .OR. allsametype) THEN
                ! have to add this in as if it isn't constrained don't want it
                ! storing an invalid value
                IF (distance(i) < curr_dist_invalid_min .OR.                   &
                  curr_dist_invalid_min == rMDI) THEN
                  curr_dist_invalid_min = distance(i)
                  curr_i_invalid_min=i
                  curr_j_invalid_min=j
                END IF
              END IF
            END IF
          END DO
        END DO

        ! Set unresolved data to something sensible if possible.  Need to keep this
        ! independent as we don't want it to interact with future searches.
        IF (curr_dist_valid_min < search_dist) THEN
          indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int64)*points_lambda
          found=.TRUE.
        END IF
        IF (allsametype .AND. curr_dist_invalid_min < search_dist) THEN
          cmessage = 'There are no resolved points of this type, setting '     &
          // 'to closest resolved point'
          istat = -5
          indices(k)=curr_i_invalid_min+(curr_j_invalid_min - 1_int64)*points_lambda
          found=.TRUE.
        END IF
        IF (.NOT. found) THEN
          IF (search_dist+step <= max_dist) THEN
            search_dist=search_dist+step
          ELSE IF (search_dist < max_dist) THEN
            search_dist=max_dist
          ELSE IF (constrained .AND.                                           &
                   curr_dist_invalid_min == rMDI) THEN
            ! Though hit the maximum search distance there has not been a
            ! resolved point of either type found, therefore increase
            ! max_dist by step
            cmessage = 'Despite being constrained there were no resolved '     &
            // 'points of any type within the limit, will just use closest '   &
            // 'resolved point of the same type'
            istat = -20
            max_dist=planet_radius*shum_pi_const
            curr_dist_valid_min=max_dist
            search_dist=search_dist+step
          ELSE
            search_dist=max_dist+1.0_real64
          END IF
        END IF
      ELSE
        IF (search_dist+step <= max_dist) THEN
          search_dist=search_dist+step
        ELSE IF (search_dist < max_dist) THEN
          search_dist=max_dist
        ELSE IF (constrained .AND.                                             &
                 curr_dist_invalid_min == rMDI) THEN
          ! Though hit the maximum search distance there has not been a
          ! resolved point of either type found, therefore increase
          ! max_dist by step
          cmessage = 'Despite being constrained there were no resolved '       &
          // 'points of any type within the limit, will just use closest '     &
          // 'resolved point of the same type'
          istat = -30
          max_dist=planet_radius*shum_pi_const
          curr_dist_valid_min=max_dist
          search_dist=search_dist+step
        ELSE
          search_dist=max_dist+1
        END IF
      END IF
    END IF
  END DO ! while
  IF (.NOT. found) THEN
    IF (curr_dist_invalid_min /= rMDI) THEN
      indices(k) = curr_i_invalid_min+(curr_j_invalid_min - 1)*points_lambda
    ELSE
      cmessage = 'A point has been left as still unresolved'
      istat = 47
    END IF
  END IF
END DO

END FUNCTION f_shum_spiral_arg64

! Repeat the whole function at 32-bit for comparison.
FUNCTION f_shum_spiral_arg32                                                   &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius,                                            &
            cmessage) RESULT(istat)

IMPLICIT NONE

                                          ! Number of rows in grid
INTEGER(KIND=INT32), INTENT(IN)    :: points_phi
                                          ! Number of columns in grid
INTEGER(KIND=INT32), INTENT(IN)    :: points_lambda
                                          ! Land sea mask
LOGICAL(KIND=bool),  INTENT(IN)    :: lsm(points_lambda*points_phi)
                                          ! Number of unresolved points
INTEGER(KIND=INT32), INTENT(IN)    :: no_point_unres
                                          ! Index to unresolved pts
INTEGER(KIND=INT32), INTENT(IN)    :: index_unres(no_point_unres)
                                          ! Latitudes
REAL(KIND=REAL32),   INTENT(IN)    :: lats(points_phi)
                                          ! Longitudes
REAL(KIND=REAL32),   INTENT(IN)    :: lons(points_lambda)
                                          ! False for sea, True for land field
LOGICAL(KIND=bool),  INTENT(IN)    :: is_land_field
                                          ! True to apply constraint distance
LOGICAL(KIND=bool),  INTENT(IN)    :: constrained
                                          ! Contraint distance (m)
REAL(KIND=REAL32),   INTENT(IN)    :: constrained_max_dist
                                          ! Step size modifier for search
REAL(KIND=REAL32),   INTENT(IN)    :: dist_step
                                          ! True if covering complete lat circle
LOGICAL(KIND=bool),  INTENT(IN)    :: cyclic_domain
                                          ! False for a point that is resolved,
                                          ! True for an unresolved point
LOGICAL(KIND=bool),  INTENT(IN)    :: unres_mask(points_lambda*points_phi)
                                          ! Index to resolved pts
INTEGER(KIND=INT32), INTENT(OUT)   :: indices(no_point_unres)
                                          ! Radius of planet
REAL(KIND=REAL32),   INTENT(IN)    :: planet_radius
                                          ! Error message
CHARACTER(LEN=*),    INTENT(IN OUT) :: cmessage
                                          ! Return status
INTEGER(KIND=INT32)                :: istat

! LOCAL VARIABLES

INTEGER(KIND=INT32) :: i,j,k,l ! Loop counters
INTEGER(KIND=INT32) :: north ! Number of points to search north
INTEGER(KIND=INT32) :: south ! Number of points to search south
INTEGER(KIND=INT32) :: east  ! Number of points to search east
INTEGER(KIND=INT32) :: west  ! Number of points to search west
INTEGER(KIND=INT32) :: unres_i ! Index for i for unres
INTEGER(KIND=INT32) :: unres_j ! Index for j for unres
INTEGER(KIND=INT32) :: curr_i_valid_min ! Posn of valid min distance in E-W dir
INTEGER(KIND=INT32) :: curr_j_valid_min ! Posn of valid min distance in S-N dir
INTEGER(KIND=INT32) :: curr_i_invalid_min ! Posn of invalid min dist in E-W dir
INTEGER(KIND=INT32) :: curr_j_invalid_min ! Posn of invalid min dist in S-N dir
LOGICAL :: found ! resolved point fitting critirea has been found
LOGICAL :: allsametype ! no resolved points of same type in lsm
LOGICAL(KIND=bool) :: tmp_lsm(points_lambda*points_phi) ! temp land sea mask
REAL(KIND=REAL32) :: search_dist ! distance to search
REAL(KIND=REAL32) :: step ! step size to loop over
REAL(KIND=REAL32) :: tempdist ! temporary distance
REAL(KIND=REAL32) :: min_loc_spc ! minimum of the local spacings
REAL(KIND=REAL32) :: max_dist ! maximum distance to search
REAL(KIND=REAL32) :: distance(points_lambda) ! Vector of calculation.
REAL(KIND=REAL32) :: curr_dist_valid_min   ! Current distance to valid point
REAL(KIND=REAL32) :: curr_dist_invalid_min ! Current distance to invalid point
LOGICAL(KIND=bool) :: is_the_same(SIZE(tmp_lsm,1))

! End of header

! Initialise
istat = 0
indices(:) = -1
cmessage = ' '
IF (constrained) THEN
  ! use supplied maximum distance to search
  max_dist=constrained_max_dist
ELSE
  ! reset the max distance to half the circumference of earth
  max_dist=planet_radius*shum_pi_const_32
END IF

! Test to see if all resolved points are of the opposite type
tmp_lsm=lsm

tmp_lsm(index_unres)= .NOT. is_land_field
WHERE (tmp_lsm .EQV. is_land_field)
  is_the_same = .TRUE.
ELSE WHERE
  is_the_same = .FALSE.
END WHERE

IF (ANY (is_the_same) ) THEN
  allsametype=.FALSE.
ELSE
  allsametype=.TRUE.
END IF

! Loop over every unresolved point.
DO k=1, no_point_unres
  ! Find i/j index for point.
  unres_j  = (index_unres(k)-1_int32)/points_lambda + 1_int32
  unres_i  = index_unres(k)-(unres_j-1_int32)*points_lambda

  ! Calculate minimum local grid spacing

  min_loc_spc=99999999.0_real32

  IF (unres_j > 1) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j-1_int32),       &
                    lons(unres_i), planet_radius))
  END IF
  IF (unres_j < points_phi) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j+1_int32),       &
                    lons(unres_i), planet_radius))
  END IF
  IF (unres_i > 1) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j),               &
                    lons(unres_i-1_int32), planet_radius))
  END IF
  IF (unres_i < points_lambda) THEN
    min_loc_spc=MIN(min_loc_spc,                                               &
      calc_distance(lats(unres_j), lons(unres_i), lats(unres_j),               &
                    lons(unres_i+1_int32), planet_radius))
  END IF

  ! Do in steps dist_step*minimum local grid spacing

  step=dist_step*min_loc_spc
  search_dist=step
  IF (search_dist > max_dist) THEN
    search_dist = max_dist
  END IF

  ! Assume if we don't find a valid min the value is at planet_radius*pi+1.
  curr_dist_valid_min = (planet_radius*shum_pi_const_32)+1.0_real32
  ! We want nearest one so we dont want to limit search to max_dist.
  curr_dist_invalid_min = rMDI_32b

  found=.FALSE.

  ! Loop in steps of dist_step*minimum local grid spacing until max
  ! search trying to find a valid point

  curr_i_valid_min= 0_int32
  curr_j_valid_min= 0_int32
  curr_i_invalid_min= 0_int32
  curr_j_invalid_min= 0_int32

  DO WHILE (search_dist <= max_dist .AND. .NOT. found)

    south=-1_int32
    north=-1_int32
    west=-1_int32
    east=-1_int32

    ! Find how many points can go south and be inside search_dist
    DO j = 1, unres_j-1_int32
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j-j),lons(unres_i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        south=j-1_int32
        EXIT
      END IF
    END DO
    ! Find how many points can go north and be inside search_dist
    DO j = 1, points_phi-unres_j
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j+j),lons(unres_i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        north=j-1_int32
        EXIT
      END IF
    END DO
    ! Find how many points can go west and be inside search_dist
    DO i = 1, unres_i-1_int32
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j),lons(unres_i-i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        west=i-1_int32
        EXIT
      END IF
    END DO
    ! Find how many points can go east and be inside search_dist
    DO i = 1, points_lambda-unres_i
      tempdist=calc_distance(lats(unres_j),lons(unres_i),                      &
                             lats(unres_j),lons(unres_i+i),                    &
                             planet_radius)
      IF (tempdist > search_dist) THEN
        east=i-1_int32
        EXIT
      END IF
    END DO
    ! If LAM can't go past a boundary
    IF (.NOT. cyclic_domain) THEN
      IF ( south == -1 ) THEN
        south=unres_j-1_int32
      END IF
      IF ( north == -1 ) THEN
        north=points_phi-unres_j
      END IF
      IF ( west == -1 ) THEN
        west=unres_i-1_int32
      END IF
      IF ( east == -1 ) THEN
        east=points_lambda-unres_i
      END IF
    END IF

    IF (south == -1_int32 .OR. north == -1_int32 .OR.                          &
         west == -1_int32 .OR. east  == -1_int32) THEN
      ! have hit an edge of a cyclic domain so will have to do the whole domain
      ! Want to avoid doing the whole domain as much as possible
      DO j = 1, points_phi
        DO i = 1, points_lambda
          l = i+(j - 1_int32)*points_lambda
          ! Check to see if it is a resolved point
          IF (.NOT. unres_mask(l)) THEN
            ! Calculate distance from point
            distance(i) = calc_distance(lats(unres_j), lons(unres_i),          &
                                 lats(j), lons(i), planet_radius)
          END IF
        END DO

        DO i = 1, points_lambda
          l = i+(j - 1_int32)*points_lambda
          ! Check to see if it is a resolved point
          IF (.NOT. unres_mask(l)) THEN
            ! Same type of point.
            IF (lsm(l) .EQV. is_land_field) THEN
              ! If current distance is less than any previous min store it.
              IF (distance(i) < curr_dist_valid_min) THEN
                curr_dist_valid_min = distance(i)
                curr_i_valid_min    = i
                curr_j_valid_min    = j
              END IF
            ELSE IF (constrained .OR. allsametype) THEN
              IF (distance(i) < curr_dist_invalid_min .OR.                     &
                curr_dist_invalid_min == rMDI_32b) THEN
                curr_dist_invalid_min = distance(i)
                curr_i_invalid_min=i
                curr_j_invalid_min=j
              END IF
            END IF
          END IF
        END DO
      END DO

      ! Set the unresolved data to something sensible if possible.  Need to keep this
      ! independent due to we dont want it to interact with future searches.
      IF (curr_dist_valid_min <= max_dist) THEN
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int32)*points_lambda
      ELSE IF (allsametype .AND.                                               &
               curr_dist_invalid_min /= rMDI_32b .AND.                         &
               curr_dist_invalid_min > max_dist) THEN
        cmessage = 'There are no resolved points of this type, setting '       &
        // 'to closest resolved point'
        istat = -5
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int32)*points_lambda
      ELSE IF (curr_dist_invalid_min /= rMDI_32b .AND.                         &
               curr_dist_invalid_min <= max_dist) THEN
        indices(k) = curr_i_invalid_min+(curr_j_invalid_min - 1_int32)*points_lambda
      ELSE IF (curr_dist_invalid_min /= rMDI_32b .AND.                         &
               curr_dist_invalid_min > max_dist) THEN
        ! Though hit the maximum search distance there has not been a
        ! resolved point of either type found, therefore use the closest
        ! resolved point of the same type which will be greater than max_dist
        cmessage = 'Despite being constrained there were no resolved points '  &
        // 'of any type within the limit, will just use closest resolved '     &
        // 'point of the same type'
        istat = -10
        indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int32)*points_lambda
      ELSE
        cmessage = 'A point has been left as still unresolved'
        istat = 47
      END IF
      found=.TRUE.

    ELSE ! s,n,w,e /= -1 i.e. not hit edge of a cyclic domain

      IF (south+north /= 0 .OR. east+west /= 0) THEN
        DO j = unres_j-south, unres_j+north
          DO i = unres_i-west, unres_i+east
            l = i+(j - 1_int32)*points_lambda
            ! Check to see if it is a resolved point
            IF (.NOT. unres_mask(l)) THEN
              ! Calculate distance from point
              distance(i) = calc_distance(lats(unres_j),lons(unres_i),         &
                                    lats(j),lons(i), planet_radius)
            END IF
          END DO

          DO i = unres_i-west, unres_i+east
            l = i+(j - 1_int32)*points_lambda
            ! Check to see if it is a resolved point
            IF (.NOT. unres_mask(l)) THEN
              ! Same type of point and distance less than maximum distance.
              IF ((lsm(l) .EQV. is_land_field) .AND.                           &
                       (distance(i) < search_dist)) THEN
                ! If current distance is less than any previous min store it.
                IF (distance(i) < curr_dist_valid_min) THEN
                  curr_dist_valid_min = distance(i)
                  curr_i_valid_min    = i
                  curr_j_valid_min    = j
                END IF
              ELSE IF (constrained .OR. allsametype) THEN
                ! have to add this in as if it isn't constrained don't want it
                ! storing an invalid value
                IF (distance(i) < curr_dist_invalid_min .OR.                   &
                  curr_dist_invalid_min == rMDI_32b) THEN
                  curr_dist_invalid_min = distance(i)
                  curr_i_invalid_min=i
                  curr_j_invalid_min=j
                END IF
              END IF
            END IF
          END DO
        END DO

        ! Set unresolved data to something sensible if possible.  Need to keep this
        ! independent as we don't want it to interact with future searches.
        IF (curr_dist_valid_min < search_dist) THEN
          indices(k) = curr_i_valid_min+(curr_j_valid_min - 1_int32)*points_lambda
          found=.TRUE.
        END IF
        IF (allsametype .AND. curr_dist_invalid_min < search_dist) THEN
          cmessage = 'There are no resolved points of this type, setting '     &
          // 'to closest resolved point'
          istat = -5
          indices(k)=curr_i_invalid_min+(curr_j_invalid_min - 1_int32)*points_lambda
          found=.TRUE.
        END IF
        IF (.NOT. found) THEN
          IF (search_dist+step <= max_dist) THEN
            search_dist=search_dist+step
          ELSE IF (search_dist < max_dist) THEN
            search_dist=max_dist
          ELSE IF (constrained .AND.                                           &
                   curr_dist_invalid_min == rMDI_32b) THEN
            ! Though hit the maximum search distance there has not been a
            ! resolved point of either type found, therefore increase
            ! max_dist by step
            cmessage = 'Despite being constrained there were no resolved '     &
            // 'points of any type within the limit, will just use closest '   &
            // 'resolved point of the same type'
            istat = -20
            max_dist=planet_radius*shum_pi_const_32
            curr_dist_valid_min=max_dist
            search_dist=search_dist+step
          ELSE
            search_dist=max_dist+1.0_real32
          END IF
        END IF
      ELSE
        IF (search_dist+step <= max_dist) THEN
          search_dist=search_dist+step
        ELSE IF (search_dist < max_dist) THEN
          search_dist=max_dist
        ELSE IF (constrained .AND.                                             &
                 curr_dist_invalid_min == rMDI_32b) THEN
          ! Though hit the maximum search distance there has not been a
          ! resolved point of either type found, therefore increase
          ! max_dist by step
          cmessage = 'Despite being constrained there were no resolved '       &
          // 'points of any type within the limit, will just use closest '     &
          // 'resolved point of the same type'
          istat = -30
          max_dist=planet_radius*shum_pi_const_32
          curr_dist_valid_min=max_dist
          search_dist=search_dist+step
        ELSE
          search_dist=max_dist+1
        END IF
      END IF
    END IF
  END DO ! while
  IF (.NOT. found) THEN
    IF (curr_dist_invalid_min /= rMDI_32b) THEN
      indices(k) = curr_i_invalid_min+(curr_j_invalid_min - 1_int32)*points_lambda
    ELSE
      cmessage = 'A point has been left as still unresolved'
      istat = 47
    END IF
  END IF
END DO

END FUNCTION f_shum_spiral_arg32


FUNCTION calc_distance_arg64(lat0, lon0, lat1, lon1, planet_radius)

IMPLICIT NONE

REAL(KIND=REAL64) :: calc_distance_arg64
REAL(KIND=REAL64), INTENT(IN) :: lat0, lon0, lat1, lon1, planet_radius

REAL(KIND=REAL64) :: dlat_rad
REAL(KIND=REAL64) :: dlon_rad
REAL(KIND=REAL64) :: lat0_rad
REAL(KIND=REAL64) :: lat1_rad

lat0_rad = lat0*shum_pi_over_180_const
lat1_rad = lat1*shum_pi_over_180_const
dlat_rad = lat1_rad - lat0_rad
dlon_rad = (lon1-lon0)*shum_pi_over_180_const

! Use the Haversine formula.
calc_distance_arg64 = 2.0_real64*planet_radius*                                &
              ASIN(SQRT(SIN(0.5_real64*dlat_rad)**2_int64 +                    &
              COS(lat0_rad)*COS(lat1_rad)*SIN(0.5_real64*dlon_rad)**2_int64))

END FUNCTION calc_distance_arg64

FUNCTION calc_distance_arg32(lat0, lon0, lat1, lon1, planet_radius)

IMPLICIT NONE

REAL(KIND=REAL32) :: calc_distance_arg32
REAL(KIND=REAL32), INTENT(IN) :: lat0, lon0, lat1, lon1, planet_radius

REAL(KIND=REAL32) :: dlat_rad
REAL(KIND=REAL32) :: dlon_rad
REAL(KIND=REAL32) :: lat0_rad
REAL(KIND=REAL32) :: lat1_rad

lat0_rad = lat0*shum_pi_over_180_const_32
lat1_rad = lat1*shum_pi_over_180_const_32
dlat_rad = lat1_rad - lat0_rad
dlon_rad = (lon1-lon0)*shum_pi_over_180_const_32

! Use the Haversine formula.
calc_distance_arg32 = 2.0_real32*planet_radius*                                &
              ASIN(SQRT(SIN(0.5_real32*dlat_rad)**2_int32 +                    &
              COS(lat0_rad)*COS(lat1_rad)*SIN(0.5_real32*dlon_rad)**2_int32))

END FUNCTION calc_distance_arg32


!------------------------------------------------------------------------------!

END MODULE f_shum_spiral_search_mod
