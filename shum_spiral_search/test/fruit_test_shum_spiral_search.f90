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
MODULE fruit_test_shum_spiral_search_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
                  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE, C_BOOL

IMPLICIT NONE

PRIVATE

PUBLIC :: fruit_test_shum_spiral_search

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
  INTEGER, PARAMETER :: bool   = C_BOOL
!------------------------------------------------------------------------------!

! The number of distinct test cases used
INTEGER, PARAMETER :: cases = 2

!------------------------------------------------------------------------------!

INTERFACE sample_6x6_data
MODULE PROCEDURE sample_6x6_data_32, sample_6x6_data_64
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

SUBROUTINE fruit_test_shum_spiral_search

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_spiral_search_version_mod, ONLY: get_shum_spiral_search_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

version = get_shum_spiral_search_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_spiral_search at Shumlib version: ", version

CALL run_test_case(                                                            &
    test_spiral6_search_arg64, "spiral6_search_arg64")

CALL run_test_case(                                                            &
    test_spiral6_search_arg32, "spiral6_search_arg32")

END SUBROUTINE fruit_test_shum_spiral_search

! Functions used to provide sample arrays of lat/long data - the goal here
! isn't for a numerical workout but easy to identify and work with arrays.
!------------------------------------------------------------------------------!

SUBROUTINE sample_6x6_data_64                                       &
                             (latitude, longitude, lsm, unres_mask, &
                                   index_unres, planet_radius )
IMPLICIT NONE
REAL(KIND=real64), INTENT(OUT) :: latitude(6)
REAL(KIND=real64), INTENT(OUT) :: longitude(6)
LOGICAL(KIND=bool), INTENT(OUT) :: lsm(36)
LOGICAL(KIND=bool), INTENT(OUT) :: unres_mask(36)
INTEGER(KIND=int64), INTENT(OUT) :: index_unres(5)
REAL(KIND=real64), INTENT(OUT) :: planet_radius

latitude(:)  = [ 30.0_real64, 35.0_real64, 40.0_real64, 45.0_real64,           &
                 50.0_real64, 55.0_real64 ]
longitude(:) = [  0.0_real64,  5.0_real64, 10.0_real64, 15.0_real64,           &
                 20.0_real64, 25.0_real64 ]
lsm(:) = [     .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE.,                 &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE.    ]
unres_mask(:)=[.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,                &
               .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.   ]

index_unres(1:5) = [ 1_int64, 2_int64, 7_int64, 21_int64, 28_int64]
planet_radius = 6000000.0_real64

END SUBROUTINE sample_6x6_data_64

!------------------------------------------------------------------------------!

SUBROUTINE sample_6x6_data_32                                                  &
                             (latitude, longitude, lsm, unres_mask,            &
                              index_unres, planet_radius )
IMPLICIT NONE
REAL(KIND=real32), INTENT(OUT) :: latitude(6)
REAL(KIND=real32), INTENT(OUT) :: longitude(6)
LOGICAL(KIND=bool), INTENT(OUT) :: lsm(36)
LOGICAL(KIND=bool), INTENT(OUT) :: unres_mask(36)
INTEGER(KIND=int32), INTENT(OUT) :: index_unres(5)
REAL(KIND=real32)   :: planet_radius

REAL(KIND=real64)   :: latitude_64(6)
REAL(KIND=real64)   :: longitude_64(6)
INTEGER(KIND=int64) :: index_unres_64(5)
REAL(KIND=real64)   :: planet_radius_64

INTEGER(KIND=int32) :: i

CALL sample_6x6_data_64(latitude_64, longitude_64, lsm, unres_mask,&
                             index_unres_64, planet_radius_64 )

DO i=1,6
  latitude(i) = REAL(latitude_64(i), KIND=real32)
  longitude(i) = REAL(longitude_64(i), KIND=real32)
END DO
DO i=1,5
  index_unres(i) = INT(index_unres_64(i), KIND=int32)
END DO
planet_radius = REAL(planet_radius_64, KIND=real32)

END SUBROUTINE sample_6x6_data_32

!------------------------------------------------------------------------------!

SUBROUTINE test_spiral6_search_arg64

USE f_shum_spiral_search_mod, ONLY: f_shum_spiral_search_algorithm

IMPLICIT NONE

INTEGER(KIND=int64), PARAMETER :: no_point_unres = 5
INTEGER(KIND=int64), PARAMETER :: points_phi = 6
INTEGER(KIND=int64), PARAMETER :: points_lambda = 6

REAL(KIND=real64) :: lats(points_phi)
REAL(KIND=real64) :: lons(points_lambda)

LOGICAL(KIND=bool) :: lsm(points_phi*points_lambda)
LOGICAL(KIND=bool) :: unres_mask(points_phi*points_lambda)
INTEGER(KIND=int64) :: index_unres(no_point_unres)
INTEGER(KIND=int64) :: indices(no_point_unres)

LOGICAL(KIND=bool) :: is_land_field = .TRUE.
LOGICAL(KIND=bool) :: constrained   = .FALSE.
LOGICAL(KIND=bool) :: cyclic_domain = .FALSE.

REAL(KIND=real64) :: constrained_max_dist = 200000.0
REAL(KIND=real64) :: planet_radius
REAL(KIND=real64) :: dist_step = 3.0

INTEGER(KIND=int64) :: result_land(no_point_unres)
INTEGER(KIND=int64) :: result_land_con(no_point_unres)
INTEGER(KIND=int64) :: result_sea(no_point_unres)

INTEGER(KIND=int64)          :: i
INTEGER(KIND=int64)          :: status
CHARACTER(LEN=400)           :: message
CHARACTER(LEN=200)           :: case_info

! Retrieve the set of data points to be tested
CALL sample_6x6_data(lats, lons, lsm, unres_mask, index_unres, planet_radius )

! Set expected results arrays
result_land(:) = [29_int64, 29_int64, 29_int64, 29_int64, 29_int64 ]
result_land_con(:) = [29_int64, 29_int64, 29_int64, 29_int64, 29_int64 ]
result_sea(:) = [8_int64, 3_int64, 8_int64, 20_int64, 27_int64 ]

WRITE(case_info, "(A,36L1,A)") " (Test lsm: ",lsm ,")"

! The above grid will be tested using the 3 spiral search options
! and for non-cyclic & cyclic (though this appears to make no difference)

DO i = 1,cases

  IF (cases==2) THEN
    cyclic_domain=.TRUE.
  END IF

  is_land_field = .TRUE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  WRITE(case_info, "(A,I0,A)") " (Test spiral case: ", i,")"

  CALL assert_equals(0_int64, status,                                          &
    "Land Spiral search algorithm returned non-zero exit status"//case_info//  &
    " Message: "//TRIM(message))

  CALL assert_equals(result_land, indices, no_point_unres,                     &
    "Resolved land indices do not agree with expected result"//case_info)

  constrained   = .TRUE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  CALL assert_equals(-30_int64, status,                                        &
    "Constrained Land Spiral search algorithm exit status.ne.-30"//case_info// &
    " Message: "//TRIM(message))

  CALL assert_equals(result_land_con, indices, no_point_unres,                 &
    "Resolved land indices do not agree with expected result"//case_info)

  is_land_field = .FALSE.
  constrained = .FALSE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  CALL assert_equals(0_int64, status,                                          &
    "Sea Spiral search algorithm returned non-zero exit status"//case_info//   &
    " Message: "//TRIM(message))

  CALL assert_equals(result_sea, indices, no_point_unres,                      &
    "Resolved sea indices do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_spiral6_search_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_spiral6_search_arg32

USE f_shum_spiral_search_mod, ONLY: f_shum_spiral_search_algorithm

IMPLICIT NONE

INTEGER(KIND=int32), PARAMETER :: no_point_unres = 5
INTEGER(KIND=int32), PARAMETER :: points_phi = 6
INTEGER(KIND=int32), PARAMETER :: points_lambda = 6

REAL(KIND=real32) :: lats(points_phi)
REAL(KIND=real32) :: lons(points_lambda)

LOGICAL(KIND=bool) :: lsm(points_phi*points_lambda)
LOGICAL(KIND=bool) :: unres_mask(points_phi*points_lambda)
INTEGER(KIND=int32) :: index_unres(no_point_unres)
INTEGER(KIND=int32) :: indices(no_point_unres)

LOGICAL(KIND=bool) :: is_land_field = .TRUE.
LOGICAL(KIND=bool) :: constrained   = .FALSE.
LOGICAL(KIND=bool) :: cyclic_domain = .FALSE.

REAL(KIND=real32)   :: planet_radius
REAL(KIND=real32)   :: constrained_max_dist = 200000.0
REAL(KIND=real32) :: dist_step = 3.0

INTEGER(KIND=int32) :: result_land(no_point_unres)
INTEGER(KIND=int32) :: result_land_con(no_point_unres)
INTEGER(KIND=int32) :: result_sea(no_point_unres)

INTEGER(KIND=int32)          :: i
INTEGER(KIND=int32)          :: status
CHARACTER(LEN=400)           :: message
CHARACTER(LEN=200)           :: case_info

! Retrieve the set of data points to be tested - should find 32bit version
CALL sample_6x6_data(lats, lons, lsm, unres_mask, index_unres, &
                          planet_radius )

! Set expected results arrays
result_land(:) = [29_int32, 29_int32, 29_int32, 29_int32, 29_int32 ]
result_land_con(:) = [29_int32, 29_int32, 29_int32, 29_int32, 29_int32 ]
result_sea(:) = [8_int32, 3_int32, 8_int32, 20_int32, 27_int32 ]

WRITE(case_info, "(A,36L1,A)") " (Test lsm: ",lsm ,")"

! The above grid will be tested using the 3 spiral search options
! and the non-cyclic and cyclic option

DO i = 1,cases

  WRITE(case_info, "(A,I0,A)") " (Test spiral case: ", i,")"

  IF (cases==2) THEN
    cyclic_domain=.TRUE.
  END IF

  is_land_field = .TRUE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  CALL assert_equals(0_int32, status,                                          &
    "Land Spiral search algorithm returned non-zero exit status"//case_info//  &
    " Message: "//TRIM(message))

  CALL assert_equals(result_land, indices, no_point_unres,                     &
    "Resolved land indices do not agree with expected result"//case_info)

  constrained   = .TRUE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  CALL assert_equals(-30_int32, status,                                        &
    "Constrained Land Spiral search algorithm exit status .ne.-30"//case_info//&
    " Message: "//TRIM(message))

  CALL assert_equals(result_land_con, indices, no_point_unres,                 &
    "Resolved land indices do not agree with expected result"//case_info)

  is_land_field = .FALSE.
  constrained = .FALSE.
  status = f_shum_spiral_search_algorithm                                      &
           (lsm, index_unres, no_point_unres,                                  &
            points_phi, points_lambda, lats, lons,                             &
            is_land_field, constrained, constrained_max_dist,                  &
            dist_step, cyclic_domain, unres_mask,                              &
            indices, planet_radius, message)

  CALL assert_equals(0_int32, status,                                          &
    "Sea Spiral search algorithm returned non-zero exit status"//case_info//   &
    " Message: "//TRIM(message))

  CALL assert_equals(result_sea, indices, no_point_unres,                      &
    "Resolved sea indices do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_spiral6_search_arg32

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_spiral_search_mod
