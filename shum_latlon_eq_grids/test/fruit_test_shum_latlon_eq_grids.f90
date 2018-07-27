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
MODULE fruit_test_shum_latlon_eq_grids_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

PRIVATE

PUBLIC :: fruit_test_shum_latlon_eq_grids

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

! The number of points in the individual test arrays
  INTEGER, PARAMETER :: n = 10
! The number of distinct test cases (poles) used
  INTEGER, PARAMETER :: cases = 6

! Test tolerances
  REAL(KIND=real64), PARAMETER :: test_tol_64 = 1.0E-5_real64
  REAL(KIND=real32), PARAMETER :: test_tol_32 = 1.0E-4_real32

!------------------------------------------------------------------------------!

INTERFACE sample_starting_data
MODULE PROCEDURE sample_starting_data_32, sample_starting_data_64
END INTERFACE

INTERFACE sample_rotated_data
MODULE PROCEDURE sample_rotated_data_32, sample_rotated_data_64
END INTERFACE

INTERFACE sample_wind_data
MODULE PROCEDURE sample_wind_data_32, sample_wind_data_64
END INTERFACE

INTERFACE sample_rotated_wind_data
MODULE PROCEDURE sample_rotated_wind_data_32, sample_rotated_wind_data_64
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

SUBROUTINE fruit_test_shum_latlon_eq_grids

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_latlon_eq_grids_version_mod, ONLY:                                  &
                                          get_shum_latlon_eq_grids_version

IMPLICIT NONE

INTEGER(KIND=int64) :: version

version = get_shum_latlon_eq_grids_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_latlon_eq_grids at Shumlib version: ", version

CALL run_test_case(                                                            &
    test_ll_to_eq_arg64, "ll_to_eq_arg64")
CALL run_test_case(                                                            &
    test_ll_to_eq_arg32, "ll_to_eq_arg32")
CALL run_test_case(                                                            &
    test_eq_to_ll_arg64, "eq_to_ll_arg64")
CALL run_test_case(                                                            &
    test_eq_to_ll_arg32, "eq_to_ll_arg32")
CALL run_test_case(                                                            &
    test_eq_to_ll_w_arg64, "eq_to_ll_w_arg64")
CALL run_test_case(                                                            &
    test_eq_to_ll_w_arg32, "eq_to_ll_w_arg32")
CALL run_test_case(                                                            &
    test_ll_to_eq_w_arg64, "ll_to_eq_w_arg64")
CALL run_test_case(                                                            &
    test_ll_to_eq_w_arg32, "ll_to_eq_w_arg32")

END SUBROUTINE fruit_test_shum_latlon_eq_grids

! Functions used to provide sample arrays of lat/long data - the goal here 
! isn't for a numerical workout but easy to identify and work with arrays.
! The range from -90 to 90 lat and 1 to 360 long is spread across 10 values.
!------------------------------------------------------------------------------!
SUBROUTINE sample_starting_data_64(latitude, longitude)
IMPLICIT NONE 
REAL(KIND=real64), INTENT(OUT) :: latitude(n)
REAL(KIND=real64), INTENT(OUT) :: longitude(n)

latitude(:)  = [ -90.0_real64, -80.0_real64, -60.0_real64, -45.0_real64,       &
                 -30.0_real64,   0.0_real64,  17.0_real64,  38.0_real64,       &
                  69.0_real64,  90.0_real64  ]
longitude(:) = [   0.0_real64,   5.0_real64,  20.0_real64,  60.0_real64,       &
                  75.0_real64,  90.0_real64,  99.0_real64, 168.0_real64,       &
                 279.0_real64,   0.0_real64  ]

END SUBROUTINE sample_starting_data_64

!------------------------------------------------------------------------------!

SUBROUTINE sample_starting_data_32(latitude, longitude) 
IMPLICIT NONE 
REAL(KIND=real32), INTENT(OUT) :: latitude(n)
REAL(KIND=real32), INTENT(OUT) :: longitude(n)

REAL(KIND=real64)   :: latitude_64(n)
REAL(KIND=real64)   :: longitude_64(n)
INTEGER(KIND=int64) :: i

CALL sample_starting_data_64(latitude_64, longitude_64)

DO i=1,n
  latitude(i) = REAL(latitude_64(i), KIND=real32)
  longitude(i) = REAL(longitude_64(i), KIND=real32)
END DO

END SUBROUTINE sample_starting_data_32

! To provide decent coverage the sample rotated data is provided for 6 cases
! based on different pole positions
!------------------------------------------------------------------------------!

SUBROUTINE sample_rotated_data_64                                              &
                     (case_index, pole_lat, pole_lon, latitude_eq, longitude_eq)
IMPLICIT NONE 
INTEGER(KIND=int64), INTENT(IN ) :: case_index
REAL(KIND=real64),   INTENT(OUT) :: pole_lat
REAL(KIND=real64),   INTENT(OUT) :: pole_lon
REAL(KIND=real64),   INTENT(OUT) :: latitude_eq(n)
REAL(KIND=real64),   INTENT(OUT) :: longitude_eq(n)

SELECT CASE (case_index)
  CASE (1_int64)
    ! Note; in this case the "rotated" pole is in the same position as the
    ! regular lat-lon pole, so this case should return the starting values.
    ! *Except* for the points with latitudes of -90.0 or 90.0, the longitude
    ! is effectively undefined at these points and will be returned as 0.0
    pole_lat = 90.0_real64
    pole_lon = 180.0_real64
    latitude_eq(:)  = [ -90.0_real64,                                          &
                        -80.0_real64,                                          &
                        -60.0_real64,                                          &
                        -45.0_real64,                                          &
                        -30.0_real64,                                          &
                          0.0_real64,                                          &
                         17.0_real64,                                          &
                         38.0_real64,                                          &
                         69.0_real64,                                          &
                         90.0_real64  ]

    longitude_eq(:) = [   0.0_real64,                                          &
                          5.0_real64,                                          &
                         20.0_real64,                                          &
                         60.0_real64,                                          &
                         75.0_real64,                                          &
                         90.0_real64,                                          &
                         99.0_real64,                                          &
                        168.0_real64,                                          &
                        279.0_real64,                                          &
                          0.0_real64  ]

  CASE (2_int64)
    pole_lat = 54.0_real64
    pole_lon = 130.0_real64
    latitude_eq(:)  = [ -54.0_real64,                                          &
                        -58.7895666440831_real64,                              &
                        -53.2397218048810_real64,                              &
                        -25.4617609512455_real64,                              &
                         -6.46157700979021_real64,                             &
                         26.7609842064548_real64,                              &
                         45.9183375303789_real64,                              &
                         59.6633088709791_real64,                              &
                         35.0804320498546_real64,                              &
                         54.0_real64  ]

    longitude_eq(:) = [ 179.999998090904_real64,                               &
                        164.067424780139_real64,                               &
                        128.271798984558_real64,                               &
                        132.613004167377_real64,                               &
                        134.443460342966_real64,                               &
                        133.954311410277_real64,                               &
                        134.928761741302_real64,                               &
                        253.849811448046_real64,                               &
                        346.965123607299_real64,                               &
                          0.0_real64  ]

  CASE (3_int64)
    pole_lat = 18.0_real64
    pole_lon = 80.0_real64
    latitude_eq(:)  = [ -18.0000000000000_real64,                              &
                        -15.1637492785758_real64,                              &
                         -1.71067286726960_real64,                             &
                         24.4207289101076_real64,                              &
                         41.7588127942475_real64,                              &
                         69.4894732039266_real64,                              &
                         71.8596760766394_real64,                              &
                         12.4979706108037_real64,                              &
                         -1.93508314394315_real64,                             &
                         18.0000000000000_real64  ]
      
    longitude_eq(:) = [ 179.999999146226_real64,                               &
                        169.992216056674_real64,                               &
                        154.328820636016_real64,                               &
                        164.596942113430_real64,                               &
                        174.192615969597_real64,                               &
                        209.709344178194_real64,                               &
                        269.751129208168_real64,                               &
                        306.230553786676_real64,                               &
                          6.70398224673472_real64,                             &
                          0.0_real64  ]

  CASE (4_int64)
    pole_lat = -18.0_real64
    pole_lon = 30.0_real64
    latitude_eq(:)  = [  18.0000000000000_real64,                              &
                         27.0005058293276_real64,                              &
                         47.3850530206082_real64,                              &
                         53.2169749313423_real64,                              &
                         47.4687959970568_real64,                              &
                         28.3937504351384_real64,                              &
                         13.6262727974178_real64,                              &
                        -48.3479264657106_real64,                              &
                        -24.2446748966316_real64,                              &
                        -18.0000000000000_real64  ]

    longitude_eq(:) = [ 179.999999146226_real64,                               &
                        175.275515243798_real64,                               &
                        172.632364760056_real64,                               &
                        216.188947557755_real64,                               &
                        244.943093060330_real64,                               &
                        280.115766071256_real64,                               &
                        293.271297230166_real64,                               &
                        307.497867496828_real64,                               &
                         21.5263019525786_real64,                              &
                          0.0_real64  ]

  CASE (5_int64)
    pole_lat = -54.0_real64
    pole_lon = -20.0_real64
    latitude_eq(:)  = [  54.0000000000000_real64,                              &
                         62.7767838116441_real64,                              &
                         67.7838555403794_real64,                              &
                         40.1082874036865_real64,                              &
                         21.1089785738208_real64,                              &
                        -11.5974542206932_real64,                              &
                        -30.6003087235067_real64,                              &
                        -73.0882699868836_real64,                              &
                        -40.7803149924612_real64,                              &
                        -54.0000000000000_real64  ]
    
    longitude_eq(:) = [  180.000001909096_real64,                              &
                         189.231457280872_real64,                              &
                         238.213796851124_real64,                              &
                         245.572391211510_real64,                              &
                         247.636063981486_real64,                              &
                         253.592507609032_real64,                              &
                         256.342925265174_real64,                              &
                         157.851738816216_real64,                              &
                          24.4519350174060_real64,                             &
                           0.0_real64  ]

  CASE (6_int64)
    ! Note; for the points with latitudes of 90.0 or -90.0; the longitude
    ! is effectively undefined at these points and will be returned as 0.0
    pole_lat = -90.0_real64
    pole_lon = -70.0_real64
    latitude_eq(:)  = [  90.0_real64,                                          &
                         80.0_real64,                                          &
                         60.0_real64,                                          &
                         45.0_real64,                                          &
                         30.0_real64,                                          &
                          0.0_real64,                                          &
                        -17.0_real64,                                          &
                        -38.0_real64,                                          &
                        -69.0_real64,                                          &
                        -90.0_real64  ]

    longitude_eq(:) = [  250.0_real64,                                         &
                         285.0_real64,                                         &
                         270.0_real64,                                         &
                         230.0_real64,                                         &
                         215.0_real64,                                         &
                         200.0_real64,                                         &
                         191.0_real64,                                         &
                         122.0_real64,                                         &
                          11.0_real64,                                         &
                         250.0_real64  ]

END SELECT

END SUBROUTINE sample_rotated_data_64

!------------------------------------------------------------------------------!

SUBROUTINE sample_rotated_data_32                                              &
                     (case_index, pole_lat, pole_lon, latitude_eq, longitude_eq)
IMPLICIT NONE 
INTEGER(KIND=int32), INTENT(IN ) :: case_index
REAL(KIND=real32),   INTENT(OUT) :: pole_lat
REAL(KIND=real32),   INTENT(OUT) :: pole_lon
REAL(KIND=real32),   INTENT(OUT) :: latitude_eq(n)
REAL(KIND=real32),   INTENT(OUT) :: longitude_eq(n)

INTEGER(KIND=int64) :: case_index_64
REAL(KIND=real64)   :: pole_lat_64
REAL(KIND=real64)   :: pole_lon_64
REAL(KIND=real64)   :: latitude_eq_64(n)
REAL(KIND=real64)   :: longitude_eq_64(n)

INTEGER(KIND=int64) :: i

case_index_64 = INT(case_index, KIND=int64)

CALL sample_rotated_data_64(                                                   &
       case_index_64, pole_lat_64, pole_lon_64, latitude_eq_64, longitude_eq_64)

pole_lat = REAL(pole_lat_64, KIND=real32)
pole_lon = REAL(pole_lon_64, KIND=real32)

DO i = 1,n
  latitude_eq(i) = REAL(latitude_eq_64(i), KIND=real32)
  longitude_eq(i) = REAL(longitude_eq_64(i), KIND=real32)
END DO

END SUBROUTINE sample_rotated_data_32

!------------------------------------------------------------------------------!

SUBROUTINE sample_rotated_wind_data_64                                         &
                     (case_index, pole_lat, pole_lon, longitude_eq, u_ll, v_ll)
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(IN ) :: case_index
REAL(KIND=real64),   INTENT(OUT) :: pole_lat
REAL(KIND=real64),   INTENT(OUT) :: pole_lon
REAL(KIND=real64),   INTENT(OUT) :: longitude_eq(n)
REAL(KIND=real64),   INTENT(OUT) :: u_ll(n)
REAL(KIND=real64),   INTENT(OUT) :: v_ll(n)

REAL(KIND=real64) :: latitude_eq_dummy(n)

! We're sharing data with the test cases from the non-vector rotation tests, 
! so to avoid duplication retrieve the pole co-ordinates and longitudes
! directly from there (the latitudes aren't needed however)
CALL sample_rotated_data(                                                      &
                case_index, pole_lat, pole_lon, latitude_eq_dummy, longitude_eq)

! Now provide the expected results for the wind vectors on the eq grid
SELECT CASE (case_index)
  CASE (1_int64)
    u_ll(:) = [ -20.0_real64,                                                  &
                -12.0_real64,                                                  &
                 -5.0_real64,                                                  &
                  0.0_real64,                                                  &
                  0.3_real64,                                                  &
                  5.0_real64,                                                  &
                  9.9_real64,                                                  &
                 21.0_real64,                                                  &
                 50.0_real64,                                                  &
                120.0_real64  ]

    v_ll(:) = [ -20.0_real64,                                                  &
                 12.0_real64,                                                  &
                 -5.0_real64,                                                  &
                  0.0_real64,                                                  &
                  0.4_real64,                                                  &
                 12.0_real64,                                                  &
                 -9.9_real64,                                                  &
                 21.0_real64,                                                  &
                -50.0_real64,                                                  &
                160.0_real64  ]

  CASE (2_int64)
    u_ll(:) = [  -2.46513572980279_real64,                                     &
                 16.4036524439934_real64,                                      &
                  2.82849141954085_real64,                                     &
                  0.0_real64,                                                  &
                  0.283582564804992_real64,                                    &
                 11.7023568817882_real64,                                      &
                 -8.99897206544481_real64,                                     &
                  0.933171791845741_real64,                                    &
                 28.9585133557067_real64,                                      &
                199.701624425298_real64  ]

    v_ll(:) = [  28.1766411382487_real64,                                      &
                 -4.34973407194856_real64,                                     &
                  6.48071263747929_real64,                                     &
                  0.0_real64,                                                  &
                 -0.411802065147264_real64,                                    &
                 -5.66169969278346_real64,                                     &
                -10.7255995943883_real64,                                      &
                -29.6838203472347_real64,                                      &
                 64.5089490243591_real64,                                      &
                 10.9206777215090_real64  ]

  CASE (3_int64)
    u_ll(:) = [ -23.1691183718383_real64,                                      &
                 12.3204184796072_real64,                                      &
                 -2.89527772301524_real64,                                     &
                  0.0_real64,                                                  &
                  0.300513599726356_real64,                                    &
                 10.4673622694327_real64,                                      &
                -10.8624367341968_real64,                                      &
                  3.06913674758749_real64,                                     &
                 30.2050787224513_real64,                                      &
                136.731456986939_real64  ]
                
    v_ll(:) = [  16.2231918521525_real64,                                      &
                 11.6707878263360_real64,                                      &
                  6.45115237043830_real64,                                     &
                  0.0_real64,                                                  &
                 -0.399614299419373_real64,                                    &
                 -7.70936619447131_real64,                                     &
                 -8.83331495472522_real64,                                     &
                -29.5394718914304_real64,                                      &
                 63.9347575217934_real64,                                      &
               -145.960640825631_real64  ]

  CASE (4_int64)
    u_ll(:) = [ -27.3205081847729_real64,                                      &
                 -3.39028557311053_real64,                                     &
                 -6.98028101148540_real64,                                     &
                  0.0_real64,                                                  &
                  0.412019531672704_real64,                                    &
                 10.0291878814405_real64,                                      &
                -12.5568577930325_real64,                                      &
                 22.2997466637935_real64,                                      &
                 44.2103465842125_real64,                                      &
                -23.9230514129678_real64  ]

    v_ll(:) = [  -7.32050766858131_real64,                                     &
                 16.6284684722544_real64,                                      &
                 -1.12945872022677_real64,                                     &
                  0.0_real64,                                                  &
                 -0.283266513095239_real64,                                    &
                 -8.27135964873773_real64,                                     &
                 -6.19235877992844_real64,                                     &
                -19.6143136186467_real64,                                      &
                 55.1855529545896_real64,                                      &
               -198.564064249029_real64  ]

  CASE (5_int64)
    u_ll(:) = [ -25.6342556805206_real64,                                      &
                 -3.41399144132580_real64,                                     &
                 -4.58978101346331_real64,                                     &
                  0.0_real64,                                                  &
                  2.413148739746743E-002_real64,                               &
                  3.04247017090075_real64,                                     &
                -13.9246276848847_real64,                                      &
                -15.3868164870599_real64,                                      &
                  3.49840347379222_real64,                                     &
                -58.0398979394267_real64  ]

    v_ll(:) = [ -11.9534486950711_real64,                                      &
                 16.6236176098512_real64,                                      &
                  5.37902502768408_real64,                                     &
                  0.0_real64,                                                  &
                 -0.499417343748608_real64,                                    &
                -12.6389625863510_real64,                                      &
                  1.45764492629112_real64,                                     &
                -25.4016904633047_real64,                                      &
                 70.6240835206699_real64,                                      &
               -191.393234590936_real64  ]

  CASE (6_int64)
    u_ll(:) = [   2.46513666864877_real64,                                     &
                 12.0_real64,                                                  &
                  5.0_real64,                                                  &
                  0.0_real64,                                                  &
                 -0.3_real64,                                                  &
                 -5.0_real64,                                                  &
                 -9.9_real64,                                                  &
                -21.0_real64,                                                  &
                -50.0_real64,                                                  &
                 10.9206843755689_real64  ]

    v_ll(:) = [  28.1766410561103_real64,                                      &
                -12.0_real64,                                                  &
                  5.0_real64,                                                  &
                  0.0_real64,                                                  &
                 -0.4_real64,                                                  &
                -12.0_real64,                                                  &
                  9.9_real64,                                                  &
                -21.0_real64,                                                  &
                 50.0_real64,                                                  &
               -199.701624061421_real64  ]

END SELECT

END SUBROUTINE sample_rotated_wind_data_64

!------------------------------------------------------------------------------!

SUBROUTINE sample_rotated_wind_data_32                                         &
                     (case_index, pole_lat, pole_lon, longitude_eq, u_ll, v_ll)
IMPLICIT NONE 
INTEGER(KIND=int32), INTENT(IN ) :: case_index
REAL(KIND=real32),   INTENT(OUT) :: pole_lat
REAL(KIND=real32),   INTENT(OUT) :: pole_lon
REAL(KIND=real32),   INTENT(OUT) :: longitude_eq(n)
REAL(KIND=real32),   INTENT(OUT) :: u_ll(n)
REAL(KIND=real32),   INTENT(OUT) :: v_ll(n)

INTEGER(KIND=int64) :: case_index_64
REAL(KIND=real64)   :: pole_lat_64
REAL(KIND=real64)   :: pole_lon_64
REAL(KIND=real64)   :: longitude_eq_64(n)
REAL(KIND=real64)   :: u_ll_64(n)
REAL(KIND=real64)   :: v_ll_64(n)

INTEGER(KIND=int64) :: i

case_index_64 = INT(case_index, KIND=int64)

CALL sample_rotated_wind_data_64(                                              &
     case_index_64, pole_lat_64, pole_lon_64, longitude_eq_64, u_ll_64, v_ll_64)

pole_lat = REAL(pole_lat_64, KIND=real32)
pole_lon = REAL(pole_lon_64, KIND=real32)

DO i = 1,n
  longitude_eq(i) = REAL(longitude_eq_64(i), KIND=real32)
  u_ll(i) = REAL(u_ll_64(i), KIND=real32)
  v_ll(i) = REAL(v_ll_64(i), KIND=real32)
END DO

END SUBROUTINE sample_rotated_wind_data_32

!------------------------------------------------------------------------------!

SUBROUTINE sample_wind_data_64(lambda, u_eq, v_eq)
IMPLICIT NONE
REAL(KIND=real64), INTENT(OUT) :: lambda(n)
REAL(KIND=real64), INTENT(OUT) :: u_eq(n)
REAL(KIND=real64), INTENT(OUT) :: v_eq(n)

REAL(KIND=real64) :: phi_dummy(n)

! We're sharing data with the test cases from the non-vector rotation tests, 
! so to avoid duplication retrieve the starting longitudes directly from 
! there (the latitudes aren't needed however)
CALL sample_starting_data(phi_dummy, lambda)

u_eq(:) = [ -20.0_real64, -12.0_real64,  -5.0_real64,   0.0_real64,            &
              0.3_real64,   5.0_real64,   9.9_real64,  21.0_real64,            &
             50.0_real64, 120.0_real64  ]

v_eq(:) = [ -20.0_real64,  12.0_real64,  -5.0_real64,   0.0_real64,            &
              0.4_real64,  12.0_real64,  -9.9_real64,  21.0_real64,            &
            -50.0_real64, 160.0_real64  ]

END SUBROUTINE sample_wind_data_64

!------------------------------------------------------------------------------!

SUBROUTINE sample_wind_data_32(lambda, u_eq, v_eq)
IMPLICIT NONE 
REAL(KIND=real32), INTENT(OUT) :: lambda(n)
REAL(KIND=real32), INTENT(OUT) :: u_eq(n)
REAL(KIND=real32), INTENT(OUT) :: v_eq(n)

REAL(KIND=real64)   :: lambda64(n)
REAL(KIND=real64)   :: u_eq_64(n)
REAL(KIND=real64)   :: v_eq_64(n)

INTEGER(KIND=int64) :: i

CALL sample_wind_data_64(lambda64, u_eq_64, v_eq_64)

DO i = 1,n
  lambda(i) = REAL(lambda64(i), KIND=real32)
  u_eq(i) = REAL(u_eq_64(i), KIND=real32)
  v_eq(i) = REAL(v_eq_64(i), KIND=real32)
END DO

END SUBROUTINE sample_wind_data_32

!------------------------------------------------------------------------------!

SUBROUTINE test_ll_to_eq_arg64

USE f_shum_latlon_eq_grids_mod, ONLY: f_shum_latlon_to_eq

IMPLICIT NONE 

REAL(KIND=real64) :: longitude(n)
REAL(KIND=real64) :: latitude(n)

REAL(KIND=real64) :: phi_pole
REAL(KIND=real64) :: phi_eq(n)
REAL(KIND=real64) :: phi_exp(n)

REAL(KIND=real64) :: lambda_pole
REAL(KIND=real64) :: lambda_eq(n)
REAL(KIND=real64) :: lambda_exp(n)

INTEGER(KIND=int64)          :: i
INTEGER(KIND=int64)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

! Retrieve the set of lat/lon points to be tested
CALL sample_starting_data(latitude, longitude)

! The above points will be tested using a variety of target grids, the 
! pole co-ordinates of these and the expected values after rotation are
! returned by a call to "sample_rotated_data" with the case number, i
DO i = 1,cases

  CALL sample_rotated_data(i, phi_pole, lambda_pole, phi_exp, lambda_exp)

  status = f_shum_latlon_to_eq(                                                &
         latitude, longitude, phi_eq, lambda_eq, phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int64, status,                                          &
    "Rotation of ll arrays to eq returned non-zero exit status"//case_info//   &
    " Message: "//TRIM(message))

  CALL assert_equals(phi_exp, phi_eq, n, test_tol_64,                          &
    "Rotated phi array does not agree with expected result"//case_info)

  CALL assert_equals(lambda_exp, lambda_eq, n, test_tol_64,                    &
    "Rotated lambda array does not agree with expected result"//case_info)

END DO

END SUBROUTINE test_ll_to_eq_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_ll_to_eq_arg32

USE f_shum_latlon_eq_grids_mod, ONLY: f_shum_latlon_to_eq

IMPLICIT NONE 

REAL(KIND=real32) :: longitude(n)
REAL(KIND=real32) :: latitude(n)

REAL(KIND=real32) :: phi_pole
REAL(KIND=real32) :: phi_eq(n)
REAL(KIND=real32) :: phi_exp(n)

REAL(KIND=real32) :: lambda_pole
REAL(KIND=real32) :: lambda_eq(n)
REAL(KIND=real32) :: lambda_exp(n)

INTEGER(KIND=int32)          :: i
INTEGER(KIND=int32)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

! Retrieve the set of lat/lon points to be tested
CALL sample_starting_data(latitude, longitude)

! The above points will be tested using a variety of target grids, the 
! pole co-ordinates of these and the expected values after rotation are
! returned by a call to "sample_rotated_data" with the case number, i
DO i = 1,cases

  CALL sample_rotated_data(i, phi_pole, lambda_pole, phi_exp, lambda_exp)

  status = f_shum_latlon_to_eq(                                                &
         latitude, longitude, phi_eq, lambda_eq, phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int32, status,                                          &
    "Rotation of ll arrays to eq returned non-zero exit status"//case_info//   &
    " Message: "//TRIM(message))

  CALL assert_equals(phi_exp, phi_eq, n, test_tol_32,                          &
    "Rotated phi array does not agree with expected result"//case_info)

  CALL assert_equals(lambda_exp, lambda_eq, n, test_tol_32,                    &
    "Rotated lambda array does not agree with expected result"//case_info)

END DO

END SUBROUTINE test_ll_to_eq_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_eq_to_ll_arg64

USE f_shum_latlon_eq_grids_mod, ONLY: f_shum_eq_to_latlon

IMPLICIT NONE 

REAL(KIND=real64) :: phi_pole
REAL(KIND=real64) :: phi_eq(n)
REAL(KIND=real64) :: phi_exp(n)

REAL(KIND=real64) :: lambda_pole
REAL(KIND=real64) :: lambda_eq(n)
REAL(KIND=real64) :: lambda_exp(n)

REAL(KIND=real64) :: latitude(n)
REAL(KIND=real64) :: longitude(n)

INTEGER(KIND=int64)          :: i
INTEGER(KIND=int64)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

! Retrive the *original* data (which we are going to try and rotate back to)
CALL sample_starting_data(phi_exp, lambda_exp)

! Now go through the sample cases un-rotating the data and comparing to the
! original data above 
DO i = 1,cases

  CALL sample_rotated_data(i, phi_pole, lambda_pole, phi_eq, lambda_eq)
  
  status = f_shum_eq_to_latlon(                                                &
         phi_eq, lambda_eq, latitude, longitude, phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int64, status,                                          &
    "Unrotating eq arrays to ll returned non-zero exit status"//case_info//    &
    " Message: "//TRIM(message))

  CALL assert_equals(phi_exp, latitude, n, test_tol_64,                        &
    "Unrotated phi array does not agree with expected result"//case_info)

  CALL assert_equals(lambda_exp, longitude, n, test_tol_64,                    &
    "Unrotated lambda array does not agree with expected result"//case_info)

END DO

END SUBROUTINE test_eq_to_ll_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_eq_to_ll_arg32

USE f_shum_latlon_eq_grids_mod, ONLY: f_shum_eq_to_latlon 

IMPLICIT NONE 

REAL(KIND=real32) :: phi_pole
REAL(KIND=real32) :: phi_eq(n)
REAL(KIND=real32) :: phi_exp(n)

REAL(KIND=real32) :: lambda_pole
REAL(KIND=real32) :: lambda_eq(n)
REAL(KIND=real32) :: lambda_exp(n)

REAL(KIND=real32) :: latitude(n)
REAL(KIND=real32) :: longitude(n)

INTEGER(KIND=int32)          :: i
INTEGER(KIND=int32)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

! Retrive the *original* data (which we are going to try and rotate back to)
CALL sample_starting_data(phi_exp, lambda_exp)

! Now go through the sample cases un-rotating the data and comparing to the
! original data above 
DO i = 1,cases

  CALL sample_rotated_data(i, phi_pole, lambda_pole, phi_eq, lambda_eq)
  
  status = f_shum_eq_to_latlon(                                                &
         phi_eq, lambda_eq, latitude, longitude, phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int32, status,                                          &
    "Unrotating eq arrays to ll returned non-zero exit status"//case_info//    &
    " Message: "//TRIM(message))

  CALL assert_equals(phi_exp, latitude, n, test_tol_32,                        & 
    "Unrotated phi array does not agree with expected result"//case_info)

  CALL assert_equals(lambda_exp, longitude, n, test_tol_32,                    &
    "Unrotated lambda array does not agree with expected result"//case_info)

END DO

END SUBROUTINE test_eq_to_ll_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_eq_to_ll_w_arg64

USE f_shum_latlon_eq_grids_mod, ONLY:                                          &
                     f_shum_latlon_eq_vector_coeff, f_shum_eq_to_latlon_vector

IMPLICIT NONE

REAL(KIND=real64) :: lambda(n)
REAL(KIND=real64) :: lambda_eq(n)
REAL(KIND=real64) :: lambda_pole

REAL(KIND=real64) :: phi_pole

REAL(KIND=real64) :: coeff1(n)
REAL(KIND=real64) :: coeff2(n)

REAL(KIND=real64) :: u(n)
REAL(KIND=real64) :: u_eq(n)
REAL(KIND=real64) :: u_ll(n)

REAL(KIND=real64) :: v(n)
REAL(KIND=real64) :: v_eq(n)
REAL(KIND=real64) :: v_ll(n)

INTEGER(KIND=int64)          :: i
INTEGER(KIND=int64)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

CALL sample_wind_data(lambda, u_eq, v_eq)

DO i = 1,cases
  CALL sample_rotated_wind_data(i, phi_pole, lambda_pole, lambda_eq, u_ll, v_ll)

  status = f_shum_latlon_eq_vector_coeff(coeff1, coeff2, lambda_eq, lambda,    &
                                         phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int64, status,                                          &
    "Calculating vector coeffs returned non-zero exit status"//case_info//     &
    " Message: "//TRIM(message))

  status = f_shum_eq_to_latlon_vector(coeff1, coeff2, u_eq, v_eq, u, v, message)

  CALL assert_equals(0_int64, status,                                          &
       "Rotating vectors eq -> ll returned non-zero exit status"//case_info//  &
       " Message: "//TRIM(message))

  CALL assert_equals(u_ll, u, n, test_tol_64,                                  &
        "Rotated U components do not agree with expected result"//case_info)

  CALL assert_equals(v_ll, v, n, test_tol_64,                                  &
        "Rotated V components do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_eq_to_ll_w_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_eq_to_ll_w_arg32

USE f_shum_latlon_eq_grids_mod, ONLY:                                          &
                     f_shum_latlon_eq_vector_coeff, f_shum_eq_to_latlon_vector

IMPLICIT NONE

REAL(KIND=real32) :: lambda(n)
REAL(KIND=real32) :: lambda_eq(n)
REAL(KIND=real32) :: lambda_pole

REAL(KIND=real32) :: phi_pole

REAL(KIND=real32) :: coeff1(n)
REAL(KIND=real32) :: coeff2(n)

REAL(KIND=real32) :: u(n)
REAL(KIND=real32) :: u_eq(n)
REAL(KIND=real32) :: u_ll(n)

REAL(KIND=real32) :: v(n)
REAL(KIND=real32) :: v_eq(n)
REAL(KIND=real32) :: v_ll(n)

INTEGER(KIND=int32)          :: i
INTEGER(KIND=int32)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

CALL sample_wind_data(lambda, u_eq, v_eq)

DO i = 1,cases
  CALL sample_rotated_wind_data(i, phi_pole, lambda_pole, lambda_eq, u_ll, v_ll)

  status = f_shum_latlon_eq_vector_coeff(coeff1, coeff2, lambda_eq, lambda,    &
                                         phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int32, status,                                          &
    "Calculating vector coeffs returned non-zero exit status"//case_info//     &
    " Message:"//TRIM(message))

  status = f_shum_eq_to_latlon_vector(coeff1, coeff2, u_eq, v_eq, u, v, message)

  CALL assert_equals(0_int32, status,                                          &
       "Rotating vectors eq -> ll returned non-zero exit status"//case_info//  &
       " Message:"//TRIM(message))

  CALL assert_equals(u_ll, u, n, test_tol_32,                                  &
        "Rotated U components do not agree with expected result"//case_info)

  CALL assert_equals(v_ll, v, n, test_tol_32,                                  &
        "Rotated V components do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_eq_to_ll_w_arg32

!------------------------------------------------------------------------------!

SUBROUTINE test_ll_to_eq_w_arg64

USE f_shum_latlon_eq_grids_mod, ONLY:                                          &
                     f_shum_latlon_eq_vector_coeff, f_shum_latlon_to_eq_vector

IMPLICIT NONE

REAL(KIND=real64) :: lambda(n)
REAL(KIND=real64) :: lambda_eq(n)
REAL(KIND=real64) :: lambda_pole

REAL(KIND=real64) :: phi_pole

REAL(KIND=real64) :: coeff1(n)
REAL(KIND=real64) :: coeff2(n)

REAL(KIND=real64) :: u(n)
REAL(KIND=real64) :: u_eq(n)
REAL(KIND=real64) :: u_ll(n)

REAL(KIND=real64) :: v(n)
REAL(KIND=real64) :: v_eq(n)
REAL(KIND=real64) :: v_ll(n)

INTEGER(KIND=int64)          :: i
INTEGER(KIND=int64)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

CALL sample_wind_data(lambda, u_eq, v_eq)

DO i = 1,cases
  CALL sample_rotated_wind_data(i, phi_pole, lambda_pole, lambda_eq, u_ll, v_ll)

  status = f_shum_latlon_eq_vector_coeff(coeff1, coeff2, lambda_eq, lambda,    &
                                         phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int64, status,                                          &
      "Calculating vector coeffs returned non-zero exit status"//case_info//   &
      " Message: "//TRIM(message))

  status = f_shum_latlon_to_eq_vector(coeff1, coeff2, u_ll, v_ll, u, v, message)

  CALL assert_equals(0_int64, status,                                          &
      "Rotating vectors ll -> eq returned non-zero exit status"//case_info//   &
      " Message: "//TRIM(message))

  CALL assert_equals(u_eq, u, n, test_tol_64,                                  &
      "Unrotated U components do not agree with expected result"//case_info)

  CALL assert_equals(v_eq, v, n, test_tol_64,                                  &
      "Unrotated V components do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_ll_to_eq_w_arg64

!------------------------------------------------------------------------------!

SUBROUTINE test_ll_to_eq_w_arg32

USE f_shum_latlon_eq_grids_mod, ONLY:                                          &
                     f_shum_latlon_eq_vector_coeff, f_shum_latlon_to_eq_vector

IMPLICIT NONE

REAL(KIND=real32) :: lambda(n)
REAL(KIND=real32) :: lambda_eq(n)
REAL(KIND=real32) :: lambda_pole

REAL(KIND=real32) :: phi_pole

REAL(KIND=real32) :: coeff1(n)
REAL(KIND=real32) :: coeff2(n)

REAL(KIND=real32) :: u(n)
REAL(KIND=real32) :: u_eq(n)
REAL(KIND=real32) :: u_ll(n)

REAL(KIND=real32) :: v(n)
REAL(KIND=real32) :: v_eq(n)
REAL(KIND=real32) :: v_ll(n)

INTEGER(KIND=int32)          :: i
INTEGER(KIND=int32)          :: status
CHARACTER(LEN=500)           :: message
CHARACTER(LEN=20)            :: case_info

CALL sample_wind_data(lambda, u_eq, v_eq)

DO i = 1,cases
  CALL sample_rotated_wind_data(i, phi_pole, lambda_pole, lambda_eq, u_ll, v_ll)

  status = f_shum_latlon_eq_vector_coeff(coeff1, coeff2, lambda_eq, lambda,    &
                                         phi_pole, lambda_pole, message)

  WRITE(case_info, "(A,I0,A)") " (Test pole case: ", i,")"

  CALL assert_equals(0_int32, status,                                          &
      "Calculating vector coeffs returned non-zero exit status"//case_info//   &
      " Message: "//TRIM(message))

  status = f_shum_latlon_to_eq_vector(coeff1, coeff2, u_ll, v_ll, u, v, message)

  CALL assert_equals(0_int32, status,                                          &
      "Rotating vectors ll -> eq returned non-zero exit status"//case_info//   &
      " Message: "//TRIM(message))

  CALL assert_equals(u_eq, u, n, test_tol_32,                                  &
      "Unrotated U components do not agree with expected result"//case_info)

  CALL assert_equals(v_eq, v, n, test_tol_32,                                  &
      "Unrotated V components do not agree with expected result"//case_info)

END DO

END SUBROUTINE test_ll_to_eq_w_arg32

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_latlon_eq_grids_mod
