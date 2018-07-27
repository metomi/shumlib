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
! Description : Physical constants for the planet earth
!

MODULE f_shum_planet_earth_constants_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_DOUBLE

! Universal constants
USE f_shum_conversions_mod, ONLY: shum_pi_const

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_earth_radius_const,       shum_earth_g_const,       &
          shum_earth_r_const,            shum_earth_cp_const,      &
          shum_earth_pref_const,         shum_earth_sclht_const,   &
          shum_earth_repsilon_const,     shum_earth_lapse_const,   &
          shum_earth_omega_const,        shum_earth_dha_const,     &
          shum_lapse_trop_const,         shum_vkman_const,         &
          shum_rv_const

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
INTEGER, PARAMETER :: real64 = C_DOUBLE
!------------------------------------------------------------------------------!

!----------------------------------------------------------------------
! Earth constants
!----------------------------------------------------------------------

! Earth radius in metres
REAL(KIND=real64), PARAMETER :: shum_earth_radius_const   = 6371229.0_real64

! Mean acceleration due to gravity at the Earth's surface
REAL(KIND=real64), PARAMETER :: shum_earth_g_const        = 9.80665_real64

! Gas constant for dry air
REAL(KIND=real64), PARAMETER :: shum_earth_r_const        = 287.05_real64

! Specific heat of dry air at constant pressure
REAL(KIND=real64), PARAMETER :: shum_earth_cp_const       = 1005.0_real64

! Reference surface pressure
REAL(KIND=real64), PARAMETER :: shum_earth_pref_const     = 100000.0_real64

! Mean scale height for pressure
REAL(KIND=real64), PARAMETER :: shum_earth_sclht_const    = 6.8e+03_real64

! repsilon, ratio of molecular weights of water and dry air
REAL(KIND=real64), PARAMETER :: shum_earth_repsilon_const = 0.62198_real64

! Near surface environmental lapse rate
REAL(KIND=real64), PARAMETER :: shum_earth_lapse_const    = 0.0065_real64

! Angular speed of planet rotation (2*pi/siderial day: 23h56m04s)
REAL(KIND=real64), PARAMETER :: shum_earth_omega_const    = 7.292116e-5_real64

! Increment to Earth's hour angle per day number from epoch
REAL(KIND=real64), PARAMETER :: shum_earth_dha_const = 2.0_real64*shum_pi_const

! Tropopause lapse rate
REAL(KIND=real64), PARAMETER :: shum_lapse_trop_const     = 0.002_real64

! Von Karman's constant
REAL(KIND=real64), PARAMETER :: shum_vkman_const          = 0.4_real64

! Rv - the gas constant for water vapour
! Note repsilon = r/rv  so to be consistent with above definitions rv=r/repsilon
REAL(KIND=real64), PARAMETER :: shum_rv_const  = shum_earth_r_const/   &
                                                 shum_earth_repsilon_const

END MODULE f_shum_planet_earth_constants_mod
