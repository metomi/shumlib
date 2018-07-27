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
! Description : Water related physical constants

MODULE f_shum_water_constants_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                   &
  C_DOUBLE

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_tfs_const,         shum_tm_const,          &
          shum_rho_water_const,   shum_rhosea_const,      &
          shum_rho_ice_const,     shum_lc_const,          &
          shum_lf_const,          shum_hcapv_const,       &
          shum_hcapw_const,       shum_hcapi_const,       &
          shum_dpsidt_const

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

! tfs, temperature at which sea water freezes
REAL(KIND=real64), PARAMETER :: shum_tfs_const         = 271.35_real64
! tm, temperature at which fresh water freezes and ice melts
REAL(KIND=real64), PARAMETER :: shum_tm_const          = 273.15_real64

! density of pure water (kg/m3)
REAL(KIND=real64), PARAMETER :: shum_rho_water_const   = 1000.0_real64
! density of sea water (kg/m3)
REAL(KIND=real64), PARAMETER :: shum_rhosea_const      = 1026.0_real64
! Density of ice (kg/m3)
REAL(KIND=real64), PARAMETER :: shum_rho_ice_const     = 917.0_real64

! latent heat of condensation of water at 0degc
REAL(KIND=real64), PARAMETER :: shum_lc_const          = 2.501e6_real64
! latent heat of fusion of water at 0degc
REAL(KIND=real64), PARAMETER :: shum_lf_const          = 0.334e6_real64

! Specific heat capacity of water vapour (J/kg/K)
REAL(KIND=real64), PARAMETER :: shum_hcapv_const       = 1850.0_real64
! Specific heat capacity of water (J/kg/K)
REAL(KIND=real64), PARAMETER :: shum_hcapw_const       = 4180.0_real64
! Specific heat capacity of ice (J/kg/K)
REAL(KIND=real64), PARAMETER :: shum_hcapi_const       = 2100.0_real64

! Rate of change of ice potential with temperature
! RHO_ICE*LF/ZERODEGC*1/(RHO_WATER*G) (m/K)
REAL(KIND=real64), PARAMETER :: shum_dpsidt_const      = 114.3_real64

END MODULE f_shum_water_constants_mod
