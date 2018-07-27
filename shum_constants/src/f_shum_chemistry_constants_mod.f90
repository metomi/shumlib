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
! Description : A module containing constants/parameters for Chemistry
!
MODULE f_shum_chemistry_constants_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_DOUBLE

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_avogadro_const,        shum_boltzmann_const,                   &
          shum_rho_so4_const,         shum_ref_mfp_const,                     &
          shum_tref_mfp_const,        shum_pref_mfp_const

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

! No. of molecules in 1 mole
REAL(KIND=real64), PARAMETER :: shum_avogadro_const = 6.022e23_real64

! Boltzmanns constant (J K-1)
REAL(KIND=real64), PARAMETER :: shum_boltzmann_const = 1.3804e-23_real64

! Density of SO4 particle (kg/m3)
REAL(KIND=real64), PARAMETER :: shum_rho_so4_const = 1769.0_real64

! Mean Free Path 
! Ref value (m)
REAL(KIND=real64), PARAMETER :: shum_ref_mfp_const  = 6.6e-8_real64
! Ref temperature (K)
REAL(KIND=real64), PARAMETER :: shum_tref_mfp_const = 293.15_real64
! Ref pressure (Pa)
REAL(KIND=real64), PARAMETER :: shum_pref_mfp_const = 1.01325e5_real64

END MODULE f_shum_chemistry_constants_mod

