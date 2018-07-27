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
!
! Description : A module containing constants/parameters for
!               Relative Molecular Mass
!
MODULE f_shum_rel_mol_mass_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                   &
  C_DOUBLE

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_rmm_s_const,       shum_rmm_h2o2_const,     &
          shum_rmm_o3_const,      shum_rmm_air_const,      &
          shum_rmm_w_const

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

! Relative Molecular Mass (kg/mole)
REAL(KIND=real64), PARAMETER :: shum_rmm_s_const = 3.20e-2_real64    ! S
REAL(KIND=real64), PARAMETER :: shum_rmm_h2o2_const = 3.40e-2_real64 ! H2O2
REAL(KIND=real64), PARAMETER :: shum_rmm_o3_const = 4.8e-2_real64    ! O3
REAL(KIND=real64), PARAMETER :: shum_rmm_air_const = 2.896e-2_real64 ! dry air
REAL(KIND=real64), PARAMETER :: shum_rmm_w_const = 1.8e-2_real64     ! water

END MODULE f_shum_rel_mol_mass_mod
