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
! Description : !  Global standard invariant physical constants/conversions

MODULE f_shum_conversions_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                   &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE

PRIVATE

PUBLIC :: shum_rsec_per_day_const,           shum_isec_per_day_const,          &
          shum_rsec_per_hour_const,          shum_isec_per_hour_const,         &
          shum_isec_per_min_const,           shum_rhour_per_day_const,         &
          shum_ihour_per_day_const,          shum_rhour_per_sec_const,         &
          shum_rday_per_hour_const,          shum_pi_const,                    &
          shum_pi_over_180_const,            shum_180_over_pi_const,           &
          shum_zerodegc_const,               shum_kt2ms_const,                 &
          shum_ft2m_const,                   shum_rsec_per_min_const,          &
          shum_rsec_per_day_const_32,        shum_isec_per_day_const_32,       &
          shum_rsec_per_hour_const_32,       shum_isec_per_hour_const_32,      &
          shum_isec_per_min_const_32,        shum_rhour_per_day_const_32,      &
          shum_ihour_per_day_const_32,       shum_rhour_per_sec_const_32,      &
          shum_rday_per_hour_const_32,       shum_pi_const_32,                 &
          shum_pi_over_180_const_32,         shum_180_over_pi_const_32,        &
          shum_zerodegc_const_32,            shum_kt2ms_const_32,              &
          shum_ft2m_const_32,                shum_rsec_per_min_const_32

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

! Number of seconds in one day - now rsec_per_day and isec_per_day
! which will replace magic number 86400 wherever possible
REAL(KIND=real64),    PARAMETER :: shum_rsec_per_day_const  = 86400.0_real64
INTEGER(KIND=int64),  PARAMETER :: shum_isec_per_day_const  = 86400_int64

REAL(KIND=real64),    PARAMETER :: shum_rsec_per_hour_const = 3600.0_real64
INTEGER(KIND=int64),  PARAMETER :: shum_isec_per_hour_const = 3600_int64

REAL(KIND=real64),    PARAMETER :: shum_rsec_per_min_const  = 60.0_real64
INTEGER(KIND=int64),  PARAMETER :: shum_isec_per_min_const  = 60_int64

REAL(KIND=real64),    PARAMETER :: shum_rhour_per_day_const = 24.0_real64
INTEGER(KIND=int64),  PARAMETER :: shum_ihour_per_day_const = 24_int64

REAL(KIND=real64),    PARAMETER ::                                      &
           shum_rhour_per_sec_const = 1.0_real64/shum_rsec_per_hour_const
REAL(KIND=real64),    PARAMETER ::                                      &
           shum_rday_per_hour_const = 1.0_real64/shum_rhour_per_day_const

! Pi
REAL(KIND=real64), PARAMETER ::                           &
                        shum_pi_const    = 3.14159265358979323846_real64
!Conversion factor degrees to radians
REAL(KIND=real64), PARAMETER ::                           &
                        shum_pi_over_180_const  = shum_pi_const/180.0_real64
!Conversion factor radians to degrees
REAL(KIND=real64), PARAMETER ::                           &
                        shum_180_over_pi_const  = 180.0_real64/shum_pi_const

! zerodegc is a conversion between degrees centigrade and kelvin
REAL(KIND=real64), PARAMETER :: shum_zerodegc_const            = 273.15_real64

! Knots to m/s conversion
REAL(KIND=real64), PARAMETER :: shum_kt2ms_const  = 1852.0_real64/3600.0_real64
! Feet to metres conversion
REAL(KIND=real64), PARAMETER :: shum_ft2m_const   = 0.3048_real64

! -=# As above, but in 32 Bit #=-
REAL(KIND=real32),    PARAMETER ::                                            &
         shum_rsec_per_day_const_32  = REAL(shum_rsec_per_day_const,real32)
INTEGER(KIND=int32),  PARAMETER ::                                            &
         shum_isec_per_day_const_32  = REAL(shum_isec_per_day_const,real32)
REAL(KIND=real32),    PARAMETER ::                                            &
         shum_rsec_per_hour_const_32 = REAL(shum_rsec_per_hour_const,real32)
INTEGER(KIND=int32),  PARAMETER ::                                            &
         shum_isec_per_hour_const_32 = REAL(shum_isec_per_hour_const,real32)
REAL(KIND=real32),  PARAMETER ::                                              &
         shum_rsec_per_min_const_32  = REAL(shum_rsec_per_min_const,real32)
INTEGER(KIND=int32),  PARAMETER ::                                            &
         shum_isec_per_min_const_32  = REAL(shum_isec_per_min_const,real32)
REAL(KIND=real32),    PARAMETER ::                                            &
         shum_rhour_per_day_const_32 = REAL(shum_rhour_per_day_const,real32)
INTEGER(KIND=int32),  PARAMETER ::                                            &
         shum_ihour_per_day_const_32 = REAL(shum_ihour_per_day_const,real32)
REAL(KIND=real32),    PARAMETER ::                                            &
         shum_rhour_per_sec_const_32 = 1.0_real32/shum_rsec_per_hour_const_32
REAL(KIND=real32),    PARAMETER ::                                            &
         shum_rday_per_hour_const_32 = 1.0_real32/shum_rhour_per_day_const_32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_pi_const_32            = 3.14159265358979323846_real32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_pi_over_180_const_32   = shum_pi_const_32/180.0_real32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_180_over_pi_const_32   = 180.0_real32/shum_pi_const_32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_zerodegc_const_32      = 273.15_real32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_kt2ms_const_32         = 1852.0_real32/3600.0_real32
REAL(KIND=real32), PARAMETER ::                                               &
         shum_ft2m_const_32          = 0.3048_real32

END MODULE f_shum_conversions_mod
