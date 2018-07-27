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

MODULE c_shum_spiral_search_mod

USE f_shum_string_conv_mod,   ONLY: f_shum_f2c_string
USE f_shum_spiral_search_mod, ONLY: f_shum_spiral_search_algorithm

USE, INTRINSIC :: iso_c_binding, ONLY:                                         &
    C_INT64_T, C_INT32_T, C_CHAR, C_FLOAT, C_DOUBLE, C_BOOL, C_LOC, C_F_POINTER

IMPLICIT NONE 

! Note - this module (intentionally) has nothing set to PUBLIC - this is because
! it shouldn't ever be accessed from Fortran and only exists to provide the
! interface to access the routines from C (which is unaffected by Fortran's
! PRIVATE attribute)
PRIVATE

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION c_shum_spiral_search_algorithm(                                       &
    lsm, index_unres, no_point_unres,                                          &
    points_phi, points_lambda, lats, lons,                                     &
    is_land_field, constrained, constrained_max_dist,                          &
    dist_step, cyclic_domain, unres_mask,                                      &
    indices, planet_radius,                                                    &
    cmessage, message_len) RESULT(status)                                      &
    BIND(c, NAME="c_shum_spiral_search_algorithm")

IMPLICIT NONE

INTEGER(KIND=C_INT64_T)      :: status

INTEGER(KIND=C_INT64_T), INTENT(IN)    :: points_phi
INTEGER(KIND=C_INT64_T), INTENT(IN)    :: points_lambda
LOGICAL(KIND=C_BOOL),    INTENT(IN)    :: lsm(points_lambda*points_phi)
INTEGER(KIND=C_INT64_T), INTENT(IN)    :: no_point_unres 
INTEGER(KIND=C_INT64_T), INTENT(IN)    :: index_unres(no_point_unres)
REAL(KIND=C_DOUBLE),     INTENT(IN)    :: lats(points_phi)
REAL(KIND=C_DOUBLE),     INTENT(IN)    :: lons(points_lambda)
LOGICAL(KIND=C_BOOL),    INTENT(IN)    :: is_land_field
LOGICAL(KIND=C_BOOL),    INTENT(IN)    :: constrained
REAL(KIND=C_DOUBLE),     INTENT(IN)    :: constrained_max_dist
REAL(KIND=C_DOUBLE),     INTENT(IN)    :: dist_step
LOGICAL(KIND=C_BOOL),    INTENT(IN)    :: cyclic_domain
LOGICAL(KIND=C_BOOL),    INTENT(IN)    :: unres_mask(points_lambda*points_phi)
INTEGER(KIND=C_INT64_T), INTENT(OUT),                                          &
                         TARGET        :: indices(no_point_unres)
REAL(KIND=C_DOUBLE),     INTENT(IN)    :: planet_radius
INTEGER(KIND=C_INT64_T), INTENT(IN)    :: message_len
CHARACTER(KIND=C_CHAR, LEN=1),                                                 &
                         INTENT(OUT)   :: cmessage(message_len + 1)

! Intermediary pointer for output
INTEGER(KIND=C_INT64_T), POINTER :: indices_ptr(:)

CHARACTER(LEN=message_len) :: message

CALL C_F_POINTER (C_LOC(indices), indices_ptr, [points_lambda*points_phi])

status = f_shum_spiral_search_algorithm(                                       &
    lsm, index_unres, no_point_unres,                                          &
    points_phi, points_lambda, lats, lons,                                     &
    is_land_field, constrained, constrained_max_dist,                          &
    dist_step, cyclic_domain,                                                  &
    unres_mask, indices_ptr, planet_radius, message)

! If something went wrong allow the calling program to catch the non-zero 
! exit code and error message then act accordingly
IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(TRIM(message))
END IF

NULLIFY(indices_ptr)

END FUNCTION c_shum_spiral_search_algorithm

!------------------------------------------------------------------------------!

END MODULE c_shum_spiral_search_mod
