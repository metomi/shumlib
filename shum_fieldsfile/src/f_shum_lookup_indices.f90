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
! Description: Indices for the lookup headers.
!
MODULE f_shum_lookup_indices_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

PRIVATE

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

INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbyr    = 1
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbmon   = 2
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbdat   = 3
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbhr    = 4
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbmin   = 5
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbday   = 6
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbsec   = 6
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbyrd   = 7
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbmond  = 8
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbdatd  = 9
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbhrd   = 10
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbmind  = 11
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbdayd  = 12
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbsecd  = 12
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbtim   = 13
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbft    = 14
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lblrec  = 15
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbcode  = 16
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbhem   = 17
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrow   = 18
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbnpt   = 19
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbext   = 20
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbpack  = 21
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrel   = 22
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbfc    = 23
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbcfc   = 24
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbproc  = 25
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbvc    = 26
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrvc   = 27
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbexp   = 28
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbegin  = 29
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbnrec  = 30
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbproj  = 31
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbtyp   = 32
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lblev   = 33
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrsvd1 = 34
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrsvd2 = 35
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrsvd3 = 36
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbrsvd4 = 37
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbsrce  = 38
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser1 = 39
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser2 = 40
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser3 = 41
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser4 = 42
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser5 = 43
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser6 = 44
INTEGER(KIND=int64), PARAMETER, PUBLIC :: lbuser7 = 45
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bulev   = 46
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bhulev  = 47
INTEGER(KIND=int64), PARAMETER, PUBLIC :: brsvd3  = 48
INTEGER(KIND=int64), PARAMETER, PUBLIC :: brsvd4  = 49
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bdatum  = 50
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bacc    = 51
INTEGER(KIND=int64), PARAMETER, PUBLIC :: blev    = 52
INTEGER(KIND=int64), PARAMETER, PUBLIC :: brlev   = 53
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bhlev   = 54
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bhrlev  = 55
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bplat   = 56
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bplon   = 57
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bgor    = 58
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bzy     = 59
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bdy     = 60
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bzx     = 61
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bdx     = 62
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bmdi    = 63
INTEGER(KIND=int64), PARAMETER, PUBLIC :: bmks    = 64

!------------------------------------------------------------------------------!

END MODULE f_shum_lookup_indices_mod
