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
! Description: Indices for the fixed length header.
!
MODULE f_shum_fixed_length_header_indices_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE

PUBLIC

PRIVATE :: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
  INTEGER, PRIVATE, PARAMETER :: int64  = C_INT64_T
  INTEGER, PRIVATE, PARAMETER :: int32  = C_INT32_T
  INTEGER, PRIVATE, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: real32 = C_FLOAT
!------------------------------------------------------------------------------!

INTEGER(KIND=int64), PARAMETER :: data_set_format_version = 1
INTEGER(KIND=int64), PARAMETER :: sub_model               = 2
INTEGER(KIND=int64), PARAMETER :: vert_coord_type         = 3
INTEGER(KIND=int64), PARAMETER :: horiz_grid_type         = 4
INTEGER(KIND=int64), PARAMETER :: dataset_type            = 5
INTEGER(KIND=int64), PARAMETER :: run_identifier          = 6
INTEGER(KIND=int64), PARAMETER :: experiment_number       = 7
INTEGER(KIND=int64), PARAMETER :: calendar                = 8
INTEGER(KIND=int64), PARAMETER :: grid_staggering         = 9
INTEGER(KIND=int64), PARAMETER :: time_type               = 10
INTEGER(KIND=int64), PARAMETER :: projection_number       = 11
INTEGER(KIND=int64), PARAMETER :: model_version           = 12
INTEGER(KIND=int64), PARAMETER :: obs_file_type           = 14
INTEGER(KIND=int64), PARAMETER :: last_fieldop_type       = 15
INTEGER(KIND=int64), PARAMETER :: t1_year                 = 21
INTEGER(KIND=int64), PARAMETER :: t1_month                = 22
INTEGER(KIND=int64), PARAMETER :: t1_day                  = 23
INTEGER(KIND=int64), PARAMETER :: t1_hour                 = 24
INTEGER(KIND=int64), PARAMETER :: t1_minute               = 25
INTEGER(KIND=int64), PARAMETER :: t1_second               = 26
INTEGER(KIND=int64), PARAMETER :: t1_year_day_number      = 27
INTEGER(KIND=int64), PARAMETER :: t2_year                 = 28
INTEGER(KIND=int64), PARAMETER :: t2_month                = 29
INTEGER(KIND=int64), PARAMETER :: t2_day                  = 30
INTEGER(KIND=int64), PARAMETER :: t2_hour                 = 31
INTEGER(KIND=int64), PARAMETER :: t2_minute               = 32
INTEGER(KIND=int64), PARAMETER :: t2_second               = 33
INTEGER(KIND=int64), PARAMETER :: t2_year_day_number      = 34
INTEGER(KIND=int64), PARAMETER :: t3_year                 = 35
INTEGER(KIND=int64), PARAMETER :: t3_month                = 36
INTEGER(KIND=int64), PARAMETER :: t3_day                  = 37
INTEGER(KIND=int64), PARAMETER :: t3_hour                 = 38
INTEGER(KIND=int64), PARAMETER :: t3_minute               = 39
INTEGER(KIND=int64), PARAMETER :: t3_second               = 40
INTEGER(KIND=int64), PARAMETER :: t3_year_day_number      = 41
INTEGER(KIND=int64), PARAMETER :: num_prognostic_fields   = 153

! Positional elements - note that these are *not* updateable by the user
! directly and are only written to by the API (to ensure everything remains
! consistent)
INTEGER(KIND=int64), PARAMETER :: int_const_start         = 100
INTEGER(KIND=int64), PARAMETER :: int_const_dim           = 101
INTEGER(KIND=int64), PARAMETER :: real_const_start        = 105
INTEGER(KIND=int64), PARAMETER :: real_const_dim          = 106
INTEGER(KIND=int64), PARAMETER :: lev_dep_const_start     = 110
INTEGER(KIND=int64), PARAMETER :: lev_dep_const_dim1      = 111
INTEGER(KIND=int64), PARAMETER :: lev_dep_const_dim2      = 112
INTEGER(KIND=int64), PARAMETER :: row_dep_const_start     = 115
INTEGER(KIND=int64), PARAMETER :: row_dep_const_dim1      = 116
INTEGER(KIND=int64), PARAMETER :: row_dep_const_dim2      = 117
INTEGER(KIND=int64), PARAMETER :: col_dep_const_start     = 120
INTEGER(KIND=int64), PARAMETER :: col_dep_const_dim1      = 121
INTEGER(KIND=int64), PARAMETER :: col_dep_const_dim2      = 122
INTEGER(KIND=int64), PARAMETER :: additional_const_start  = 125
INTEGER(KIND=int64), PARAMETER :: additional_const_dim1   = 126
INTEGER(KIND=int64), PARAMETER :: additional_const_dim2   = 127
INTEGER(KIND=int64), PARAMETER :: extra_const_start       = 130
INTEGER(KIND=int64), PARAMETER :: extra_const_dim         = 131
INTEGER(KIND=int64), PARAMETER :: temp_histfile_start     = 135
INTEGER(KIND=int64), PARAMETER :: temp_histfile_dim       = 136
INTEGER(KIND=int64), PARAMETER :: comp_field_index1_start = 140
INTEGER(KIND=int64), PARAMETER :: comp_field_index1_dim   = 141
INTEGER(KIND=int64), PARAMETER :: comp_field_index2_start = 142
INTEGER(KIND=int64), PARAMETER :: comp_field_index2_dim   = 143
INTEGER(KIND=int64), PARAMETER :: comp_field_index3_start = 144
INTEGER(KIND=int64), PARAMETER :: comp_field_index3_dim   = 145
INTEGER(KIND=int64), PARAMETER :: lookup_start            = 150
INTEGER(KIND=int64), PARAMETER :: lookup_dim1             = 151
INTEGER(KIND=int64), PARAMETER :: lookup_dim2             = 152
INTEGER(KIND=int64), PARAMETER :: data_start              = 160
INTEGER(KIND=int64), PARAMETER :: data_dim                = 161

!------------------------------------------------------------------------------!

END MODULE f_shum_fixed_length_header_indices_mod
