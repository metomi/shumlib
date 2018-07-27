/* *********************************COPYRIGHT**********************************/
/* (C) Crown copyright Met Office. All rights reserved.                       */
/* For further details please refer to the file LICENCE.txt                   */
/* which you should have received as part of this distribution.               */
/* *********************************COPYRIGHT**********************************/
/*                                                                            */
/* This file is part of the UM Shared Library project.                        */
/*                                                                            */
/* The UM Shared Library is free software: you can redistribute it            */
/* and/or modify it under the terms of the Modified BSD License, as           */
/* published by the Open Source Initiative.                                   */
/*                                                                            */
/* The UM Shared Library is distributed in the hope that it will be           */
/* useful, but WITHOUT ANY WARRANTY; without even the implied warranty        */
/* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* Modified BSD License for more details.                                     */
/*                                                                            */
/* You should have received a copy of the Modified BSD License                */
/* along with the UM Shared Library.                                          */
/* If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 */
/******************************************************************************/
#ifndef C_SHUM_SPIRAL_SEARCH_H
#define C_SHUM_SPIRAL_SEARCH_H

#include <inttypes.h>
#include <stdbool.h>

extern int64_t c_shum_spiral_search_algorithm(
                      bool    *lsm,
                      int64_t *index_unres,
                      int64_t *no_point_unres,
                      int64_t *points_phi,
                      int64_t *points_lambda,
                      double  *lats,
                      double  *lons,
                      bool    *is_land_field,
                      bool    *constrained,
                      double  *constrained_max_dist,
                      double  *dist_step,
                      bool    *cyclic,
                      bool    *unres_mask,
                      int64_t *indices,
                      double  *planet_radius,
                      char    *message,
                      int64_t *message_len);

#endif
