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
#ifndef C_SHUM_WGDOS_PACKING_H
#define C_SHUM_WGDOS_PACKING_H

#include <inttypes.h>

extern int64_t c_shum_read_wgdos_header(
                      char    *bytes_in,
                      int64_t *num_words,
                      int64_t *accuracy,
                      int64_t *cols,
                      int64_t *rows,
                      char    *message,
                      int64_t *message_len);

extern int64_t c_shum_wgdos_pack(
                      double  *field, 
                      int64_t *cols, 
                      int64_t *rows, 
                      int64_t *accuracy, 
                      double  *rmdi, 
                      int32_t *comp_field, 
                      int64_t *len_comp, 
                      int64_t *num_words, 
                      char    *err_msg,
                      int64_t *message_len);

extern int64_t c_shum_wgdos_unpack(
                      int32_t *comp_field, 
                      int64_t *len_comp, 
                      int64_t *cols, 
                      int64_t *rows, 
                      double  *rmdi, 
                      double  *field, 
                      char    *err_msg,
                      int64_t *message_len);

#endif
