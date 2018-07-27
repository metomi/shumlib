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
/* Description:                                                               */
/*   Version inclusion routine; when compiled with the macro SHUMLIB_LIBNAME  */
/*   set this file will create a function to retrieve the (hard-coded)        */
/*   version number (with the name "get_SHUMLIB_LIBNAME_version()"            */
/*                                                                            */
/*   At the same time it includes the "precision bomb" header which means all */
/*   Shumlib code will be checked for suitable precisions                     */
/******************************************************************************/

#include <inttypes.h>
#include "precision_bomb.h"
#include "shumlib_version.h"

int64_t GET_SHUMLIB_VERSION(SHUMLIB_LIBNAME) {
  return SHUMLIB_VERSION;}
