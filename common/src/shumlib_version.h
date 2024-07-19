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
/*   Version inclusion header;                                                */
/******************************************************************************/

/* Check the macro we need is defined.
 * This should happen outside the include guard, else only the top level library
 * is validated
 */
#if !defined(SHUMLIB_LIBNAME)
#error "Please define 'SHUMLIB_LIBNAME' when including this header"
#endif

/* Include guard to prevent including this file multiple times */
#if !defined(SHUMLIB_VERSION_H)
#define SHUMLIB_VERSION_H

#include <inttypes.h>

/* Master definition of version number (uses YYYYMMX format)
 * where "X" is the release number in month "MM" of year "YYYY"
 */
#if !defined(SHUMLIB_VERSION)
#define SHUMLIB_VERSION 2023031
#endif

/* 2-stage expansion which will replace in the including code:
 *    GET_SHUMLIB_VERSION(my_name)  ->   get_my_name_version()
 *  to allow each library to create its own unique version function
 */
#define SHUMLIB_XSTRING_EXPANSION(str) get_##str##_version
#define GET_SHUMLIB_VERSION(libname) SHUMLIB_XSTRING_EXPANSION(libname)(void)

#endif

/* The prototype should be OUTSIDE the include guard, because it is preprocessed
 * to a different function in every include instance.
 */
extern int64_t GET_SHUMLIB_VERSION(SHUMLIB_LIBNAME);
