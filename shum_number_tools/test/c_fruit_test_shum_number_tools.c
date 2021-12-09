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

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include "c_fruit_test_shum_number_tools.h"

/******************************************************************************/
/* prototypes                                                                 */
/******************************************************************************/

/******************************************************************************/
/* tests                                                                      */
/******************************************************************************/

float c_test_generate_finf()
{
  return strtof("INF", NULL);
}

/******************************************************************************/

double c_test_generate_dinf()
{
  return strtod("INF", NULL);
}

/******************************************************************************/

float c_test_generate_fnan()
{
  return nanf("");
}

/******************************************************************************/

double c_test_generate_dnan()
{
  return nan("");
}

/******************************************************************************/

void c_test_generate_fdenormal(float *denormal_float)
{
  union fdenorm_t {
      int32_t bits;
      float denormal;
  } fdenorm;

  fdenorm.bits = UINT32_C(0x1);

  if (fpclassify(fdenorm.denormal)!=FP_SUBNORMAL)
  {
    *denormal_float=0.0;
    return;
  }

  *denormal_float=fdenorm.denormal;
}

/******************************************************************************/

void c_test_generate_ddenormal(double *denomal_double)
{
  union ddenorm_t {
      int64_t bits;
      double denormal;
  } ddenorm;

  ddenorm.bits = UINT64_C(0x1);

  if (fpclassify(ddenorm.denormal)!=FP_SUBNORMAL)
  {
    *denomal_double=0.0;
    return;
  }

  *denomal_double=ddenorm.denormal;
}

/******************************************************************************/
