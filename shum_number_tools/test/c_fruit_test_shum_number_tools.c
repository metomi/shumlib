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
#include <float.h>
#if defined(SHUM_X86_INTRINSIC)
#include <stdbool.h>
#include <xmmintrin.h>
#include <xmmintrin.h>
#endif
#include "c_fruit_test_shum_number_tools.h"
#include "c_shum_compiler_select.h"

/******************************************************************************/
/* prototypes                                                                 */
/******************************************************************************/

/******************************************************************************/
/* tests                                                                      */
/******************************************************************************/

float c_test_generate_finf(void)
{
  return strtof("INF", NULL);
}

/******************************************************************************/

double c_test_generate_dinf(void)
{
  return strtod("INF", NULL);
}

/******************************************************************************/

float c_test_generate_fnan(void)
{
  return nanf("");
}

/******************************************************************************/

double c_test_generate_dnan(void)
{
  return nan("");
}

/******************************************************************************/

void c_test_generate_fdenormal(float *denormal_float)
{
#if !defined(SHUM_IS_GNU_COMPILER)
#if defined(__STDC_IEC_559__)
/* tell the compiler we will modify the floating point environment */
#pragma STDC FENV_ACCESS ON
#endif
#endif

#if defined(SHUM_X86_INTRINSIC)
/* save the current MXCSR state of the FZ and DAZ bits */
bool hwftz = _MM_GET_FLUSH_ZERO_MODE();
bool hwdaz = _MM_GET_DENORMALS_ZERO_MODE();

if (hwftz)
{
  _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_OFF);
}

if (hwdaz)
{
  _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
}
#endif

  union fdenorm_t {
      int32_t bits;
      float denormal;
  } fdenorm;

  /* Attempt to genertate a denormal number */

  fdenorm.bits = UINT32_C(0x1);

#if defined(FLT_TRUE_MIN)
  if (fpclassify(fdenorm.denormal)!=FP_SUBNORMAL)
  {
    fdenorm.denormal=FLT_TRUE_MIN;
#endif
    if (fpclassify(fdenorm.denormal)!=FP_SUBNORMAL)
    {
      fdenorm.denormal=FLT_MIN*0.1F;
      if (fpclassify(fdenorm.denormal)!=FP_SUBNORMAL)
      {
        fdenorm.bits = 0;
      }
    }
#if defined(FLT_TRUE_MIN)
  }
#endif

  *denormal_float=fdenorm.denormal;

#if defined(SHUM_X86_INTRINSIC)
if (hwftz)
{
  _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
}

if (hwdaz)
{
  _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
}
#endif
}

/******************************************************************************/

void c_test_generate_ddenormal(double *denomal_double)
{
#if !defined(SHUM_IS_GNU_COMPILER)
#if defined(__STDC_IEC_559__)
/* tell the compiler we will modify the floating point environment */
#pragma STDC FENV_ACCESS ON
#endif
#endif

#if defined(SHUM_X86_INTRINSIC)
/* save the current MXCSR state of the FZ and DAZ bits */
bool hwftz =_MM_GET_FLUSH_ZERO_MODE();
bool hwdaz =_MM_GET_DENORMALS_ZERO_MODE();

if (hwftz)
{
  _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_OFF);
}

if (hwdaz)
{
  _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
}
#endif

  union ddenorm_t {
      int64_t bits;
      double denormal;
  } ddenorm;

  /* Attempt to genertate a denormal number */

  ddenorm.bits = UINT64_C(0x1);

#if defined(DBL_TRUE_MIN)
  if (fpclassify(ddenorm.denormal)!=FP_SUBNORMAL)
  {
    ddenorm.denormal=DBL_TRUE_MIN;
#endif
    if (fpclassify(ddenorm.denormal)!=FP_SUBNORMAL)
    {
      ddenorm.denormal=DBL_MIN*0.1;
      if (fpclassify(ddenorm.denormal)!=FP_SUBNORMAL)
      {
        ddenorm.bits = 0;
      }
    }
#if defined(DBL_TRUE_MIN)
  }
#endif

  *denomal_double=ddenorm.denormal;

#if defined(SHUM_X86_INTRINSIC)
/* restore the MXCSR state of the FZ and DAZ bits (if required) */

if (hwftz)
{
  _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
}

if (hwdaz)
{
  _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
}
#endif
}

/******************************************************************************/
