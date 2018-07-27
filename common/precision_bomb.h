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
/*   Precision bomb pre-processor file. Including this in a compilation will  */
/*   ensure that the precision of floats and doubles corresponds to IEC 60559 */
/*   and fail if this is not the case.                                        */
/******************************************************************************/

#include <float.h>

#if defined(__STDC_IEC_559__)

  /* Supports C99 ANNEX F */

#else

  /* float */
  #if defined(SHUM_FLOAT_BOMB)
    #undef SHUM_FLOAT_BOMB
  #endif

  #if FLT_RADIX != 2
    #define SHUM_FLOAT_BOMB
  #endif

  #if FLT_MANT_DIG != 24
    #if !defined(SHUM_FLOAT_BOMB)
      #define SHUM_FLOAT_BOMB
    #endif
  #endif

  #if FLT_MAX_EXP != 128
    #if !defined(SHUM_FLOAT_BOMB)
      #define SHUM_FLOAT_BOMB
    #endif
  #endif

  #if FLT_MIN_EXP != -125
    #if !defined(SHUM_FLOAT_BOMB)
      #define SHUM_FLOAT_BOMB
    #endif
  #endif

  #if defined(SHUM_FLOAT_BOMB)
    #error the type "float" does not conform to IEC 60559 single format. This is a requirement for this code to function correctly.
  #endif

  /* double */
  #if defined(SHUM_DBL_BOMB)
    #undef SHUM_DBL_BOMB
  #endif

  #if DBL_MANT_DIG != 53
    #define SHUM_DBL_BOMB
  #endif

  #if DBL_MAX_EXP != 1024
    #if !defined(SHUM_DBL_BOMB)
      #define SHUM_DBL_BOMB
    #endif
  #endif

  #if DBL_MIN_EXP != -1021
    #if !defined(SHUM_DBL_BOMB)
      #define SHUM_DBL_BOMB
    #endif
  #endif

  #if defined(SHUM_DBL_BOMB)
    #error the type "double" does not conform to IEC 60559 double format. This is a requirement for this code to function correctly.
  #endif

#endif
