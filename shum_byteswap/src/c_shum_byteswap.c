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
/*   Portable byteswapping routines; These allow the checking of              */
/*   machine endianism, and accordingly, the changing of file                 */
/*   endianism.                                                               */
/******************************************************************************/

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <inttypes.h>
#include "c_shum_byteswap.h"
#include "c_shum_byteswap_opt.h"

#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
#include "c_shum_thread_utils.h"
#endif

/* -------------------------------------------------------------------------- */

/* We want the compiler to inline the par_swap??() family in the
 * non-"OpenMP via shum_thread_utils" case for performance reasons.
 * However, in this case, the par_swap??() family must exist as seperate
 * routines in order to create the required function pointers.
 * Therefore, create an OpenMP and SHUM_USE_C_OPENMP_VIA_THREAD_UTILS dependent
 * macro to expand to "static inline" qualifiers only in the OpenMP via
 * thread_utils case.
 */

#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
#define INLINEQUAL
#else
#define INLINEQUAL static inline
#endif

/* -------------------------------------------------------------------------- */
/* Prototypes and Typedefs                                                    */
/* -------------------------------------------------------------------------- */

/* Prototypes */

/* Note: the first argument to these would more appropriately be (void *).
 * However, due to a bug, the Intel compiler does not recognise the VALUE
 * attribute in Fortran ABSTRACT INTERFACEs.
 * We must therefore use (void **) and pass-by-reference on the Fortran side.
 */
INLINEQUAL void c_shum_byteswap_par_swap64(void **const,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict);

INLINEQUAL void c_shum_byteswap_par_swap32(void **const,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict);

INLINEQUAL void c_shum_byteswap_par_swap16(void **const,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict,
                                           const int64_t *const restrict);

/* -------------------------------------------------------------------------- */
/* Procedure implentations                                                    */
/* -------------------------------------------------------------------------- */

int64_t c_shum_byteswap(void *array, int64_t len, int64_t word_len,
                        char *message, int64_t message_len)
{
  int64_t status = 0;
  const int64_t imin = 0;
  const int64_t imax = len-1;
  const int64_t incr = 1;

  size_t message_len_t = (size_t) message_len;

#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
  /* intermediate restrict pointers are not needed for the
   * thread utils case.
   */
#else
  const int64_t *const restrict incr_ptr=&incr;
  const int64_t *const restrict imin_ptr=&imin;
  const int64_t *const restrict imax_ptr=&imax;
#endif

#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
#if defined(_CRAYC)
  #pragma _CRI inline_always c_shum_byteswap_par_swap64
  #pragma _CRI inline_always c_shum_byteswap_par_swap32
  #pragma _CRI inline_always c_shum_byteswap_par_swap16
#endif
#endif

  if (word_len == 8)
  {
#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    f_shum_startOMPparallelfor(&array, c_shum_byteswap_par_swap64, &imin,
                               &imax, &incr);
#else
    c_shum_byteswap_par_swap64(&array,imin_ptr,imax_ptr,incr_ptr);
#endif
  }
  else if (word_len == 4)
  {
#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    f_shum_startOMPparallelfor(&array, c_shum_byteswap_par_swap32, &imin,
                               &imax, &incr);
#else
    c_shum_byteswap_par_swap32(&array,imin_ptr,imax_ptr,incr_ptr);
#endif
  }
  else if (word_len == 2)
  {
#if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    f_shum_startOMPparallelfor(&array, c_shum_byteswap_par_swap16, &imin,
                               &imax, &incr);
#else
    c_shum_byteswap_par_swap16(&array,imin_ptr,imax_ptr,incr_ptr);
#endif
  }
  else
  {
    status = 1;
    snprintf(message, message_len_t,
             "c_shum_byteswap: Word length of %" PRId64 " bytes not allowed. "
             "Supported word lengths are 2, 4 or 8 bytes",
             word_len);
  }

  return status;
}

/* -------------------------------------------------------------------------- */

INLINEQUAL void c_shum_byteswap_par_swap64(void **const array,
                                           const int64_t * const restrict imin,
                                           const int64_t * const restrict imax,
                                           const int64_t * const restrict incr)
{
  {
    uint64_t *const restrict ptr_64 = (uint64_t *)*array;
    int64_t i;
    const int64_t span=(*imax-*imin)/(*incr);
    const int64_t min=*imin;
    const int64_t cincr=*incr;

#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    #pragma omp parallel for default(shared) private(i)
#endif
    for (i=0; i<=span; i=i+1)
    {
      ptr_64[min+i*cincr] = (uint64_t)bswap_64(ptr_64[min+i*cincr]);
    }
  }
}

/* -------------------------------------------------------------------------- */

INLINEQUAL void c_shum_byteswap_par_swap32(void **const array,
                                           const int64_t *const restrict imin,
                                           const int64_t *const restrict imax,
                                           const int64_t *const restrict incr)
{
  {
    uint32_t *const restrict ptr_32 = (uint32_t *)*array;
    int64_t i;
    const int64_t span=(*imax-*imin)/(*incr);
    const int64_t min=*imin;
    const int64_t cincr=*incr;

#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    #pragma omp parallel for default(shared) private(i)
#endif
    for (i=0; i<=span; i=i+1)
    {
      ptr_32[min+i*cincr] = (uint32_t)bswap_32(ptr_32[min+i*cincr]);
    }
  }
}

/* -------------------------------------------------------------------------- */

INLINEQUAL void c_shum_byteswap_par_swap16(void **const array,
                                           const int64_t *const restrict imin,
                                           const int64_t *const restrict imax,
                                           const int64_t *const restrict incr)
{
  {
    uint16_t *const restrict ptr_16 = (uint16_t *)*array;
    int64_t i;
    const int64_t span=(*imax-*imin)/(*incr);
    const int64_t min=*imin;
    const int64_t cincr=*incr;

#if defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
    #pragma omp parallel for default(shared) private(i)
#endif
    for (i=0; i<=span; i=i+1)
    {
      ptr_16[min+i*cincr] = (uint16_t)bswap_16(ptr_16[min+i*cincr]);
    }
  }
}

/* -------------------------------------------------------------------------- */

endianness c_shum_get_machine_endianism(void)
{
  endianness machine_endian;

  union {
    uint32_t thirtytwo_bit_int;
    uint8_t  four_byte_array[4];
  } byteorder={UINT32_C(0x01020304)};

  if (byteorder.four_byte_array[0]==1)
  {
    machine_endian=bigEndian;
  }
  else
  {
    machine_endian=littleEndian;
  }

  return machine_endian;
}

/* -------------------------------------------------------------------------- */
