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

/* -------------------------------------------------------------------------- */
/* Procedure implentations                                                    */
/* -------------------------------------------------------------------------- */

int64_t c_shum_byteswap(void *array, int64_t len, int64_t word_len, 
                        char *message, int64_t message_len)
{

  size_t message_len_t = (size_t) message_len;

  int64_t status = 0;

  size_t i;

  if (word_len == 8)
  {
    uint64_t *ptr_64;

    ptr_64 = (uint64_t *)array;

#pragma omp parallel for default(none) private(i) shared(len, ptr_64)
    for (i=0 ; i < (size_t)len ; i++)
    {
      ptr_64[i] = (uint64_t)bswap_64(ptr_64[i]);
    }
  }
  else if (word_len == 4)
  {
    uint32_t *ptr_32;

    ptr_32 = (uint32_t *)array;

#pragma omp parallel for default(none) private(i) shared(len, ptr_32)
    for (i=0 ; i < (size_t)len ; i++)
    {
      ptr_32[i] = (uint32_t)bswap_32(ptr_32[i]);
    }

  }
  else if (word_len == 2)
  {
    uint16_t *ptr_16;

    ptr_16 = (uint16_t *)array;

#pragma omp parallel for default(none) private(i) shared(len, ptr_16)
    for (i=0 ; i < (size_t)len ; i++)
    {
      ptr_16[i] = (uint16_t)bswap_16(ptr_16[i]);
    }

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
