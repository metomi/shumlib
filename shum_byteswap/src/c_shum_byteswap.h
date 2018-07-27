#ifndef C_BYTESWAP
#define C_BYTESWAP

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

#include <stdint.h>

/* -------------------------------------------------------------------------- */
/* Prototypes                                                                 */
/* -------------------------------------------------------------------------- */

typedef enum ENDIANNESS { bigEndian, littleEndian, numEndians } endianness;

extern int64_t c_shum_byteswap(void *array, int64_t len, int64_t word_len, 
                               char *message, int64_t message_len);
extern endianness c_shum_get_machine_endianism(void);

#endif
