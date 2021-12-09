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
/*                                                                            */
/* Global macros for word sizes                                               */
/*                                                                            */
/* Information:                                                               */
/*                                                                            */
/* Header file providing definitions of byte-sizes of words from their        */
/* equivalent bit-size. Used to avoid magic numbers in code.                  */
/*                                                                            */
/******************************************************************************/

#ifndef C_SHUM_WORD_BYTES_H
#define C_SHUM_WORD_BYTES_H

/* Number of bytes in a word of a specific bit size */
#define WORD8BYTES    1 /*   8 bit */
#define WORD16BYTES   2 /*  16 bit */
#define WORD32BYTES   4 /*  32 bit */
#define WORD64BYTES   8 /*  64 bit */
#define WORD128BYTES 16 /* 128 bit */
#define WORD256BYTES 32 /* 256 bit */
#define WORD512BYTES 64 /* 512 bit */

#endif
