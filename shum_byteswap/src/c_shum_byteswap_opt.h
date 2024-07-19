#ifndef C_SHUM_BYTESWAP_OPT
#define C_SHUM_BYTESWAP_OPT

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
/*   This header contains all the macros and pre-processing keys to select    */
/*   the correct optimised version of the byteswapping code for your machine. */
/******************************************************************************/

/* -------------------------------------------------------------------------- */
/* bswap() family of macros                                                   */
/* -------------------------------------------------------------------------- */

/* Scalar Macros */

/*
 * There are several possible implementations of scalar bswap() we may use.
 *
 * i) If we are using a compiler which implements GNU GCC extensions
 *    (defines __GNUC__ >= 2), but is eariler than version 4.3, we can use the
 *    non-standard <byteswap.h> implementations, which may expand the macro to
 *    optimised inline assembly [dependant on architecture and version].
 *
 * ii) If we are using a compiler which implements GNU GCC extensions at version
 *     4.3 or later, but before version 4.8, we can use the the non-standard
 *     intrinsic __builtin_bswap*() except for bswap_16, which we implement
 *     directly as a bit-shift based macro.
 *
 * iii) If we are using a compiler which implements GNU GCC extensions at
 *      version 4.8 or later, we can use the the non-standard intrinsic
 *      __builtin_bswap*() functions for all sizes of bswap.
 *
 * iv) If we are using the Cray compiler (defines SHUM_IS_CRAY_COMPILER) with
 *     GNU extensions enabled (defines SHUM_HAS_GNU_EXTENSIONS): we can use GCC
 *     extensions, but not inline assembly. Therefore, for
 *     SHUM_HAS_GNU_EXTENSIONS support greater than version 4.3 but less than
 *     6.1, fall-back to the non-standard intrinsic __builtin_bswap*()
 *     functions, except for __builtin_bswap16, which isn't implemented, and
 *     so therefore use a directly implemented macro for bswap_16. (Note, some
 *     later versions of the Cray compiler support __builtin_bswap16, but there
 *     is no way to automatically differentiate these.)
 *
 * v) If none of the above are possible, implement the macros directly as
 *    bit-shift operations. This is much less efficient, but uses only
 *    standard C99, so will always work.
 *
 * vi) If the user defines one of the macros C_USE_BSWAP_BITSHIFT,
 *     C_USE_BSWAP_BUILTINS, C_USE_BSWAP_BYTESWAP_H, or
 *     C_USE_BSWAP_OSBYTEORDER_H override the above cases and use the direct
 *     bit-shift based macros, the __builtin_bswap*() non-standard intrinsic
 *     functions, the <byteswap.h> headers, or the <libkern/osbyteorder.h>
 *     headers repectively, as per that choice.
 *
 * Note: the following cases were added to support more systems at a later date
 *       and so are out of sequence. A user choice (case vi) will still override
 *       these defaults
 *
 * vii) If we are usining a version of the Cray compiler (defines
 *      SHUM_IS_CRAY_COMPILER) with GNU extensions enabled (defines
 *      SHUM_HAS_GNU_EXTENSIONS) with a version greater than 6.1, fall-back to
 *      the non-standard intrinsic __builtin_bswap*() functions for all data
 *      sizes.
 *
 * viii) If we are on a Mac OS X / Darwin system (defines __APPLE__) instead
 *       use the non-standard <libkern/osbyteorder.h> header
 */

#include "c_shum_compiler_select.h"

/*----------------*/
/* Control macros */
/*----------------*/

/* Feature test macros */
#if defined(SHUM_HAS_GNU_EXTENSIONS)

#define C_SHUM_BSWAP_HASGNU6_1 (SHUM_HAS_GNU_EXTENSIONS >= 60100)

#define C_SHUM_BSWAP_HASGNU4_8 ((SHUM_HAS_GNU_EXTENSIONS >= 40800)             \
                               && (SHUM_HAS_GNU_EXTENSIONS < 60100))

#define C_SHUM_BSWAP_HASGNU4_3 ((SHUM_HAS_GNU_EXTENSIONS >= 40300)             \
                               && (SHUM_HAS_GNU_EXTENSIONS < 40800))

#define C_SHUM_BSWAP_HASGNU2_0 ((SHUM_HAS_GNU_EXTENSIONS >= 20000)             \
                               && (SHUM_HAS_GNU_EXTENSIONS < 40300))

#else

#define C_SHUM_BSWAP_HASGNU6_1 0
#define C_SHUM_BSWAP_HASGNU4_8 0
#define C_SHUM_BSWAP_HASGNU4_3 0
#define C_SHUM_BSWAP_HASGNU2_0 0

#endif

/* ensure test macros are unset */
#undef C_USE_BSWAP_USERCHOICE
#undef C_USE_BSWAP_64_BITSHIFT
#undef C_USE_BSWAP_32_BITSHIFT
#undef C_USE_BSWAP_16_BITSHIFT
#undef C_USE_BSWAP_BUILTINS_64
#undef C_USE_BSWAP_BUILTINS_32
#undef C_USE_BSWAP_BUILTINS_16

/*------------*/
/* test cases */
/*------------*/

/* test for user choice */

#if defined(C_USE_BSWAP_BYTESWAP_H)

/* user choice is <byteswap.h> */
#define C_USE_BSWAP_USERCHOICE
#undef C_USE_BSWAP_OSBYTEORDER_H
#undef C_USE_BSWAP_BITSHIFT
#undef C_USE_BSWAP_BUILTINS

#elif defined(C_USE_BSWAP_OSBYTEORDER_H)

/* user choice is <libkern/OSByteOrder.h> */
#define C_USE_BSWAP_USERCHOICE
#undef C_USE_BSWAP_BYTESWAP_H
#undef C_USE_BSWAP_BITSHIFT
#undef C_USE_BSWAP_BUILTINS

#elif defined(C_USE_BSWAP_BUILTINS)

/* user choice is __builtin_bswap*() */
#define C_USE_BSWAP_USERCHOICE
#define C_USE_BSWAP_BUILTINS_64
#define C_USE_BSWAP_BUILTINS_32
#define C_USE_BSWAP_BUILTINS_16
#undef C_USE_BSWAP_OSBYTEORDER_H
#undef C_USE_BSWAP_BYTESWAP_H
#undef C_USE_BSWAP_BITSHIFT

#elif defined(C_USE_BSWAP_BITSHIFT)

/* user choice is bit-shift macros */
#define C_USE_BSWAP_USERCHOICE
#define C_USE_BSWAP_64_BITSHIFT
#define C_USE_BSWAP_32_BITSHIFT
#define C_USE_BSWAP_16_BITSHIFT
#undef C_USE_BSWAP_OSBYTEORDER_H
#undef C_USE_BSWAP_BYTESWAP_H
#undef C_USE_BSWAP_BUILTINS

#endif

#if !defined(C_USE_BSWAP_USERCHOICE)
#undef C_USE_BSWAP_OSBYTEORDER_H
#undef C_USE_BSWAP_BYTESWAP_H
#undef C_USE_BSWAP_BITSHIFT
#undef C_USE_BSWAP_BUILTINS
#endif

/* test for used case */

#if defined(C_USE_BSWAP_USERCHOICE)

/* case vi) */

#elif defined(__APPLE__)

/* case viii) */
#define C_USE_BSWAP_OSBYTEORDER_H

#elif C_SHUM_BSWAP_HASGNU2_0

/* case i) */
#define C_USE_BSWAP_BYTESWAP_H

#elif C_SHUM_BSWAP_HASGNU6_1 && defined(SHUM_IS_CRAY_COMPILER)

/* case vii) */
#define C_USE_BSWAP_BUILTINS_64
#define C_USE_BSWAP_BUILTINS_32
#define C_USE_BSWAP_BUILTINS_16

#elif C_SHUM_BSWAP_HASGNU4_3 && defined(SHUM_IS_CRAY_COMPILER)

/* case iv) */
#define C_USE_BSWAP_BUILTINS_64
#define C_USE_BSWAP_BUILTINS_32
#define C_USE_BSWAP_16_BITSHIFT

#elif C_SHUM_BSWAP_HASGNU4_8

/* case iii) */
#define C_USE_BSWAP_BUILTINS_64
#define C_USE_BSWAP_BUILTINS_32
#define C_USE_BSWAP_BUILTINS_16

#elif C_SHUM_BSWAP_HASGNU4_3

/* case ii) */
#define C_USE_BSWAP_BUILTINS_64
#define C_USE_BSWAP_BUILTINS_32
#define C_USE_BSWAP_16_BITSHIFT

#else

/* case v) */
#define C_USE_BSWAP_64_BITSHIFT
#define C_USE_BSWAP_32_BITSHIFT
#define C_USE_BSWAP_16_BITSHIFT

#endif

/*-----------------------*/
/* Implementation macros */
/*-----------------------*/

#if defined(C_USE_BSWAP_BUILTINS_64)
#define bswap_64(x) __builtin_bswap64(x)
#endif

#if defined(C_USE_BSWAP_BUILTINS_32)
#define bswap_32(x) __builtin_bswap32(x)
#endif

#if defined(C_USE_BSWAP_BUILTINS_16)
#define bswap_16(x) __builtin_bswap16(x)
#endif

#if defined(C_USE_BSWAP_BYTESWAP_H)
#include <byteswap.h>
#endif

#if defined(C_USE_BSWAP_OSBYTEORDER_H)
#include <libkern/OSByteOrder.h>
#define bswap_16(x) OSSwapInt16(x)
#define bswap_32(x) OSSwapInt32(x)
#define bswap_64(x) OSSwapInt64(x)
#endif


#if defined(C_USE_BSWAP_64_BITSHIFT)

#define bswap_64(x) \
               ((((x) & UINT64_C(0xff00000000000000)) >> 56) \
              | (((x) & UINT64_C(0x00ff000000000000)) >> 40) \
              | (((x) & UINT64_C(0x0000ff0000000000)) >> 24) \
              | (((x) & UINT64_C(0x000000ff00000000)) >> 8)  \
              | (((x) & UINT64_C(0x00000000ff000000)) << 8)  \
              | (((x) & UINT64_C(0x0000000000ff0000)) << 24) \
              | (((x) & UINT64_C(0x000000000000ff00)) << 40) \
              | (((x) & UINT64_C(0x00000000000000ff)) << 56))

#endif

#if defined(C_USE_BSWAP_32_BITSHIFT)

#define bswap_32(x) \
               ((((x) & UINT32_C(0xff000000)) >> 24) \
              | (((x) & UINT32_C(0x00ff0000)) >>  8) \
              | (((x) & UINT32_C(0x0000ff00)) <<  8) \
              | (((x) & UINT32_C(0x000000ff)) << 24))

#endif

#if defined(C_USE_BSWAP_16_BITSHIFT)

#define bswap_16(x) \
               ((((x) & UINT16_C(0xff00)) >>  8) \
              | (((x) & UINT16_C(0x00ff)) <<  8))

#endif

/* -------------------------------------------------------------------------- */
/* End bswap() family of macros                                               */
/* -------------------------------------------------------------------------- */

#endif
