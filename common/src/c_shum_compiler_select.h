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
/* Global macros for compiler detection                                       */
/*                                                                            */
/* Information:                                                               */
/*                                                                            */
/* Header file providing definitions of macros to test compiler versions and  */
/* compatibility.                                                             */
/*                                                                            */
/******************************************************************************/

#if !defined(C_SHUM_COMPILER_SELECT_H)
#define C_SHUM_COMPILER_SELECT_H

/******************************************************************************/

/* detect GNU compiler */

#if defined(__GNUC__)
#if defined(__GNUC_PATCHLEVEL__)
#define SHUM_IS_GNU_COMPILER (__GNUC__ * 10000                                 \
                             + __GNUC_MINOR__ * 100                            \
                             + __GNUC_PATCHLEVEL__)
#define SHUM_HAS_GNU_EXTENSIONS (__GNUC__ * 10000                              \
                                + __GNUC_MINOR__ * 100                         \
                                + __GNUC_PATCHLEVEL__)
#else
#define SHUM_IS_GNU_COMPILER (__GNUC__ * 10000                                 \
                             + __GNUC_MINOR__ * 100)
#define SHUM_HAS_GNU_EXTENSIONS (__GNUC__ * 10000                              \
                                + __GNUC_MINOR__ * 100)
#endif
#endif

/******************************************************************************/

/* detect Clang compiler */

#if defined(__clang__)
#define SHUM_IS_CLANG_COMPILER (__clang_major__ * 10000                        \
                               + __clang_minor__ * 100                         \
                               + __clang_patchlevel__)
#define SHUM_HAS_CLANG_EXTENSIONS (__clang_major__ * 10000                     \
                                  + __clang_minor__ * 100                      \
                                  + __clang_patchlevel__)
#endif

/******************************************************************************/

/* detect Intel compiler */

#if defined(__INTEL_COMPILER)
#if (__INTEL_COMPILER < 2021) || (__INTEL_COMPILER == 202110)                  \
                             || (__INTEL_COMPILER == 202111)
#if defined(__INTEL_COMPILER_UPDATE)
#define SHUM_IS_INTEL_COMPILER ((__INTEL_COMPILER/100) * 10000                 \
                               + (__INTEL_COMPILER/10 % 10) * 100              \
                               + __INTEL_COMPILER_UPDATE)
#else
#define SHUM_IS_INTEL_COMPILER ((__INTEL_COMPILER/100) * 10000                 \
                               + (__INTEL_COMPILER/10 % 10) * 100              \
                               + (__INTEL_COMPILER % 10))
#endif
#else
#define SHUM_IS_INTEL_COMPILER (__INTEL_COMPILER * 10000 
                               + __INTEL_COMPILER_UPDATE * 100)
#endif
#endif


#if defined(__INTEL_LLVM_COMPILER)
#if __INTEL_LLVM_COMPILER < 1000000L
#define SHUM_IS_INTEL_COMPILER ((__INTEL_LLVM_COMPILER/100) * 10000            \
                                + (__INTEL_LLVM_COMPILER/10 % 10) * 100        \
                                +(__INTEL_LLVM_COMPILER % 10))
#else
#define SHUM_IS_INTEL_COMPILER ((__INTEL_LLVM_COMPILER/10000) * 10000          \
                                + (__INTEL_LLVM_COMPILER/100 % 100) * 100      \
                                +(__INTEL_LLVM_COMPILER % 100))
#endif
#endif

/******************************************************************************/

/* detect Cray compiler */

#if defined(_CRAYC)
#if defined(_RELEASE_PATCHLEVEL)
#define SHUM_IS_CRAY_COMPILER (_RELEASE_MAJOR * 10000                          \
                              + _RELEASE_MINOR * 100                           \
                              + _RELEASE_PATCHLEVEL)
#else
#define SHUM_IS_CRAY_COMPILER (_RELEASE_MAJOR * 10000                          \
                              + _RELEASE_MINOR * 100)
#endif
#endif

#if defined(__cray__)
#define SHUM_IS_CRAY_COMPILER (__clang_major__ * 10000                         \
                              + __clang_minor__ * 100                          \
                              + __clang_patchlevel__)
#endif

/******************************************************************************/

/* Remove GNU false-positives */

#if defined(SHUM_IS_GNU_COMPILER)
#if defined(SHUM_IS_CRAY_COMPILER) || defined(SHUM_IS_INTEL_COMPILER)          \
    || defined(SHUM_IS_CLANG_COMPILER)
#undef SHUM_IS_GNU_COMPILER
#endif
#endif

/******************************************************************************/

/* Remove Clang false-positives */

#if defined(SHUM_IS_CLANG_COMPILER)
#if defined(SHUM_IS_CRAY_COMPILER) || defined(SHUM_IS_INTEL_COMPILER)
#undef SHUM_IS_CLANG_COMPILER
#endif
#endif

/******************************************************************************/

#endif
