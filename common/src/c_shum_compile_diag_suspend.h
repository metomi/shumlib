#if !defined(SHUM_COMPILE_DIAG_SUSPEND_H)

/**********************************COPYRIGHT***********************************/
/*            (C) Crown copyright Met Office. All rights reserved.            */
/*         For further details please refer to the file COPYRIGHT.txt         */
/*        which you should have received as part of this distribution.        */
/**********************************COPYRIGHT***********************************/
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
/* This header defines macros to turn off compile-time checking in a compiler */
/* agnostic way.                                                              */
/*                                                                            */
/* The macros are:                                                            */
/*    SHUM_COMPILE_DIAG_GLOBAL_SUSPEND(flag)                                  */
/*    SHUM_COMPILE_DIAG_SUSPEND(flag)                                         */
/*    SHUM_COMPILE_DIAG_RESUME                                                */
/*                                                                            */
/* SHUM_COMPILE_DIAG_GLOBAL_SUSPEND() is used to suspend a checking flag      */
/* globally if the compiler does not support scoped suspension. This must be  */
/* called outside of a function body. "flag" is the compiler flag to suspend. */
/*                                                                            */
/* SHUM_COMPILE_DIAG_SUSPEND() is used to suspend a checking flag in a scoped */
/* region. "flag" is the compiler flag to suspend. SHUM_COMPILE_DIAG_RESUME   */
/* is to end the scoped region and resume compiler checking. These may be     */
/* used anywhere in the code, but pairs must be strictly nested.              */
/******************************************************************************/

#define SHUM_COMPILE_DIAG_SUSPEND_H

#define SHUM_EXEC_PRAGMA(pragma_string) _Pragma(#pragma_string)
#define SHUM_EXEC_PRAGMA_STRINGIFY(string) #string

/* Deal with GCC */

#if defined(__GNUC__) && !defined(__clang__)

/* we are using GCC */

#if (__GNUC__ < 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ < 8))

/* older versions of GCC (<4.8) cannot suspend checking, so we must turn it off
 * completely. Call SHUM_COMPILE_DIAG_GLOBAL_SUSPEND from the top of the unit,
 * after including this header.
 */
#define SHUM_COMPILE_DIAG_GLOBAL_SUSPEND(flag)                                 \
  SHUM_EXEC_PRAGMA(message SHUM_EXEC_PRAGMA_STRINGIFY(GCC version too low)     \
                   SHUM_EXEC_PRAGMA_STRINGIFY(to modify diagnostic checking))  \
  SHUM_EXEC_PRAGMA(                                                            \
  message SHUM_EXEC_PRAGMA_STRINGIFY(flag permanently turned off !))           \
  SHUM_EXEC_PRAGMA(GCC diagnostic ignored #flag)

#else

/* newer versions of GCC can suspend checking, so we do that as it is preferable
 * to continue to check other code in the same file.
 */
#define SHUM_COMPILE_DIAG_SUSPEND(flag)                                        \
  _Pragma("GCC diagnostic push") SHUM_EXEC_PRAGMA(GCC diagnostic ignored #flag)

#define SHUM_COMPILE_DIAG_RESUME _Pragma("GCC diagnostic pop")

#endif

#endif

/* Deal with Clang */

#if defined(__clang__)

/* we are using Clang - which supports suspending checking */

#define SHUM_COMPILE_DIAG_SUSPEND(flag)                                        \
  _Pragma("clang diagnostic push")                                             \
  SHUM_EXEC_PRAGMA(clang diagnostic ignored #flag)

#define SHUM_COMPILE_DIAG_RESUME _Pragma("clang diagnostic pop")

#endif

/* Deal with other compilers */

/* if we have not specifically set the macros yet, set them blank */

#if !defined(SHUM_COMPILE_DIAG_SUSPEND)
#define SHUM_COMPILE_DIAG_SUSPEND(flag)
#endif

#if !defined(SHUM_COMPILE_DIAG_RESUME)
#define SHUM_COMPILE_DIAG_RESUME
#endif

#if !defined(SHUM_COMPILE_DIAG_GLOBAL_SUSPEND)
#define SHUM_COMPILE_DIAG_GLOBAL_SUSPEND(flag)
#endif

#endif
