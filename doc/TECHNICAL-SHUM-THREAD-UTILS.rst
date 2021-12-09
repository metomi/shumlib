Technical Reference: shum_thread_utils
++++++++++++++++++++++++++++++++++++++

Introduction
------------

The ``f_shum_thread_utils`` module is a Fortran ``MODULE``, designed to provide an abstracted interface to some OpenMP features.
However, despite being written in Fortran, it is not expected to be called directly *from* Fortran.
(In fact the ``f_shum_thread_utils`` module contains a ``PRIVATE`` statement, limiting the visibility of its routines within Fortran.)

Instead, the ``f_shum_thread_utils`` module should be called from C code. All of the routines within the shum_thread_utils module have an
``ISO_C_BINDING`` interface, and there is a corresponding header file (``c_shum_thread_utils.h``) containing the C prototypes.

The rationale for the ``f_shum_thread_utils`` module is as follows: there is no guarantee that runtime libraries used by an OpenMP
implementation are compatible between vendors or indeed, between different versions of an implementation from the same vendor.
This means that mixing code between different compiler vendors - or different versions of a compiler from the same vendor - can cause problems.
Normally, one can ensure that all of the code in a specific language is compiled from the same compiler. However, when mixing languages,
there are many cases when it is legitimate - or even required - to use compilers from different vendors. This is when problems may begin.

As an explicit example, it is known that mixing Fortran code compiled by the ``Intel ifort 12.0.4 20110427`` compiler
with C code compiled by the ``GNU GCC 6.3.0`` compiler on ``Linux x86`` systems is unsuccessful.
In this case, all the individual units compile correctly, but upon linking of the final executable binary a fatal error is met due to ABI
(Application Binary Interface) differences.

By utilising the ``f_shum_thread_utils`` module, we can ensure all the direct OpenMP references are contained within one language - in this case
Fortran, the primary programming language of most of the code utilising SHUMlib. This ensures that only one OpenMP runtime implementation is used, and
there can be no incompatibilities.

There is however some performance impact associated with utilising the ``f_shum_thread_utils`` module. Its use introduces extra function calls, which
in itself adds overhead. Additionally, the fact that Fortran function calls will be used from within C may inhibit some opitimisations -
inlining for example - as compilers tend to be less effective in applying these kind of optimisations across languages.
Generally though, in the problematic cases where the OpenMP implementation runtimes are incompatible, the additional performance gained
by being able to use OpenMP at all (albeit less efficiently than directly) greatly outweighs this overhead.

Because of these performance considerations, we want to avoid *always* using ``f_shum_thread_utils``. As a consequence, we should use preprocessing
in combination with an if-def to select ``f_shum_thread_utils`` code only in cases where we need it. Ideally, a native C OpenMP implementation should also be produced
(also protected by preprocessing if-def). In this way, the correct desired behaviour can be selected at compile-time, allowing one of
the following behaviours at runtime:

 * No OpenMP is used.
 * OpenMP is used through native C. (The compiler defines the [standard] ``_OPENMP`` pre-processing macro - through the nomal
   compiler switch selection process - which may have better performance, but be less portable)
 * OpenMP is used through ``f_shum_thread_utils``. (Which may be more portable but less performant than through native C. The
   compiler defines ``_OPENMP``; the user defines ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS``)

Additionally, there are many coding standards rules which must be followed when using the ``f_shum_thread_utils`` module to ensure the performance
impact is avoided, whilst continuing to maintain the highest possible portability of the code, and the ability to validate with automated testing.

*See also:* The thread_utils API documentation

Recommended Coding Practices
----------------------------

The following are a list of recomended coding practices when using OpenMP code with the shum_thread_utils module. These
have been designed to balance readability, and ease of programming, with correctness, performance, and the ability to automate testing.

**Protecting OpenMP in C Code**

OpenMP directives (``#pragma omp``) in C code must be protected by *both* a ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS`` and an ``_OPENMP`` if-def.
This ensures it is possible to select the use of only the Fortan OpenMP runtime library if required.
If possible, provide a Fortran implementation of the OpenMP parallelism as well, using the wrappers in the ``shum_thread_utils``.
An example of such use is given below.

::

  #if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)

   /* this branch uses the Fortran OpenMP runtime, via thread_utils */
   thread_utils_func();

  #elif defined(_OPENMP) && !defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)

   /* this branch uses OpenMP pragmas within C */
   #pragma omp parallel
   {
     omp_func();
   }

  #else

   /* this branch does not use OpenMP */
   serial_func();

  #endif

Ideally this should lead to code capable of providing all three possible runtime outcomes. You must *always* ensure that the no OpenMP case is possible.

**If-def tests**

If-def tests on ``_OPENMP`` & ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS`` must always occur as a pair.
You may not test the use of ``_OPENMP`` or ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS`` in isolation.

``_OPENMP`` must come first in any ``#if defined()`` pair.

**Restictions on if-def style**

If-def tests must not use the ``#ifdef``/``#ifndef`` style. Instead use ``#if defined()`` or  ``#if !defined()`` as appropriate.
This restriction is required to simplify the implementation of automated testing.

**Combining multiple if-def tests**

Any OpenMP if-def pair must not also include a logical test on a third macro. If this functionality is required, find an appropriate nesting of ``#if defined()`` tests.
For example instead of:

::

  #if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS) && defined(OTHER)
    /* do stuff */
  #endif

Use:

::

  #if defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)
  #if defined(OTHER)
    /* do stuff */
  #endif
  #endif

**Negative Logic if-defs**

You must not use negative logic in an if-def test on ``_OPENMP`` (i.e. ``#if !defined(_OPENMP)``).
Instead, use positive logic and an ``#else`` branch.
Use of negative logic is permitted for if-def tests on the accompanying ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS`` macro,
as this will be required to distinguish between cases using the C and Fortan OpenMP runtimes.

**Other Uses of the _OPENMP Macro**

The use of the ``_OPENMP`` preprocessor macro for code other than directives is permitted
(as long as it is in conjunction with ```SHUM_USE_C_OPENMP_VIA_THREAD_UTILS``).
This can be used equivalently to how the ``!$`` sentinel would be in Fortran.

**Derived Macros**

You cannot hide the use of the ``_OPENMP`` & ``SHUM_USE_C_OPENMP_VIA_THREAD_UTILS`` macros through the definition of a third macro dependent on them.
For example, you must not define and use a new macro in place of the two original macros, as shown here:

::

  #define USE_THREAD_UTILS defined(_OPENMP) && defined(SHUM_USE_C_OPENMP_VIA_THREAD_UTILS)

  #if defined(USE_THREAD_UTILS)
    thread_utils_func();
  #endif

Use of the thread_utils API (from C)
------------------------------------

This section will detail how to correctly use the shum_thread_utils module from within C code.

**Inclusion of the header**

To access the ``f_shum_thread_utils`` routines, the ``c_shum_thread_utils.h`` header must be included in your code as shown in the code example below.

::

  #include <stdio.h>
  #include <stdint.h>
  #include "c_shum_thread_utils.h"

  int main(void)
  {
    printf("thread ID: %" PRId64 "\n", threadID());
    return 0;
  }

This exposes the correct C prototypes corresponding to the Fortran implementation. There is no requirement to protect
the code with the ``_OPENMP`` pre-processing macro, as none of the OpenMP is exposed directly to the C.

However, one may choose to use protect the inclusion anyway, perhaps to allow preprocessing to switching between ``f_shum_thread_utils``,
direct OpenMP, and no OpenMP - as shown in the example below.

::

  #include <stdio.h>
  #include <stdint.h>

  #if defined(_OPENMP) && defined(SHUM_USE_OPENMP_VIA_THREADUTILS)
  #include "c_shum_thread_utils.h"
  #elif defined(_OPENMP) && !defined(SHUM_USE_OPENMP_VIA_THREADUTILS)
  #include <omp.h>
  #endif

  int main(void)
  {
    int tid;

  #if defined(_OPENMP) && defined(SHUM_USE_OPENMP_VIA_THREADUTILS)
    tid = (int)threadID();
  #elif defined(_OPENMP) && !defined(SHUM_USE_OPENMP_VIA_THREADUTILS)
    tid = omp_get_thread_num();
  #else
    tid = 1;
  #endif

    printf("thread ID: %d\n", tid);
    return 0;
  }

Note of course that this is a highly contrived example - if the OpenMP header were available it would be perfectly safe to include it in non-OpenMP builds.
Additionally, calls to find the thread number are safe regardless of whether OpenMP is enabled or not; and we have no parallel regions in this example anyway!
But it does serve to illustrate conceptually how a more complex case may work.