Developer's Guide
-----------------

This part of the documentation is intended for developers of Shumlib itself, and
goes into more detail than is required simply to *use* the libraries.

Library Code/Standards
%%%%%%%%%%%%%%%%%%%%%%

The code making up Shumlib libraries is standard Fortran (2003) and/or C (C99)
code, performing a relatively simple task (or possibly a set of multiple related
tasks). Although the exact implementation is up to the author there are a few
conventions which are kept across all libraries:

Variable Precision
''''''''''''''''''

Ideally a library from Shumlib should be able to be included in as many
applications as possible, which means avoiding making assumptions about the
native precisions of variables. This means that *all* variables should have
their precision explicitly defined (i.e. do not allow the compiler to determine
precision of native variables).

Since the libraries can contain a mixture of Fortran and C the usage of the
``ISO_C_BINDING`` module in Fortran is important and allows us to mandate that
the C types are used to define the precisions. This decision is rooted in the
fact that (in the 2003 standard) Fortran still lacks a consistent way of
defining the precision of ``REAL`` variables. Therefore we defer responsibility
to the C variables exposed by ``ISO_C_BINDING``, but to ease the transition
someday to Fortran 2008 this is done via intermediate variables. All Fortran
Shumlib modules should therefore contain these definitions:

.. parsed-literal::

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
      C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

    INTEGER, PARAMETER :: int64  = C_INT64_T
    INTEGER, PARAMETER :: int32  = C_INT32_T
    INTEGER, PARAMETER :: real64 = C_DOUBLE
    INTEGER, PARAMETER :: real32 = C_FLOAT

And then *all* ``REAL`` and ``INTEGER`` variables should have their ``KIND`` set
to one of these. An obvious question might be whether you can actually rely on
``C_FLOAT`` and ``C_DOUBLE`` mapping onto a 32-bit and 64-bit ``REAL``
respectively.  This is a valid concern and in order to safeguard against cases
where this might not be true a mechanism exists in Shumlib which is referred to
as the "Precision Bomb" (which is covered in a later section of this document).

It is recommended as part of final verification of Shumlib code (where Fortran
code is included) that one tests running with the compiler options that will
override the native precision of variables. If everything is explicitly set as
described above this should not have any effect on the testing results.

Handling Errors
'''''''''''''''

Error handling in libraries can be tricky; it's undesirable to have the code in
the library directly abort because the application using the library might have
special requirements or tasks that need to be performed as part of a controlled
exit. Although there are a few ways to work around this (callbacks etc.) it's
simpler to avoid the issue entirely and pass information back to the calling
code for it to handle.

As is common practice an ``INTEGER`` error code should be returned by each
exposed function (or returned via an argument if a ``SUBROUTINE``), which has a
value of ``0`` for success and ``> 0`` for failure (though the exact value is up
to the author). Ideally the library functions/routines should also pass back
some sort of message which contains information about the error. The
``shum_string_conv`` library is available to help with this in the case that
Fortran code needs to pass or receive ``CHARACTER`` variables from  C code.


Build System
%%%%%%%%%%%%

This section elaborates on the overview of how to use the build system detailed
in ``INSTALL.rst`` and so assumes you are familiar with how the build system is
invoked and its results.

The Shumlib build system consists of a system of recursive makefiles. What
follows is a brief overview of the tasks each class of makefile performs, in
roughly the order they are processed when invoking a build:

    **Platform specific makefile fragments**
        These are the individual ``.mk`` files contained in the ``make``
        directory. Non-developer users should only ever need to interact with
        these files to fully configure Shumlib. They are heavily commented and
        hopefully self explanatory. After setting up its variables each of these
        then invokes the top-level makefile.

    **Top-level makefile**
        Provides the main control structures for the build; handling things like
        the dependencies between different libraries, sets up some key variables
        and the handing-off to the individual library makefiles with global
        targets like ``check`` or ``clean``) (finer detail in a section below).

    **Library makefiles**
        Each library (directory beginning with ``shum_``) contains a ``src``
        directory for the library source code, and a makefile which is set off
        by the top-level makefile above if the given library is to be
        built. These makefiles store the compilation commands for the libraries
        themselves and ultimately place them into the final output directory
        (finer detail in a section below).

    **Testing makefiles**
        In parallel to the ``src`` directory, each library directory
        (``shum_*``) also contains a ``test`` directory, containing additional
        code required to produce unit tests for the associated
        library. The Fortran and C interfaces for the library are tested, via
        the "FRUIT" testing framework (more on this below).

    **FRUIT and FRUIT driver makefile**
        As part of the testing the FRUIT testing framework is built (also into a
        library), and a top-level "driver" Fortran program is generated to allow
        test routines to perform all of the tests. Two makefiles exist for these
        purposes in the ``fruit`` directory (for more details see the FRUIT
        testing section). These are invoked by the top-level makefile when
        required.

    **Common/Version makefile**
        Finally a makefile exists to produce some additional objects that
        hard-code the Shumlib version number into each library. This file is
        included within the makefiles of each library to generate the
        appropriate code (for more details see below).


Structure of the top-level makefile
'''''''''''''''''''''''''''''''''''

For the most part the best sources of information about what the main makefile
does are the inline comments. However a few details not explicitly mentioned or
benefiting from additional information will be discussed here.

Note in the "Libraries" section, each library has a main library variable listed. This is
assigned from a human-readable name for the library, which should match the name of the
top level directory under ``${DIR_ROOT}`` containing that library's code directories
(the ``src`` or ``test`` directories). Following this, there is another variable -
the prerequisites variable - which is formed of the main library variable with the suffix
``_PREREQ`` appended. This is assigned a space separated list of the main variables for
other libraries upon which this library depends. Finally, there is the ``ALL_LIBS_VARS``
variable, which is composed of a space separated list of all the main library variables
defined in the makefile.

These variables are used to auto-generate the library make targets (so you are
able to type ``make -f <configuration> library_name``). For example
one can see the following definition for the Fieldsfile Class library:

.. parsed-literal::

    FFCLASS=shum_fieldsfile_class
    FFCLASS_PREREQ=FFILE PACK

Notice that the dependencies for the ``${FFCLASS}`` variable include the variables
``${FFILE}`` and ``${PACK}``. This will be used to auto-generate the make target
``shum_fieldsfile_class`` that will build code from the
``${DIR_ROOT}/shum_fieldsfile_class/src`` directory, and which will automatically
depend on the ``shum_fieldsfile`` and ``shum_wgdos_packing`` targets. By setting it up this
way the Fieldsfile API and WGDOS Packing libraries (corresponding to ``${FFILE}``
and ``${PACK}`` respectively) will *always* be built before the Fieldsfile Class if required.

Libraries for which tests are defined will also auto-generate a second build
target (the library name appended with ``_tests``) that depends on both
``fruit`` (the target for the FRUIT testing framework itself) and the main
library (to ensure the library is always recompiled if needed before tests are
run).

| So in our example, the ``shum_fieldsfile_class_tests`` target would first build the following targets
  as prerequisite dependencies:
|  ``fruit``
|  ``shum_fieldsfile``
|  ``shum_wgdos_packing``
|  ``shum_fieldsfile_test``
|  ``shum_wgdos_packing_test``
|  ``shum_fieldsfile_class``

There are two further special auto-generated targets for each library:

i) A target to build only that library's prerequisites (the library name appended with ``_prereq``)

ii) A target to build only that library's prerequisites and their tests (the library name appended with ``_prereq_test``)

| In our example, these are the ``shum_fieldsfile_class_prereq`` target, which is equivalent to:
|  ``shum_fieldsfile``
|  ``shum_wgdos_packing``

| And the the ``shum_fieldsfile_class_prereq_test`` target, which is equivalent to:
|  ``fruit``
|  ``shum_fieldsfile_class_prereq``
|  ``shum_fieldsfile_test``
|  ``shum_wgdos_packing_test``

At any point following the building of one or more library, the generic ``test`` target can be built.
This will compile and run the fruit driver and any tests using only the libraries which have already been built.
If tests have already been built, and you only wish to execute them, you can use the ``run_tests`` target.
(Note that running either of these targets without first having built at least one library or test will result in
errors.)

There are a few more generic targets which apply to all available libraries. These are:

i) The ``all_libs`` target builds all the available libraries (and dependencies) as required. Note that this is the default target if none is explicitly given.

ii) The ``all_tests`` target builds all the available libraries (and dependencies) as required.

iii) The ``check`` target is equivalent to ``all_libs`` followed by ``all_tests`` and ``run_tests`` (ie. build all libraries, then build and run all available tests).

Finally, there are two targets for cleaning the build structure.

i) ``clean`` to completely remove *all* build output including the produced libraries and test executables.

ii) ``clean-temps`` to *only* remove intermediate files but leave the build output itself in place.

Most of the actual build instructions in this file simply spawn sub-make
commands located in the required directories (the ``src`` or ``test``
directories of a library, or special directories such as that of the FRUIT
testing framework)


Structure of library makefiles
''''''''''''''''''''''''''''''

In much the same way as the top-level file, these are fairly well commented with
basic information about what each part is doing. Although these files could vary
between different libraries they follow a consistent pattern.

Each of these makefiles should setup the dynamic and static library targets, and
include the version and precision bomb information by including the file
``Makefile-version`` (note that ``VERSION_LIBNAME`` must be set to the name of
the library for this to work - the version inclusion mechanism is covered in
more detail in a later section of this document).

Following this should be build instructions to build the object files from the
source (making use of the platform defined variables from the configuration
file). Note that the ``PIC`` (Position Independent Code) distinction is fairly
important; shared/dynamic libraries should enable this flag for
portability. Therefore every object file produced in these makefiles is repeated
twice (once with and once without the ``PIC`` flag, and using a different naming
scheme: appending ``_PIC`` before the extension).

*If* the library has dependencies on any of the other Shumlib libraries, the
commands that compile each object should specify the output include directory
(i.e. with ``-I${LIBDIR_OUT}/include``) so that any headers (or "mod" files)
are picked up correctly.

Structure of the testing makefiles
''''''''''''''''''''''''''''''''''

Testing is defined for Fortran using the FRUIT framework, so libraries that
define Fortran unit tests have them built from this makefile using this approach.
In addition this makefile may also build C unit tests, which are driven by FRUIT
from Fortran using ``IOS_C_BINDING`` interfaces.

Note that this file doesn't build *executables*, only the object files. See the
section on FRUIT testing for details of how these are used to produce the final
testing code.


FRUIT Testing
%%%%%%%%%%%%%

FRUIT is an externally developed Fortran testing framework which has been
reproduced and modified to form the basis for testing in Shumlib. It was chosen
for its fairly simple nature - the entire framework consists of a single
Fortran file (plus one extension file if one wishes to test MPI code).

Due to the need for a lot of Fortran "boilerplate" code in the many overloaded
interfaces required for a testing framework, the developers of FRUIT opted for
*generated* source code. Therefore you should *not* edit the ``fruit/fruit.f90``
file directly (should it require any modifications). The file is generated from
a template in the ``fruit/fruit_f90_source.txt`` file and the Ruby script
``fruit/fruit_f90_generator.rb`` (providing the ``txt`` file as the sole
argument).

In order to keep track of exactly what modifications were made to the original
FRUIT files, copies of those files have been preserved in Shumlib, appended with
the FRUIT version number from which they were copied. This allows easy use of
any "diff" tool to examine the changes directly, but in summary the
modifications made were:

    - The generator and template were modified to name the types in accordance
      with the explicit types used by Shumlib (see the earlier section on
      Library Code/Standards).

    - The template was updated to import and define the type parameters in the
      same way as the Shumlib libraries (again see the earlier section).

    - Testing of ``COMPLEX`` variables was removed completely.

The framework additionally requires a ``driver`` file is provided, holding the
actual code to be tested as a main Fortran program. Since Shumlib consists of
several small libraries, not all of which may be compiled at the time of
testing, we use some automation and source code generation to produce the driver
file. First however each library must define its testing.


Defining FRUIT tests for each library
'''''''''''''''''''''''''''''''''''''

Within each (tested) library's ``test`` directory there should be a Fortran file
which provides its testing. This file may implement Fortran unit tests, act as a
driver for C unit tests using ``ISO_C_BINDING`` interfaces, or both. If the
Fortran file is used as a driver for C tests, there must additionally be a
corresponding C file containing those tests. In order to work correctly with the
makefile that constructs the FRUIT driver file these files *must* obey certain
conventions:

    - The Fortran filename must be ``fruit_test_<library_name>.f90`` where
      ``<library_name>`` is the exact library name as it appears as the
      directory name of each library.

    - The C filename (where it exists) must be ``c_fruit_test_<library_name>.c``

    - The object files produced from the above file(s) by the makefile in the
      library ``test`` directory must similarly be called
      ``fruit_test_<library_name>.o``, ``fruit_test_<library_name>_PIC.o``,
      and where applicable ``c_fruit_test_<library_name>.o``, and
      ``c_fruit_test_<library_name>_PIC.o``,

    - The *module name* within the Fortran file must be
      ``fruit_test_<library_name>_mod``.

    - The Fortran module must be defaulted to ``PRIVATE`` except for a single
      exposed (``PUBLIC``) subroutine which accepts *no arguments* and is named
      ``fruit_test_<library_name>``.

    - The Fortran module should include the library created for the FRUIT
      framework itself by specifying ``USE fruit``.

    - The code in the module should, just like the libraries themselves,
      explicitly define precisions for all variables (using the same method
      described in the earlier sections).

Following these basic requirements are the layout for the main routine
mentioned above. FRUIT expects each test to be provided as an argument-less
``SUBROUTINE`` passed to a generic ``run_test_case`` routine which it
defines. With this in mind, and for consistency, the recommended layout of this
routine is as follows.

Firstly, have the routine report the Shumlib version and its name, details of
where this module comes from can be found in the later "Version Inclusion"
section. 

.. parsed-literal::

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
    USE f_<library_name>_version_mod, ONLY: get_<library_name>_version

    IMPLICIT NONE

    version = get_<library_name>_version()

    WRITE(OUTPUT_UNIT, "()")
    WRITE(OUTPUT_UNIT, "(A,I0)")                                               &
        "Testing <library_name> at Shumlib version: ", version    


Next, each test case (which should be defined as a ``SUBROUTINE`` elsewhere in
the module) should be referenced like this:

.. parsed-literal::

    CALL run_test_case(test_subroutine_1_name, "subroutine_1_name")
    CALL run_test_case(test_subroutine_2_name, "subroutine_2_name")
    CALL run_t....

Although not *essential* the test routines written so far tend to name each test
routine starting with ``test_`` and then quote the name without this prefix (the
given name string is used only for reporting should the test fail).

If the test is actually written in C - and Fortran is only being used as a driver -
there also needs to be an interface block at the start of the module, defining
the ``ISO_C_BINDING`` binding. Although it is not a requirement, a recommendation
is to evaluate the pass/fail status of the test in C, and return the result to
Fortran with a ``C_BOOL`` argument, like so:

.. parsed-literal::

  INTERFACE
    SUBROUTINE c_test_function(test_ret)                                         &
               BIND(c, name="c_test_function")

    IMPORT :: C_BOOL

    IMPLICIT NONE

    LOGICAL(KIND=C_BOOL), INTENT(OUT) :: test_ret

    END SUBROUTINE c_test_function
  END INTERFACE

The Fortran subroutine which drives the C test is then a thin wrapper to call this
test function like so (asserts are explained below):

.. parsed-literal::

  SUBROUTINE test_subroutine_3_name

  IMPLICIT NONE

  LOGICAL(KIND=C_BOOL) :: test_ret
  LOGICAL :: check

  CALL set_case_name("C test")
  CALL c_test_function(test_ret)
  check = test_ret
  CALL assert_true(check, "C test fails!")

  END SUBROUTINE test_subroutine_3_name

Beyond this the content of the module is entirely up to the author; the module
can contain any number of other routines or functions required to help perform
the testing, so long as the main testing routines do not accept any arguments.

The only remaining detail is how to report testing results; this is done via
"assertions" which can be called at any time from within the test functions
above. The assertion functions in FRUIT are accessed via heavily overloaded
routines; ``assert_equals`` (or ``assert_not_equals``), which compares any 2
objects of identical type/rank/kind, and ``assert_true`` (or ``assert_false``)
which compares pairs of ``LOGICAL`` values. If in doubt about what is supported
it is easiest to refer directly to the ``fruit/fruit.f90`` file (search for
``assert`` to find the list of overloaded interfaces).  However the syntax for
the assertions is typically of the following form:

.. parsed-literal::

    CALL assert_equal(var_1, var_2, [dim1,] [dim2,] [delta,] message)

Where the arguments have the following meanings:

    **var_1**
        Variable containing the *expected* value (i.e. the value to be compared
        against and considered "correct").

    **var_2**
        Variable containing the *tested* value (i.e. the value which is being 
        validated by the test).

    **dim1**, **dim2**
        In the case that **var_1** and **var_2** are 1D or 2D arrays, these
        arguments are required to give their dimensions (in order).

    **delta**
        In the case that **var_1** and **var_2** are ``REAL`` variables,
        specifies an (absolute) tolerance for the comparison to allow.

    **message**
        This specifies what should be printed to stdout in the case that the
        assertion fails. Note that it does *not* need to go to any lengths to
        repeat information from the call (because FRUIT will print that
        information as part of its own messaging).

And for the logical version:

.. parsed-literal::

    CALL assert_true(var, message)

Where **var** is the logical variable being tested and **message** is the same
as in the above case.

FRUIT makefiles
'''''''''''''''

The ``fruit`` directory contains 2 makefiles; the first looks like a slightly
simplified version of the makefiles for any of the other library units; it
simply compiles the FRUIT framework file into a pair of libraries and places the
result into the output directory alongside the other Shumlib libraries. Note
that currently the MPI file isn't used (since we have no MPI based Shumlib
libraries).

Within the other makefile; ``fruit/Makefile-driver``, are instructions to
produce the main ``PROGRAM`` file and compile it into a pair of test executables
(one static and one dynamic). Check the inline comments for exact descriptions
of what each part is doing, but the general process is as follows:

    1. Determine a list of include and link flags for the compilation; this
    needs to include each library that has been compiled *and* defines tests.
    The top-level make file determines these names and provides them via the
    ``FRUITTESTS`` variable.

    2. Auto-generate the driver file; the fact that the FRUIT test modules
    follow a carefully named set of conventions (see previous section) means a
    simple driver program can be easily constructed simply by knowing the
    library names to be tested.

    3. Compile the test executables; at this point the Shumlib libraries (and
    FRUIT library) in the output directory, plus the object files produced in
    each library's ``test`` directory are all pulled together to produce the
    final test executables.

The resulting executables are put into the output directory in a ``tests``
subdirectory (and the top-level makefile executes them immediately).

FRUIT output
''''''''''''

Hopefully the output from FRUIT is fairly self-explanatory; each call to
``run_test_case`` is counted in the output as a "case" and each call to one of
the ``assert_`` routines is counted as an "assert".  Anything less than 100%
successful rate indicates a problem.


Version Inclusion and Precision Bomb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Shumlib also contains a mechanism to ensure that each of the libraries is
populated by some hardcoded routines that are able to report the version of
Shumlib at the time they were compiled. This version is defined in the header
file ``common/shumlib_version.h`` as the macro ``SHUMLIB_VERSION``.

Alongside this is a special header; ``common/precision_bomb.h``; this is a
carefully designed header which ensures that the assumptions made about
precision in the libraries (see earlier section) are valid. If the machine
attempting to compile Shumlib does not meet the requirements it will refuse to
compile. Unfortunately there is no way around this as much of the code in the
existing libraries relies on this assumption internally.

The technique by which the version routines and precision bomb are deployed to
each library is through the use of a makefile in the ``common`` directory. This
common makefile is included within the main makefiles of each individual
library, after they have defined the ``VERSION_LIBNAME`` variable. This variable
is used to generate appropriate objects and headers for that library in the
following way:

    1. First a pair of objects (with and without ``PIC``) are compiled from
    ``common/shumlib_version.c``.  It is at this point the precision bomb header
    is included and a series of macros are expanded with the library name to
    create a single, argument-less C function which returns the Shumlib version
    number; called ``get_<library_name>_version``.

    2. The above object files are named ``c_<library_name>_version<_PIC>.o`` and
    are placed into the *library's* directory for inclusion by its makefile
    later.

    3. A custom header file called ``c_<library_name>_version.h`` is produced
    (again in the *library's* directory). It re-uses the same library name macro
    to produce a prototype for the function defined in step (1).

    4. A custom Fortran module file called ``f_<library_name>_version_mod.f90``
    is produced (once more in the *library's* directory), this module uses the
    ``ISO_C_BINDING`` module to call the C function above directly.

    5. The above Fortran module is compiled to produce a pair of object files
    (with and without ``PIC``) in the *library's* directory.

    6. Finally; a set of 3 variables are defined which hold key filenames which
    the library makefile will require; ``VERSION_OBJECTS`` and
    ``VERSION_OBJECTS_PIC`` (for object files to include in the static and
    dynamic library respectively), and ``VERSION_CLEAN`` to specify the names of
    the two generated files for the library makefile to remove when a ``clean``
    target is issued.

Having done the above and then made use of the returned variables; the functions
for retrieving the version information should appear in the produced libraries.
It is up to the application using the library to then access the function.




