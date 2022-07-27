# Main makefile storing common options for building all libraries, by triggering
# individual makefiles from each library directory
#--------------------------------------------------------------------------------

# The intention is that the user points "make" at one of the platform specific
# files, which include this file at the end; here we try to ensure that this
# has happened (if it hasn't, various important variables will not be set)
ifndef PLATFORM
$(error Platform file not loaded, re-run as "make -f" providing a file from \
the "make" subdirectory as an argument)
endif

# Setup destination directory - this can be overidden by the user if they wish
# to install directly to a different location
LIBDIR_ROOT ?= ${PWD}/build
LIBDIR_OUT ?= ${LIBDIR_ROOT}/${PLATFORM}
export

# Setup test directory
SHUM_TMPDIR ?= $(shell mktemp -d)

# Setup Base directory. This is included as the base in all paths
DIR_ROOT ?= ${PWD}

# Set switches controlling what type of test executables are built
SHUM_BUILD_DYNAMIC ?= true
SHUM_BUILD_STATIC ?= true

# Setup the flags which will be passed to all compilations - add the openMP
# flags based on the setting below (defaults to true)
SHUM_OPENMP ?= true
ifeq (${SHUM_OPENMP}, true)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_EXTRA} ${FCFLAGS_OPENMP}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_EXTRA} ${CCFLAGS_OPENMP}
else ifeq (${SHUM_OPENMP}, false)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_EXTRA} ${FCFLAGS_NOOPENMP}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_EXTRA} ${CCFLAGS_NOOPENMP}
else
$(error If specified in the environment SHUM_OPENMP environment variable must \
be set to either "true" or "false")
endif

ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, true)
# This is handled in the Machine specific makefile
else ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, false)
# This is handled in the Machine specific makefile
else
$(error If specified in the environment SHUM_USE_C_OPENMP_VIA_THREAD_UTILS \
environment variable must be set to either "true" or "false")
endif

# Check IEEE arithmetic has been correctly set
SHUM_HAS_IEEE_ARITHMETIC ?= false
ifeq (${SHUM_HAS_IEEE_ARITHMETIC}, true)
# This is handled in the Machine specific makefile
else ifeq (${SHUM_HAS_IEEE_ARITHMETIC}, false)
# This is handled in the Machine specific makefile
else
$(error If specified in the environment SHUM_HAS_IEEE_ARITHMETIC \
environment variable must be set to either "true" or "false")
endif
SHUM_EVAL_NAN_BY_BITS ?= false
ifeq (${SHUM_EVAL_NAN_BY_BITS}, true)
# This is handled in the Machine specific makefile
else ifeq (${SHUM_EVAL_NAN_BY_BITS}, false)
# This is handled in the Machine specific makefile
else
$(error If specified in the environment SHUM_EVAL_NAN_BY_BITS \
environment variable must be set to either "true" or "false")
endif
SHUM_EVAL_DENORMAL_BY_BITS ?= false
ifeq (${SHUM_EVAL_DENORMAL_BY_BITS}, true)
# This is handled in the Machine specific makefile
else ifeq (${SHUM_EVAL_DENORMAL_BY_BITS}, false)
# This is handled in the Machine specific makefile
else
$(error If specified in the environment SHUM_EVAL_DENORMAL_BY_BITS \
environment variable must be set to either "true" or "false")
endif


# Check compilers are defined
ifndef FC
$(error FC is not set)
endif
ifndef CC
$(error CC is not set)
endif
ifndef FPP
$(error FPP is not set)
endif

# Default target - build all available libraries
#--------------------------------------------------------------------------------
.PHONY: default
default: all_libs

# Output directories
#--------------------------------------------------------------------------------
OUTDIRS=${LIBDIR_OUT}/lib ${LIBDIR_OUT}/include

${OUTDIRS}:
	mkdir -p ${LIBDIR_OUT}/lib
	mkdir -p ${LIBDIR_OUT}/include

OUTDIR_TESTS=${LIBDIR_OUT}/tests

${OUTDIR_TESTS}:
	mkdir -p ${LIBDIR_OUT}/tests

${SHUM_TMPDIR}:
	mkdir -p ${SHUM_TMPDIR}

# Setup path to directory containing common/shared components; these include
# functions that provide Shumlib version information and the C Precision Bomb
# (which will protect against compilation on platforms where the assumptions
# about precision made in the libraries is invalid)
#--------------------------------------------------------------------------------
COMMON_DIR=${DIR_ROOT}/common/src

# The FRUIT source itself
#------------------------
FRUIT=fruit
.PHONY: ${FRUIT}
${FRUIT}: ${OUTDIRS}
	${MAKE} -C ${DIR_ROOT}/${FRUIT}

# Libraries
#--------------------------------------------------------------------------------

# String conv
#------------
STR_CONV=shum_string_conv
STR_CONV_PREREQ=

# Byte-swapping
#--------------
BSWAP=shum_byteswap
ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, true)
BSWAP_PREREQ=STR_CONV CONSTS THREAD_UTILS
else
BSWAP_PREREQ=STR_CONV CONSTS
endif


# Data conv
#----------
DATA_CONV=shum_data_conv
DATA_CONV_PREREQ=STR_CONV

# WGDOS packing
#--------------
PACK=shum_wgdos_packing
PACK_PREREQ=STR_CONV CONSTS

# Horizontal Interpolation
#--------------
HORIZ_INTERP=shum_horizontal_field_interp
HORIZ_INTERP_PREREQ=NUMTOOLS

# Thread Utils
#--------------
THREAD_UTILS=shum_thread_utils
THREAD_UTILS_PREREQ=

# LL to/from EQ transformation and wind rotation
#-----------------------------------------------
LLEQ=shum_latlon_eq_grids
LLEQ_PREREQ=CONSTS

# Fieldsfile API
#---------------
FFILE=shum_fieldsfile
FFILE_PREREQ=BSWAP

# Spiral Search Algorithm
#------------------------
SPIRAL=shum_spiral_search
SPIRAL_PREREQ=CONSTS STR_CONV

# Fieldsfile classes
#-------------------
FFCLASS=shum_fieldsfile_class
FFCLASS_PREREQ=FFILE PACK

# Constants
#---------------
CONSTS=shum_constants
CONSTS_PREREQ=

# Number Tools
#---------------
NUMTOOLS=shum_number_tools
ifeq (${SHUM_EVAL_NAN_BY_BITS}, true)
NUMTOOLS_PREREQ=CONSTS
else
NUMTOOLS_PREREQ=
endif


# All libs vars
#--------------
ALL_LIBS_VARS=CONSTS BSWAP STR_CONV DATA_CONV PACK THREAD_UTILS LLEQ \
	      FFILE HORIZ_INTERP SPIRAL FFCLASS NUMTOOLS
ALL_LIBS=$(foreach lib,${ALL_LIBS_VARS},${${lib}})

# Forward targets (targets with "VAR" names)
#--------------------------------------------------------------------------------

.PHONY: ${ALL_LIBS_VARS} $(addsuffix _PREREQ, ${ALL_LIBS_VARS}) $(addsuffix _TESTS, ${ALL_LIBS_VARS}) $(addsuffix _PREREQ_TESTS, ${ALL_LIBS_VARS})

# auto-generate main targets
${ALL_LIBS_VARS}: %: %_PREREQ ${OUTDIRS}
	${MAKE} -C ${DIR_ROOT}/${$@}/src

# auto-generate prerequisite targets
$(addsuffix _PREREQ, ${ALL_LIBS_VARS}): %:
	$(foreach prereq,${$@},${MAKE} -C ${DIR_ROOT} ${prereq};)

# auto-generate test makefile path targets
$(wildcard $(addprefix ${DIR_ROOT}/, $(addsuffix /test, ${ALL_LIBS}))): ${DIR_ROOT}/%/test: ${FRUIT} %

# auto-generate test targets
$(addsuffix _TESTS, ${ALL_LIBS_VARS}): %_TESTS: ${FRUIT} % %_PREREQ_TESTS ${OUTDIR_TESTS}
	test ! -d ${DIR_ROOT}/${$(subst _TESTS,,$@)}/test || ${MAKE} -C ${DIR_ROOT}/${$(subst _TESTS,,$@)}/test
	@test -d ${DIR_ROOT}/${$(subst _TESTS,,$@)}/test || echo "No tests for ${$(subst _TESTS,,$@)}"

# auto-generate prerequisite test targets
$(addsuffix _PREREQ_TESTS, ${ALL_LIBS_VARS}): %:
	$(foreach prereq,${$(subst _PREREQ_TESTS,,$@)_PREREQ},${MAKE} -C ${DIR_ROOT} ${prereq}_TESTS;)

# reverse targets  (targets with natural names, resolving to targets with "VAR" names)
#--------------------------------------------------------------------------------

.PHONY: ${ALL_LIBS} $(addsuffix _tests, ${ALL_LIBS}) $(addsuffix _prereq, ${ALL_LIBS}) $(addsuffix _prereq_tests, ${ALL_LIBS})

REVERSE_template = $(strip $(foreach revlib,${ALL_LIBS_VARS},$(if $(filter $(1),${${revlib}}),${revlib},)))

# auto-generate reverse targets
${ALL_LIBS}: %:
	${MAKE} -C ${DIR_ROOT} $(call REVERSE_template,$@)

# auto-generate reverse test targets
$(addsuffix _tests, ${ALL_LIBS}): %:
	${MAKE} -C ${DIR_ROOT} $(call REVERSE_template,$(subst _tests,,$@))_TESTS

# auto-generate reverse prereq targets
$(addsuffix _prereq, ${ALL_LIBS}): %:
	${MAKE} -C ${DIR_ROOT} $(call REVERSE_template,$(subst _prereq,,$@))_PREREQ

# auto-generate reverse prereq test targets
$(addsuffix _prereq_tests, ${ALL_LIBS}): %:
	${MAKE} -C ${DIR_ROOT} $(call REVERSE_template,$(subst _prereq_tests,,$@))_PREREQ_TESTS

# All library targets
#--------------------------------------------------------------------------------

.PHONY: all_libs all_tests

ROOT_LIBS=${LIBDIR_OUT}/lib/libshum.a ${LIBDIR_OUT}/lib/libshum.so

# 'make all_libs' builds all libraries
all_libs: ${ROOT_LIBS} ${ALL_LIBS}

# 'make all_tests' builds all tests (but does not run them)
all_tests: ${ROOT_LIBS} $(addsuffix _tests, ${ALL_LIBS})

${LIBDIR_OUT}/lib/libshum.a: ${ALL_LIBS}
	${AR} $@ $(filter-out %_PIC.o,$(wildcard ${DIR_ROOT}/*/src/*.o))

${LIBDIR_OUT}/lib/libshum.so: ${ALL_LIBS}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} -o $@ $(wildcard ${DIR_ROOT}/*/src/*_PIC.o)

# FRUIT testing control
#--------------------------------------------------------------------------------

.PHONY: test run_tests check fruit_tests

# 'make check' builds all libraries, then tests, then runs them.
check:
	${MAKE} -C ${DIR_ROOT} all_libs
	${MAKE} -C ${DIR_ROOT} all_tests
	${MAKE} -C ${DIR_ROOT} run_tests

# 'make test' builds the tests for currently build libraries, then runs them
test: ${FRUIT} $(addsuffix _tests, $(patsubst lib%.so, %, $(notdir $(wildcard ${LIBDIR_OUT}/lib/libshum_*.so))))
	${MAKE} -C ${DIR_ROOT} run_tests

# 'make run_tests' runs the currently built tests
run_tests: ${SHUM_TMPDIR}
	${MAKE} -C ${DIR_ROOT}/${FRUIT} -f Makefile-driver
ifeq (${SHUM_BUILD_STATIC}, true)
	${LIBDIR_OUT}/tests/fruit_tests_static.exe
ifneq ("$(wildcard ${LIBDIR_OUT}/tests/fruit_tests_static_one.exe)", "")
	${LIBDIR_OUT}/tests/fruit_tests_static_one.exe
endif
endif
ifeq (${SHUM_BUILD_DYNAMIC}, true)
	${LIBDIR_OUT}/tests/fruit_tests_dynamic.exe
ifneq ("$(wildcard ${LIBDIR_OUT}/tests/fruit_tests_dynamic_one.exe)", "")
	${LIBDIR_OUT}/tests/fruit_tests_dynamic_one.exe
endif
endif

# dummy target for fruit
fruit_tests:
	@echo "Fruit Lib Built"

# Cleanup targets
#--------------------------------------------------------------------------------
.PHONY: clean clean-temp clean-build
clean-temp:
	@$(foreach libname,$(ALL_LIBS),${MAKE} -C ${DIR_ROOT}/$(libname)/src clean;)
	@$(foreach libname_test,$(wildcard $(addsuffix /test, ${ALL_LIBS})),${MAKE} -C ${DIR_ROOT}/$(libname_test) clean;)
	${MAKE} -C ${DIR_ROOT}/${FRUIT} clean
	${MAKE} -C ${DIR_ROOT}/${FRUIT} -f Makefile-driver clean

clean-build:
	rm -rf ${OUTDIRS} ${OUTDIR_TESTS}
	rm -rf ${LIBDIR_OUT}
	rmdir ${LIBDIR_ROOT} || :

clean: clean-temp clean-build
