#!/bin/bash --login
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
#
# This file is part of the UM Shared Library project.
#
# The UM Shared Library is free software: you can redistribute it
# and/or modify it under the terms of the Modified BSD License, as
# published by the Open Source Initiative.
#
# The UM Shared Library is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Modified BSD License for more details.
#
# You should have received a copy of the Modified BSD License
# along with the UM Shared Library.
# If not, see <http://opensource.org/licenses/BSD-3-Clause>.
#*******************************************************************************
#
# Script to assist with installing shumlib under the many platform/compiler
# combinations available at the Met Office...
#
# USAGE:  (Note - must be run from the toplevel Shumlib directory!)
#   scripts/meto_install_shumlib.sh [xc40|x86|ex1a]
#
# This script was used to install shumlib version 2023.06.1
# and was intended for use with the UM at UM 13.3
#

set -eu

# set up no IEEE list
NO_IEEE_LIST=${NO_IEEE_LIST:-"xc40_haswell_gnu_4.9.1 xc40_ivybridge_gnu_4.9.1 ex1a_gnu_12.1.0"}

# Ensure directory is correct
cd $(readlink -f $(dirname $0)/..)

# Take the platform name as an argument
# (purely so we can maintain one script rather than 2)
PLATFORM=${1:-}

if [ -z "${PLATFORM}" ] ; then
    echo "Please provide platform or specific build as argument"
    echo "e.g. x86, xc40, ex1a or grep this file for THIS to see the "
    echo "other options for specific builds"
    exit 1
fi

# Try and detect if the script is being run from the right place
if [ ! -f Makefile ] || ! `ls -d shum_* > /dev/null 2>&1` ; then
    echo "Cannot find expected files... Please note that this script "
    echo "must be run from toplevel Shumlib directory"
    exit 1
fi

# Get the list of all possible libraries to build (this is used by some of the
# tasks to exclude certain libraries)
LIB_DIRS=$(ls -d shum_* | xargs)

# Destination for the build (can be overidden, otherwise defaults to a
# "build" directory in the working copy - like the Makefile would)
BUILD_DESTINATION=${BUILD_DESTINATION:-$PWD/build}

# This list dictates which threading variants of Shumlib will be installed
# (all versions defined will always be built + tested, but only those set
# by this list will end up in the BUILD_DESTINATION directory)
INSTALL_LIBS=${INSTALL_LIBS:-"openmp no-openmp thread_utils serial_thread_utils"}

# Function for testing membership of a list
function contains {
    local elt
    for elt in $2 ; do
        [[ "$elt" == "$1" ]] && return 0;
    done
    return 1
}

# Functions which build given libraries and tests for those libraries
function build_test_clean {
    local config=$1
    shift
    if [[ " $NO_IEEE_LIST " =~ " $THIS " ]] ; then
      export SHUM_HAS_IEEE_ARITHMETIC="false"
    else
      unset SHUM_HAS_IEEE_ARITHMETIC
    fi
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
    echo make -f make/$config.mk $*
    make -f make/$config.mk $*
    echo make -f make/$config.mk test
    make -f make/$config.mk test
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
}

# Function which executes the above function several times - once for each of
# the possible OpenMP state and Thread_Utils state combinations. Note that
# the build will only be copied to the destination install directory if the
# variable LIBDIR_OUT is set - this is done selectively depending on which
# builds are required to be installed (according to the INSTALL_LIBS variable)
function build_openmp_onoff {
    local config=$1
    local dir=$2
    shift 2
    # Copy source to temporary build directory and switch there
    TEMP_BUILD_DIR=${SHUM_TMPDIR:-$(mktemp -d)}
    mkdir -p $TEMP_BUILD_DIR
    cp -r * $TEMP_BUILD_DIR
    cd $TEMP_BUILD_DIR

    echo "Build Dir is: $TEMP_BUILD_DIR"

    # OpenMP
    unset LIBDIR_OUT
    if contains "openmp" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/openmp
    fi
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean $config $*

    # No-OpenMP
    unset LIBDIR_OUT
    if contains "no-openmp" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/no-openmp
    fi
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean $config $*

    # Thread Utils + OpenMP
    unset LIBDIR_OUT
    if contains "thread_utils" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/thread_utils
    fi
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean $config $*

    # Thread Utils, No-OpenMP
    unset LIBDIR_OUT
    if contains "serial_thread_utils" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/serial_thread_utils
    fi
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean $config $*

    # Unset threading - not practically a useful install; more of a test
    # of the Makefiles; this checks to see what happens if the environment
    # variables that control the build threading are not set
    unset LIBDIR_OUT
    if contains "unset_threading" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/unset_threading
    fi
    unset SHUM_OPENMP
    unset SHUM_USE_C_OPENMP_VIA_THREAD_UTILS
    build_test_clean $config $*

    # Test if $SHUM_TMPDIR is unset. If it is not set, we are using the mktmp
    # directory, which must be cleaned up again.
    if [ -z ${SHUM_TMPDIR+x} ]; then
      # Tidy up the temporary directory
      rm -rf $TEMP_BUILD_DIR
    fi

}

# Intel/GCC (ifort 16)
THIS="x86_ifort_16.0_gcc"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    source /etc/profile.d/metoffice.d/modules.sh || :
    module purge
    module load ifort/16.0_64  # From METO_LINUX family in rose-stem
    unset SHUM_TMPDIR
    CONFIG=meto-x86-ifort15+-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-16.0.1-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

THIS="x86_ifort_16.0_clang"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    # Intel/Clang  (ifort 16 / clang 12.0.0)
    (
    source /etc/profile.d/metoffice.d/modules.sh || :
    module purge
    module use /project/extrasoftware/modulefiles.rhel7
    module load ifort/16.0_64  # From METO_LINUX family in rose-stem
    module unload libraries/gcc
    module load gcc/8.1.0
    module load llvm/12.0.0
    unset SHUM_TMPDIR
    CONFIG=meto-x86-ifort15+-clang
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-16.0.1-clang-12.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

THIS="x86_nag_7.0_gcc"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    # NagFor/GCC (nagfor 7.0.0)
    (
    source /etc/profile.d/metoffice.d/modules.sh || :
    module purge
    module load nagfor/7.0.0_64
    unset SHUM_TMPDIR
    CONFIG=meto-x86-nagfor-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-nagfor-7.0.0-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Portland/GCC (pgfortran 16.10) - Note that the "fieldsfile_class"
# lib doesn't work with portland, so we exclude it
THIS="x86_pgfortran_16.10_gcc"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    source /etc/profile.d/metoffice.d/modules.sh || :
    module purge
    module load pgfortran/16.10_64
    unset SHUM_TMPDIR
    CONFIG=meto-x86-portland-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-pgfortran-16.10.0-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR $(sed "s/\bshum_fieldsfile_class\b//g" \
                                         <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/GCC 6.1.0
# Have to use Lfric module as default Gfortran is too old
THIS="x86_gnu_6.1.0"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    module use /data/users/lfric/modules/modulefiles.rhel7
    module purge
    module load environment/lfric/gnu/6.1.0
    unset SHUM_TMPDIR
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-gfortran-6.1.0-gcc-6.1.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/GCC 9.2.0
# Using jopa module
THIS="x86_gnu_9.2.0"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    module use /home/h03/jopa/modulefiles
    module purge
    module load jedi/gcc/9.2.0
    unset SHUM_TMPDIR
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-gfortran-9.2.0-gcc-9.2.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/GCC 9.3.0
# Using jopa module
THIS="x86_gnu_9.3.0"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    module use /home/h03/jopa/modulefiles
    module purge
    module load gnu-toolchain/gcc/9.3.0
    unset SHUM_TMPDIR
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-gfortran-9.3.0-gcc-9.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/GCC 10.2.0
# Using LFRic module
THIS="x86_gnu_10.2.0"
if [ $PLATFORM == "x86" ] || [ $PLATFORM == $THIS ] ; then
    (
    module use /data/users/lfric/modules/modulefiles.rhel7
    module purge
    module load environment/lfric/gnu/10.2.0
    unset SHUM_TMPDIR
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-gfortran-10.2.0-gcc-10.2.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# GNU generic gfortran/gcc
THIS="x86_gnu_generic"
if [[ $PLATFORM == "x86" ]] || [[ $PLATFORM == $THIS ]] ; then
  if [[ $(gcc -dumpversion) > 8 ]] ; then
    (
    unset SHUM_TMPDIR
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/x86-gfortran-$(gfortran -dumpversion)-gcc-$(gcc -dumpversion)
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
  else
    >&2 echo "SKIPPING $THIS as GCC version older than 8"
  fi
fi


# Crayftn/CrayCC Haswell 8.3.4 (Current system default)
# - note that these earlier versions of CCE don't work correctly with the
# Fieldsfile read/write libraries, so we exclude them
THIS="xc40_haswell_cray_8.3.4"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then

    (
    module purge
    module load PrgEnv-cray/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap cce/8.3.4
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-crayftn8.3.4+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-crayftn-8.3.4-craycc-8.3.4
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC Ivybridge 8.3.4 (Current system default)
# - note that these earlier versions of CCE don't work correctly with the
# Fieldsfile read/write libraries, so we exclude them
THIS="xc40_ivybridge_cray_8.3.4"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-cray/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap cce/8.3.4
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-crayftn8.3.4+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.3.4-craycc-8.3.4
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC Ivybridge 8.4.3 - note that these earlier versions of CCE don't
# work correctly with the Fieldsfile read/write libraries, so we exclude them
THIS="xc40_ivybridge_cray_8.4.3"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-cray/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap cce/8.4.3
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.4.3-craycc-8.4.3
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC Haswell 8.5.8
THIS="xc40_haswell_cray_8.5.8"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-cray/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap cce/8.5.8
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-crayftn-8.5.8-craycc-8.5.8
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC Ivybridge 8.5.8
THIS="xc40_ivybridge_cray_8.5.8"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-cray/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap cce/8.5.8
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.5.8-craycc-8.5.8
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Ifort/Icc Haswell 15.0 (Current system default)
THIS="xc40_haswell_intel_15.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-intel/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap intel/15.0.0.090
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-ifort-15.0.0-icc-15.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Ifort/Icc Ivybridge 15.0 (Current system default)
THIS="xc40_ivybridge_intel_15.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-intel/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap intel/15.0.0.090
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-ifort-15.0.0-icc-15.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Ifort/Icc Haswell 17.0
THIS="xc40_haswell_intel_17.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-intel/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap intel/17.0.0.098
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-ifort-17.0.0-icc-17.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Ifort/Icc Ivybridge 17.0
THIS="xc40_ivybridge_intel_17.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-intel/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.0.4
    module swap intel/17.0.0.098
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-ifort-17.0.0-icc-17.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Haswell 4.9.1 (Current system default)
THIS="xc40_haswell_gnu_4.9.1"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.5.3
    module swap gcc/4.9.1
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-gfortran-4.9.1-gcc-4.9.1
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Ivybridge 4.9.1 (Current system default)
THIS="xc40_ivybridge_gnu_4.9.1"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.5.3
    module swap gcc/4.9.1
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-gfortran-4.9.1-gcc-4.9.1
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Haswell 6.3.0
THIS="xc40_haswell_gnu_6.3.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.5.3
    module swap gcc/6.3.0
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-gfortran-6.3.0-gcc-6.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Ivybridge 6.3.0
THIS="xc40_ivybridge_gnu_6.3.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module load cdt/17.03
    module load cray-mpich/7.5.3
    module swap gcc/6.3.0
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    unset SHUM_TMPDIR
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-gfortran-6.3.0-gcc-6.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC 15.0.0
THIS="ex1a_cray_15.0.0"
if [ $PLATFORM == "ex1a" ] || [ $PLATFORM == $THIS ] ; then

    (
    module switch PrgEnv-cray PrgEnv-cray/8.3.3
    module load cpe/22.11
    unset SHUM_TMPDIR
    CONFIG=meto-ex1a-crayftn12.0.1+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-ex1a-crayftn-15.0.0-craycc-15.0.0
    # If it is not the case that $CYLC_TASK_WORK_PATH is unset, use it to
    # define $SHUM_TMPDIR
    if [ ! -z ${CYLC_TASK_WORK_PATH+x} ]; then
      export SHUM_TMPDIR=$CYLC_TASK_WORK_PATH/meto-ex1a-crayftn-15.0.0-craycc-15.0.0
    fi
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran 12.1.0
THIS="ex1a_gnu_12.1.0"
if [ $PLATFORM == "ex1a" ] || [ $PLATFORM == $THIS ] ; then

    (
    module switch PrgEnv-cray PrgEnv-gnu/8.3.3
    module load cpe/22.11
    unset SHUM_TMPDIR
    CONFIG=meto-ex1a-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-ex1a-gfortran-12.1.0-gcc-12.1.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi
