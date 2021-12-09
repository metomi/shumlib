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
# combinations available at the Bureau of Meteorology ...
#
# USAGE:  (Note - must be run from the toplevel Shumlib directory!)
#   scripts/bom_install_shumlib.sh [xc40]
#
#PBS -q build
#PBS -P pr_nwp
#PBS -S /bin/bash
#PBS -l select=1:ncpus=4:mem=4G
#PBS -l walltime=00:30:00

set -eu

# Number of threads (should not exceed PBS request):
export OMP_NUM_THREADS=4

# Ensure directory is correct
cd $PBS_O_WORKDIR

# Take the platform name as an argument
# (purely so we can maintain one script rather than 2)
PLATFORM=${1:-xc40}

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

# Functions which build given libraries and tests for those libraries
function build_test_clean {
    local config=$1
    shift
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
    echo make -f make/$config.mk $*
    make -f make/$config.mk $*
    echo make -f make/$config.mk test
    make -f make/$config.mk test
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
}

# Function which executes the above function four time - once for each of the
# possible OpenMP state and Thread_Utils state combinations
function build_openmp_onoff {
    local config=$1
    local dir=$2
    shift 2
    # Copy source to temporary build directory and switch there
    TEMP_BUILD_DIR=$(mktemp -d)
    cp -r * $TEMP_BUILD_DIR
    cd $TEMP_BUILD_DIR
    # Perform the build
    export LIBDIR_OUT=$dir/openmp
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean $config $*
    export LIBDIR_OUT=$dir/no-openmp
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean $config $*
    export LIBDIR_OUT=$dir/thread_utils
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean $config $*
    export LIBDIR_OUT=$dir/serial_thread_utils
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean $config $*
    # Tidy up the temporary directory
    rm -rf $TEMP_BUILD_DIR
}

# Crayftn/CrayCC Haswell 8.4.5
THIS="xc40_haswell_cray_8.4.5"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load craype/2.4.2
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.4.5
    module load craype-haswell
    CONFIG=bom-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/bom-xc40-haswell-crayftn-8.4.5-craycc-8.4.5
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Crayftn/CrayCC Haswell 8.7.0
THIS="xc40_haswell_cray_8.7.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load craype/2.5.14
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.7.0
    module load craype-haswell
    CONFIG=bom-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/bom-xc40-haswell-crayftn-8.7.0-craycc-8.7.0
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Ifort/Icc Haswell 17.0.7
THIS="xc40_haswell_intel_17.0.7"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-intel/5.2.82
    module swap intel intel/17.0.7.259
    module load craype-haswell
    CONFIG=bom-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/bom-xc40-haswell-ifort-17.0.7-icc-17.0.7
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Haswell 5.1.0 (Current system default)
THIS="xc40_haswell_gnu_5.1.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/5.1.0
    module load craype-haswell
    CONFIG=bom-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/bom-xc40-haswell-gfortran-5.1.0-gcc-5.1.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

# Gfortran/Gcc Haswell 7.3.0
THIS="xc40_haswell_gnu_7.3.0"
if [ $PLATFORM == "xc40" ] || [ $PLATFORM == $THIS ] ; then
    (
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/7.3.0
    module load craype-haswell
    CONFIG=bom-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/bom-xc40-haswell-gfortran-7.3.0-gcc-7.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
fi

