#!/usr/bin/env bash
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
#   scripts/meto_install_shumlib.sh [xc40|x86] 
#
# This script was used to install shumlib version 2018.06.1
# and is based on the families etc. from the UM at UM 11.1

set -eu

# Take the platform name as an argument 
# (purely so we can maintain one script rather than 2)
PLATFORM=${1:-}
if [ -z "${PLATFORM}" ] ; then
    echo "Please provide platform as first (and only) argument"
    echo "Either x86 or xc40"
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

# Function which builds given libraries and tests for those libraries
function build_test_clean {
    local config=$1
    shift
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
    echo make -f make/$config.mk $*
    make -f make/$config.mk $*
    echo timeout 5m make -f make/$config.mk test || :
    timeout 5m make -f make/$config.mk test || :
    echo make -f make/$config.mk clean-temp
    make -f make/$config.mk clean-temp
}

# Function which executes the above function twice - once for each of the
# possible OpenMP states
function build_openmp_onoff {
    local config=$1
    local dir=$2
    shift 2
    export LIBDIR_OUT=$dir/openmp
    export SHUM_OPENMP=true
    build_test_clean $config $*
    export LIBDIR_OUT=$dir/no-openmp
    export SHUM_OPENMP=false
    build_test_clean $config $*
}

if [ $PLATFORM == "x86" ] ; then

    # Ensure the module command will be available
    source /etc/profile.d/metoffice.d/modules.sh

    # Intel/GCC (ifort 12) - Note that the "fieldsfile_class" 
    # lib doesn't work with ifort 12, so we exclude it
    module purge
    module load ifort/12.0_64  
    CONFIG=meto-x86-ifort12+-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-12.0.4-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR $(sed "s/\bshum_fieldsfile_class\b//g" \
                                         <<< $LIB_DIRS)

    # Intel/Clang (ifort 12) - Note that the "fieldsfile_class" 
    # lib doesn't work with ifort 12, so we exclude it
    module purge
    module load ifort/12.0_64  
    CONFIG=meto-x86-ifort12+-clang
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-12.0.4-clang-3.4.2
    build_openmp_onoff $CONFIG $LIBDIR $(sed "s/\bshum_fieldsfile_class\b//g" \
                                         <<< $LIB_DIRS)

    # Intel/GCC (ifort 16)
    module purge
    module load ifort/16.0_64  # From METO_LINUX family in rose-stem
    CONFIG=meto-x86-ifort15+-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-16.0.1-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Intel/Clang  (ifort 16)
    module purge
    module load ifort/16.0_64  # From METO_LINUX family in rose-stem
    CONFIG=meto-x86-ifort15+-clang
    LIBDIR=$BUILD_DESTINATION/meto-x86-ifort-16.0.1-clang-3.4.2
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # NagFor/GCC (nagfor 6.0.0)
    module purge
    module load nagfor/6.0.0_64
    CONFIG=meto-x86-nagfor-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-nagfor-6.0.0-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Portland/GCC (pgfortran 15.7) - Note that the "fieldsfile_class" 
    # lib doesn't work with portland, so we exclude it
    module purge
    module load pgfortran/15.7_64
    CONFIG=meto-x86-portland-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-pgfortran-15.7.0-gcc-4.4.7
    build_openmp_onoff $CONFIG $LIBDIR $(sed "s/\bshum_fieldsfile_class\b//g" \
                                         <<< $LIB_DIRS)

    # Gfortran/GCC 6.1.0
    # Have to use Lfric module as default Gfortran is too old
    source /data/users/lfric/modules/setup
    module purge
    module load environment/lfric/gnu/6.1.0
    CONFIG=meto-x86-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-x86-gfortran-6.1.0-gcc-6.1.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

fi

if [ $PLATFORM == "xc40" ] ; then

    # Crayftn/CrayCC Haswell 8.3.4 (Current system default)
    # - note that these earlier versions of CCE don't work correctly with the 
    # Fieldsfile read/write libraries, so we exclude them
    module purge
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.3.4
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-crayftn8.3.4+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-crayftn-8.3.4-craycc-8.3.4
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)

    # Crayftn/CrayCC Ivybridge 8.3.4 (Current system default)
    # - note that these earlier versions of CCE don't work correctly with the 
    # Fieldsfile read/write libraries, so we exclude them
    module purge
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.3.4
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-crayftn8.3.4+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.3.4-craycc-8.3.4
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)

    # Crayftn/CrayCC Ivybridge 8.4.3 - note that these earlier versions of CCE don't 
    # work correctly with the Fieldsfile read/write libraries, so we exclude them
    module purge
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.4.3
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.4.3-craycc-8.4.3
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)

    # Crayftn/CrayCC Haswell 8.5.8
    module purge
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.5.8
    module load craype-haswell
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-crayftn-8.5.8-craycc-8.5.8
    build_openmp_onoff $CONFIG $LIBDIR all_libs
  
    # Crayftn/CrayCC Ivybridge 8.5.8
    module purge
    module load PrgEnv-cray/5.2.82
    module swap cce cce/8.5.8
    module load craype-ivybridge
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-crayftn8.4.0+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-crayftn-8.5.8-craycc-8.5.8
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Ifort/Icc Haswell 15.0 (Current system default)
    module purge
    module load PrgEnv-intel/5.2.82
    module swap intel intel/15.0.0.090
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-ifort-15.0.0-icc-15.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Ifort/Icc Ivybridge 15.0 (Current system default)
    module purge
    module load PrgEnv-intel/5.2.82
    module swap intel intel/15.0.0.090
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-ifort-15.0.0-icc-15.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Ifort/Icc Haswell 17.0 
    module purge
    module load PrgEnv-intel/5.2.82
    module swap intel intel/17.0.0.098
    module load craype-haswell
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-ifort-17.0.0-icc-17.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Ifort/Icc Ivybridge 17.0
    module purge
    module load PrgEnv-intel/5.2.82
    module swap intel intel/17.0.0.098
    module load craype-ivybridge
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-ifort-icc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-ifort-17.0.0-icc-17.0.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Gfortran/Gcc Haswell 4.9.1 (Current system default)
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/4.9.1
    module load craype-haswell
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-gfortran-4.9.1-gcc-4.9.1
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Gfortran/Gcc Ivybridge 4.9.1 (Current system default)
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/4.9.1
    module load craype-ivybridge
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-gfortran-4.9.1-gcc-4.9.1
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Gfortran/Gcc Haswell 6.3.0
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/6.3.0
    module load craype-haswell
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-haswell-gfortran-6.3.0-gcc-6.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs

    # Gfortran/Gcc Ivybridge 6.3.0
    module purge
    module load PrgEnv-gnu/5.2.82
    module swap gcc gcc/6.3.0
    module load craype-ivybridge
    module load cdt/17.03
    module load metoffice/tempdir
    module load metoffice/userenv
    module load craype-network-aries
    CONFIG=meto-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-xc40-ivybridge-gfortran-6.3.0-gcc-6.3.0
    build_openmp_onoff $CONFIG $LIBDIR all_libs


fi