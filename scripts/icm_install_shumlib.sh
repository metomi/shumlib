#!/bin/bash
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
#

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


if [ $PLATFORM == "xc40" ] ; then

    # Crayftn/CrayCC Haswell 8.6.4 (Current system default)
    # - note that these versions of CCE don't work correctly with the 
    # Fieldsfile, so we exclude them
    module list
    CONFIG=icm-xc40-crayftn-craycc
    LIBDIR=$BUILD_DESTINATION/icm-xc40-crayftn-craycc
    build_openmp_onoff $CONFIG $LIBDIR $(sed -e "s/\bshum_fieldsfile_class\b//g" \
                                             -e "s/\bshum_fieldsfile\b//g" \
                                             <<< $LIB_DIRS)
  
    # gfortran/gcc 
    module swap PrgEnv-cray/5.2.82 PrgEnv-gnu/5.2.82
    module list
    CONFIG=icm-xc40-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/icm-xc40-gfortran-gcc
    build_openmp_onoff $CONFIG $LIBDIR all_libs

fi
