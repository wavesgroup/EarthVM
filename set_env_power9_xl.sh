#!/bin/bash
#
# HDF5 and NETCDF must be already set in the environment.
# To enable building ESMF shared libraries, see the note in README.md.

export WRF=`pwd`/wrf-4.2.1
export UMWM=`pwd`/umwm/src
export HYCOM=`pwd`/hycom-2.3.01

# ESMF configuration
export ESMF_OS=Linux
export ESMF_DIR=`pwd`/esmf
export ESMF_COMPILER=xlf
export ESMF_F90COMPILER=mpixlf
export ESMF_CXXCOMPILER=mpixlc
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=mpi
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF
export ESMF_YAMLCPP=OFF

export ESMF_LIB=${ESMF_DIR}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.default
export ESMF_INCLUDE=${ESMF_DIR}/mod/mod${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.default

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ESMF_LIB
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$WRF_LIB
