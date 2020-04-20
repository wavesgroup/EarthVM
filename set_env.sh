#!/bin/bash

export WRF_DIR=$(pwd)/wrf-4.2

export HDF5=$(pwd)/netcdf
export NETCDF=$(pwd)/netcdf

# ESMF configuration
export ESMF_OS=Linux
export ESMF_DIR=$(pwd)/esmf-8.0.0
export ESMF_COMPILER=gfortran
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=openmpi
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF

export ESMF_LIB=${ESMF_DIR}/lib/lib${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.default
export ESMF_INCLUDE=${ESMF_DIR}/mod/mod${ESMF_BOPT}/${ESMF_OS}.${ESMF_COMPILER}.${ESMF_ABI}.${ESMF_COMM}.default

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ESMF_LIB
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$WRF_LIB
