# EarthVM

EarthVM is a lightweight parallel framework for running and coupling weather, ocean, and wave models.

## Getting started

### Set up environment variables

Set these environment variables before building ESMF:

```
export ESMF_OS=Linux
export ESMF_DIR=$(pwd)/esmf-8.0.0
export ESMF_COMPILER=gfortran # change to other compiler if needed
export ESMF_ABI=64
export ESMF_BOPT=O
export ESMF_COMM=openmpi # change to other MPI library if needed
export ESMF_PTHREADS=OFF
export ESMF_OPENMP=OFF
```

### Download and build ESMF

This will download esmf-8.0.0 and print the information about build parameters:

```
git clone -b ESMF_8_0_0 --depth 1 https://git.code.sf.net/p/esmf/esmf esmf-8.0.0
cd esmf-8.0.0
make info 
make
make check
```

### Download and build WRF

```
git clone -b release-v4.2 --depth 1 https://github.com/wrf-model/wrf wrf-4.2
cd wrf-4.2
./configure
./compile em_real
```

### Download and build HYCOM

```
git clone -b 2.3.01 https://github.com/hycom/hycom-src hycom-2.3.01
cd hycom-2.3.01
ARCH=intelGF-impi-sm-relo CPP_EXTRAS="-DEOS_SIG2=1 -DEOS_7T=1" TYPE=mpi make
```

### Download and build UMWM

```
git clone --recursive https://github.com/umwm/umwm
cd umwm
```
