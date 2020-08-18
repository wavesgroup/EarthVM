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
make download_esmf
cd esmf
make info 
make
make check
```

### Build WRF

```
cd wrf-4.2-1
./configure
./compile em_real
```

### Download and build UMWM

```
make download_umwm
make umwm
```

### Build HYCOM

```
make hycom
```

### Build EarthVM

```
make
```

### Run tests

```
make test
```

## Notes

* Building with Intel compilers requires ESMF <= v8.0.0
due to unsupported C++ features.
* Building WRF on ppc64 with at-12.0 (IBM Advanced Toolchain) 
requires passing `-Uvector` to cpp.
* To build ESMF shared libraries with XL on IBM Power9, the following
must be added to `build_config/Linux.xlf.default/build_rules.mk`:

```
ESMF_SL_LIBS_TO_MAKE = libesmf
ESMF_SL_SUFFIX = so
ESMF_SL_LIBLINKER = mpicxx
ESMF_SL_LIBOPTS = -qpic -shared
ESMF_SL_LIBLIBS =
```
