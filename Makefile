# EarthVM Makefile

FC = mpif90

# gfortran, debug
#FFLAGS = -Wall -O0 -g -fbacktrace -fconvert=big-endian # big-endian needed for WRF

# gfortran, optimized
#FFLAGS = -march=native -ffast-math -Wall -funroll-loops -fconvert=big-endian

# ifort, optimized
FFLAGS = -O3 -convert big_endian

CPPFLAGS = -I$(ESMF_INCLUDE) \
	   -I$(WRF)/external/esmf_time_f90 \
           -I$(WRF)/frame \
           -I$(WRF)/main \
           -I$(WRF)/share \
	   -I$(UMWM) \
	   -I$(HYCOM) \
	   -I$(NETCDF)/include
LDFLAGS = -L$(WRF)/external/esmf_time_f90 -lesmf_time \
	  -L$(WRF)/external/io_netcdf -lwrfio_nf \
	  -L$(UMWM) -lumwm \
	  -L$(HYCOM) -lhycom \
          -L$(ESMF_LIB) -lesmf_fullylinked \
          -L$(NETCDF)/lib -lnetcdff

WRF_OBJS = $(WRF)/main/module_wrf_top.o \
	   $(WRF)/frame/module_internal_header_util.o \
	   $(WRF)/frame/pack_utils.o \
	   $(WRF)/main/libwrflib.a \
	   $(WRF)/external/fftpack/fftpack5/libfftpack.a \
	   $(WRF)/external/io_grib1/libio_grib1.a \
	   $(WRF)/external/io_grib_share/libio_grib_share.a \
	   $(WRF)/external/io_int/libwrfio_int.a \
	   $(WRF)/external/RSL_LITE/librsl_lite.a

export FC FFLAGS CPPFLAGS LDFLAGS WRF_OBJS

.PHONY: all test clean umwm

all: hycom umwm
	$(MAKE) --directory=src
	$(MAKE) --directory=tests

test:
	$(MAKE) test --directory=tests

clean:
	$(MAKE) clean --directory=src
	$(MAKE) clean --directory=tests

clean_hycom:
	$(RM) hycom-2.3.01/hycom
	$(RM) hycom-2.3.01/*.o
	$(RM) hycom-2.3.01/*.a
	$(RM) hycom-2.3.01/*.mod

clean_umwm:
	$(MAKE) clean --directory=umwm

download_esmf:
	git clone -b ESMF_8_0_1 https://github.com/esmf-org/esmf

download_wrf:
	git clone -b v4.2 https://github.com/wrf-model/WRF wrf-4.2

download_umwm:
	git clone https://github.com/umwm/umwm

download_hycom:
	git clone -b 2.3.01 https://github.com/hycom/hycom-src hycom-2.3.01

hycom:
	#cd $(HYCOM) && ARCH=intelGF-impi-sm-relo CPP_EXTRAS="-DEOS_SIG2=1 -DEOS_7T=1 -DEARTHVM -DSTOKES" TYPE=mpi make
	cd $(HYCOM) && ARCH=intelsse-impi-sm-relo CPP_EXTRAS="-DEOS_SIG2=1 -DEOS_7T=1 -DEARTHVM -DSTOKES" TYPE=mpi make
	ar rcs $(HYCOM)/libhycom.a $(HYCOM)/*.o

umwm:
	CPPFLAGS="-DMPI -DESMF" $(MAKE) umwm --directory=umwm
