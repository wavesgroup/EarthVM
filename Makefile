# EarthVM Makefile

FC = mpif90
FFLAGS = -O0 -g -fbacktrace -fconvert=big-endian # big-endian needed for WRF
CPPFLAGS = -I$(ESMF_INCLUDE) -I$(WRF_DIR)/main -I$(NETCDF)/include
LDFLAGS = -L$(WRF_DIR)/external/esmf_time_f90 -lesmf_time \
	  -L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
          -L$(ESMF_LIB) -lesmf_fullylinked \
          -L$(NETCDF)/lib -lnetcdff

WRF_OBJS = $(WRF_DIR)/main/module_wrf_top.o \
	   $(WRF_DIR)/frame/module_internal_header_util.o \
	   $(WRF_DIR)/frame/pack_utils.o \
	   $(WRF_DIR)/main/libwrflib.a \
	   $(WRF_DIR)/external/fftpack/fftpack5/libfftpack.a \
	   $(WRF_DIR)/external/io_grib1/libio_grib1.a \
	   $(WRF_DIR)/external/io_grib_share/libio_grib_share.a \
	   $(WRF_DIR)/external/io_int/libwrfio_int.a \
	   $(WRF_DIR)/external/RSL_LITE/librsl_lite.a

export FC FFLAGS CPPFLAGS LDFLAGS WRF_OBJS

.PHONY: all test clean

all:
	$(MAKE) --directory=src
	$(MAKE) --directory=tests

test:
	$(MAKE) test --directory=tests

clean:
	$(MAKE) clean --directory=src
	$(MAKE) clean --directory=tests
