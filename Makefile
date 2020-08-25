# EarthVM Makefile

#OMPI_FC=gfortran
#OMPI_FC=ifort
OMPI_FC=xlf2008

FC = mpif90

# gfortran, debug
#FFLAGS = -Wall -O0 -g -fbacktrace -fbounds-check -fconvert=big-endian # big-endian needed for WRF

# gfortran, optimized
#FFLAGS = -march=native -ffast-math -Wall -funroll-loops -fconvert=big-endian

# ifort, optimized
#FFLAGS = -O3 -convert big_endian

# Power9, XL
FFLAGS = -O3 -qstrict
#FFLAGS = -O0 -g9 -qtbtable=full -qcheck=all

HYCOM_ARCH=xl-smpi-sm-relo # XL with Spectrum MPI on IBM Power9
#HYCOM_ARCH=intelGF-impi-sm-relo # gfortran on Intel x86
#HYCOM_ARCH=intelsse-impi-sm-relo # ifort on Intel x86

ESMF_LINK_FLAGS = $(ESMF_F90LINKPATHS) \
                  $(ESMF_F90ESMFLINKLIBS) \
                  $(ESMF_F90LINKOPTS) \
                  $(ESMF_CXXLINKOPTS) \
                  $(ESMF_TRACE_STATICLINKOPTS)

CPPFLAGS = -I$(ESMF_INCLUDE) \
	   -I$(WRF)/external/esmf_time_f90 \
           -I$(WRF)/frame \
           -I$(WRF)/main \
           -I$(WRF)/phys \
           -I$(WRF)/share \
	   -I$(UMWM) \
	   -I$(HYCOM) \
	   -I$(NETCDF)/include

LDFLAGS = -L$(WRF)/external/esmf_time_f90 -lesmf_time \
	  -L$(WRF)/external/io_netcdf -lwrfio_nf \
	  -L$(UMWM) -lumwm \
	  -L$(HYCOM) -lhycom \
          -L$(ESMF_LIB) -lesmf \
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

export FC OMPI_FC FFLAGS CPPFLAGS LDFLAGS WRF_OBJS

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

download_umwm:
	git clone https://github.com/umwm/umwm

hycom:
	cd $(HYCOM) && ARCH=$(HYCOM_ARCH) CPP_EXTRAS="-DEOS_SIG2=1 -DEOS_7T=1 -DEARTHVM -DSTOKES" TYPE=mpi make
	ar rcs $(HYCOM)/libhycom.a $(HYCOM)/*.o

umwm:
	CPPFLAGS="-DMPI -DESMF" FCFLAGS="-O3 -qstrict" $(MAKE) umwm --directory=umwm
