# EarthVM configuration for GNU compilers

OMPI_FC = gfortran
FC = mpif90
#FFLAGS = -march=native -ffast-math -Wall -funroll-loops -fconvert=big-endian # optimized
FFLAGS = -Wall -O0 -g -fbacktrace -fbounds-check -fconvert=big-endian # debug
HYCOM_ARCH = intelGF-impi-sm-relo
