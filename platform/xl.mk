# EarthVM Makefile for XL compiler on Power9

OMPI_FC = xlf2008
FC = mpif90
FFLAGS = -O3 -qstrict # optimized
#FFLAGS = -O0 -g9 -qtbtable=full -qcheck=all # debug
HYCOM_ARCH = power9-xl-smpi-relo
