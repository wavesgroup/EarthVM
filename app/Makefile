# EarthVM tests Makefile
#
# Any .f90 file placed here must be a program,
# and will be built and run as an application.

SRC := $(shell find . -name '*.f90')
OBJS = $(SRC:.f90=.o)
PROGS = $(OBJS:.o=)
CPPFLAGS += -I../src

EARTHVM_LIB = ../src/libearthvm.a

.PHONY: all clean $(PROGS)
.SUFFIXES: .f90 .o

all: $(PROGS)

$(PROGS): %: %.o
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^ $(EARTHVM_LIB) $(WRF_OBJS) $(LDFLAGS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<

clean:
	$(RM) $(PROGS) *.o
