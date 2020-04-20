# EarthVM tests Makefile
#
# Any .f90 file placed here must be a program,
# and will be built and run as a test.

SRC := $(shell find . -name '*.f90')
OBJS = $(SRC:.f90=.o)
PROGS = $(OBJS:.o=)
TESTS = $(PROGS:=TEST)
CPPFLAGS += -I../src

EARTHVM_LIB = ../src/libearthvm.a

.PHONY: all clean test $(TESTS)
.SUFFIXES: .f90 .o

all: $(PROGS)

test: $(TESTS) 

$(PROGS): %: %.o
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^ $(EARTHVM_LIB) $(WRF_OBJS) $(LDFLAGS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<

$(TESTS):
	./$(@:TEST=)

clean:
	$(RM) $(PROGS) *.o *.ESMF_LogFile
