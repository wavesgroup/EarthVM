.PHONY: all clean
.SUFFIXES: .f90 .o

LIB = libearthvm.a

OBJS = earthvm_assert.o \
       earthvm_constants.o \
       earthvm_coupling.o \
       earthvm_datetime.o \
       earthvm_esmf.o \
       earthvm_hycom.o \
       earthvm_io.o \
       earthvm_model.o \
       earthvm_regrid.o \
       earthvm_state.o \
       earthvm_umwm.o \
       earthvm_wrf.o

all: $(LIB)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) *.a *.o *.mod PET*

earthvm_esmf.o: earthvm_assert.o earthvm_constants.o earthvm_state.o
earthvm_hycom.o: earthvm_assert.o earthvm_io.o earthvm_state.o
earthvm_io.o: earthvm_assert.o earthvm_state.o
earthvm_model.o: earthvm_assert.o earthvm_datetime.o earthvm_esmf.o \
                 earthvm_io.o earthvm_regrid.o earthvm_state.o
earthvm_regrid.o: earthvm_assert.o
earthvm_state.o: earthvm_assert.o
earthvm_umwm.o: earthvm_assert.o earthvm_io.o earthvm_state.o
earthvm_wrf.o: earthvm_assert.o earthvm_constants.o earthvm_esmf.o earthvm_io.o earthvm_model.o earthvm_state.o
