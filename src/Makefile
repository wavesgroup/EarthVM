LIB = libearthvm.a

OBJS = earthvm_assert.o \
       earthvm_datetime.o \
       earthvm_esmf.o \
       earthvm_events.o \
       earthvm_hycom.o \
       earthvm_io.o \
       earthvm_model.o \
       earthvm_regrid.o \
       earthvm_state.o \
       earthvm_str.o \
       earthvm_wrf.o

.PHONY: all clean
.SUFFIXES: .f90 .o

all: $(LIB) main

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<

main: main.f90 $(OBJS)
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<
	$(FC) $(FFLAGS) -o $@ main.o $(OBJS) $(WRF_OBJS) $(LDFLAGS)

clean:
	$(RM) main *.a *.o *.mod PET*

earthvm_esmf.o: earthvm_assert.o earthvm_state.o
earthvm_events.o: earthvm_str.o
earthvm_hycom.o: earthvm_assert.o earthvm_events.o earthvm_state.o earthvm_io.o
earthvm_io.o: earthvm_assert.o earthvm_state.o
earthvm_model.o: earthvm_assert.o earthvm_datetime.o earthvm_esmf.o \
	         earthvm_events.o earthvm_io.o earthvm_regrid.o \
		 earthvm_state.o earthvm_str.o
earthvm_regrid.o: earthvm_assert.o
earthvm_state.o: earthvm_assert.o
earthvm_wrf.o: earthvm_assert.o earthvm_events.o earthvm_state.o earthvm_io.o
