.PHONY: all clean
.SUFFIXES: .f90 .o

LIB = libearthvm.a

OBJS = earthvm_assert.o \
       earthvm_coupling.o \
       earthvm_datetime.o \
       earthvm_esmf.o \
       earthvm_events.o \
       earthvm_hycom.o \
       earthvm_io.o \
       earthvm_model.o \
       earthvm_regrid.o \
       earthvm_state.o \
       earthvm_str.o \
       earthvm_umwm.o \
       earthvm_wrf.o

PROGS = main_awo main_ao main_aw

all: $(LIB) $(PROGS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

main_awo: main_awo.f90 $(OBJS)
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<
	$(FC) $(FFLAGS) -o $@ main_awo.o $(OBJS) $(WRF_OBJS) $(LDFLAGS)

main_ao: main_ao.f90 $(OBJS)
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<
	$(FC) $(FFLAGS) -o $@ main_ao.o $(OBJS) $(WRF_OBJS) $(LDFLAGS)

main_aw: main_aw.f90 $(OBJS)
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $<
	$(FC) $(FFLAGS) -o $@ main_aw.o $(OBJS) $(WRF_OBJS) $(LDFLAGS)

clean:
	$(RM) $(PROGS) *.a *.o *.mod PET*

earthvm_esmf.o: earthvm_assert.o earthvm_state.o
earthvm_events.o: earthvm_str.o
earthvm_hycom.o: earthvm_assert.o earthvm_events.o earthvm_state.o earthvm_io.o
earthvm_io.o: earthvm_assert.o earthvm_state.o
earthvm_model.o: earthvm_assert.o earthvm_datetime.o earthvm_esmf.o \
                 earthvm_events.o earthvm_io.o earthvm_regrid.o \
                 earthvm_state.o earthvm_str.o
earthvm_regrid.o: earthvm_assert.o
earthvm_state.o: earthvm_assert.o
earthvm_umwm.o: earthvm_assert.o earthvm_events.o earthvm_state.o earthvm_io.o
earthvm_wrf.o: earthvm_assert.o earthvm_events.o earthvm_state.o earthvm_io.o
