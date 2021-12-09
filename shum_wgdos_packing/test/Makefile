# Makefile for FRUIT testing modules; builds the object files suitable for
# inclusion in either a static (.a) or dynamic (.so) test executable, to be
# picked up by a later make task
#-------------------------------------------------------------------------------
.PHONY: test
test: fruit_test_shum_wgdos_packing.o fruit_test_shum_wgdos_packing_PIC.o

# Static version
#-------------------------------------------------------------------------------
fruit_test_shum_wgdos_packing.o: fruit_test_shum_wgdos_packing.f90  ${LIBDIR_OUT}/lib/libshum_wgdos_packing.a
	${FC} -c ${FCFLAGS_STATIC} ${FCFLAGS} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_STATIC_TRAIL}

# Dynamic version
#-------------------------------------------------------------------------------
fruit_test_shum_wgdos_packing_PIC.o: fruit_test_shum_wgdos_packing.f90  ${LIBDIR_OUT}/lib/libshum_wgdos_packing.so
	${FC} -c ${FCFLAGS_DYNAMIC} ${FCFLAGS} ${FCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_DYNAMIC_TRAIL}

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f *.o *.mod || :
