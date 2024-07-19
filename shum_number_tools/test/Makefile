# Makefile for FRUIT testing modules; builds the object files suitable for
# inclusion in either a static (.a) or dynamic (.so) test executable, to be
# picked up by a later make task
#-------------------------------------------------------------------------------
.PHONY: test
test: fruit_test_shum_number_tools.o fruit_test_shum_number_tools_PIC.o \
      c_fruit_test_shum_number_tools.o c_fruit_test_shum_number_tools_PIC.o

# Static version
#-------------------------------------------------------------------------------
fruit_test_shum_number_tools.o: fruit_test_shum_number_tools.f90 ${LIBDIR_OUT}/lib/libshum_number_tools.a
	${FC} -c ${FCFLAGS_STATIC} ${FCFLAGS} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_STATIC_TRAIL}

c_fruit_test_shum_number_tools.o: c_fruit_test_shum_number_tools.c ${LIBDIR_OUT}/lib/libshum_number_tools.a ${COMMON_DIR}/c_shum_compiler_select.h
	${CC} -c ${CCFLAGS_STATIC} ${CCFLAGS} $< \
	-I${LIBDIR_OUT}/include -I${COMMON_DIR} -o $@ ${CCFLAGS_STATIC_TRAIL}


# Dynamic version
#-------------------------------------------------------------------------------
fruit_test_shum_number_tools_PIC.o: fruit_test_shum_number_tools.f90 ${LIBDIR_OUT}/lib/libshum_number_tools.so
	${FC} -c ${FCFLAGS_DYNAMIC} ${FCFLAGS} ${FCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_DYNAMIC_TRAIL}

c_fruit_test_shum_number_tools_PIC.o: c_fruit_test_shum_number_tools.c ${LIBDIR_OUT}/lib/libshum_number_tools.so ${COMMON_DIR}/c_shum_compiler_select.h
	${CC} -c ${CCFLAGS_DYNAMIC} ${CCFLAGS} ${CCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -I${COMMON_DIR} -o $@ ${CCFLAGS_DYNAMIC_TRAIL}

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f *.o *.mod *.f90

# Preprocess targets
#--------------------------------------------------------------------------------
%.f90: %.F90
	${FPP} ${FPPFLAGS} $< -o $@
