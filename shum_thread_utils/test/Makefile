# Makefile for FRUIT testing modules; builds the object files suitable for
# inclusion in either a static (.a) or dynamic (.so) test executable, to be
# picked up by a later make task
#-------------------------------------------------------------------------------
.PHONY: test
test: fruit_test_shum_thread_utils.o fruit_test_shum_thread_utils_PIC.o \
      c_fruit_test_shum_thread_utils.o c_fruit_test_shum_thread_utils_PIC.o

# Static version
#-------------------------------------------------------------------------------
fruit_test_shum_thread_utils.o: fruit_test_shum_thread_utils.f90 ${LIBDIR_OUT}/lib/libshum_thread_utils.a
	${FC} -c ${FCFLAGS_STATIC} ${FCFLAGS} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_STATIC_TRAIL}

c_fruit_test_shum_thread_utils.o: c_fruit_test_shum_thread_utils.c ${LIBDIR_OUT}/lib/libshum_thread_utils.a
	${CC} -c ${CCFLAGS_STATIC} ${CCFLAGS} $< \
	-I${LIBDIR_OUT}/include -o $@ ${CCFLAGS_STATIC_TRAIL}

# Dynamic version
#-------------------------------------------------------------------------------
fruit_test_shum_thread_utils_PIC.o: fruit_test_shum_thread_utils.f90 ${LIBDIR_OUT}/lib/libshum_thread_utils.so
	${FC} -c ${FCFLAGS_DYNAMIC} ${FCFLAGS} ${FCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -o $@ ${FCFLAGS_DYNAMIC_TRAIL}

c_fruit_test_shum_thread_utils_PIC.o: c_fruit_test_shum_thread_utils.c ${LIBDIR_OUT}/lib/libshum_thread_utils.so
	${CC} -c ${CCFLAGS_DYNAMIC} ${CCFLAGS} ${CCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -o $@ ${CCFLAGS_DYNAMIC_TRAIL}

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f *.o *.mod
