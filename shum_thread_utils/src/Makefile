# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_thread_utils.so \
 ${LIBDIR_OUT}/lib/libshum_thread_utils.a

${LIBTARGETS}: libshum_thread_utils.so libshum_thread_utils.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Compiler Specific Overrides
F_SHUM_THREAD_UTILS_OVERRIDE ?=

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_thread_utils
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_thread_utils.o: f_shum_thread_utils.f90
	${FC} ${FCFLAGS} ${F_SHUM_THREAD_UTILS_OVERRIDE} -c -I${LIBDIR_OUT}/include $<

libshum_thread_utils.a: f_shum_thread_utils.o ${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_thread_utils_PIC.o: f_shum_thread_utils.f90
	${FC} ${FCFLAGS} ${F_SHUM_THREAD_UTILS_OVERRIDE} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_thread_utils.so: f_shum_thread_utils_PIC.o ${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}

