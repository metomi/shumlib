# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_spiral_search.so \
 ${LIBDIR_OUT}/lib/libshum_spiral_search.a

${LIBTARGETS}: libshum_spiral_search.so libshum_spiral_search.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_spiral_search
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_spiral_search.o: f_shum_spiral_search.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

c_shum_spiral_search.o: c_shum_spiral_search.f90 f_shum_spiral_search.o
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

libshum_spiral_search.a: \
        f_shum_spiral_search.o \
	c_shum_spiral_search.o \
	${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_spiral_search_PIC.o: f_shum_spiral_search.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

c_shum_spiral_search_PIC.o: c_shum_spiral_search.f90 f_shum_spiral_search_PIC.o
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_spiral_search.so: \
	f_shum_spiral_search_PIC.o \
	c_shum_spiral_search_PIC.o \
	${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean:
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}
