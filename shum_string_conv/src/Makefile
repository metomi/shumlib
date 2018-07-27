# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_string_conv.so \
 ${LIBDIR_OUT}/lib/libshum_string_conv.a

${LIBTARGETS}: libshum_string_conv.so libshum_string_conv.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_string_conv
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_string_conv.o: f_shum_string_conv.f90
	${FC} ${FCFLAGS} -c $<

libshum_string_conv.a: f_shum_string_conv.o ${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_string_conv_PIC.o: f_shum_string_conv.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c $< -o $@

libshum_string_conv.so: f_shum_string_conv_PIC.o ${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}
