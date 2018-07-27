# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_fieldsfile.so \
 ${LIBDIR_OUT}/lib/libshum_fieldsfile.a

${LIBTARGETS}: libshum_fieldsfile.so libshum_fieldsfile.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_fieldsfile
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_fieldsfile.o: f_shum_fieldsfile.f90 f_shum_lookup_indices.o \
                     f_shum_fixed_length_header_indices.o
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_lookup_indices.o: f_shum_lookup_indices.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_fixed_length_header_indices.o: f_shum_fixed_length_header_indices.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_stashmaster.o: f_shum_stashmaster.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

libshum_fieldsfile.a: \
	f_shum_fieldsfile.o f_shum_lookup_indices.o \
        f_shum_fixed_length_header_indices.o \
	f_shum_stashmaster.o ${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_fieldsfile_PIC.o: f_shum_fieldsfile.f90 f_shum_lookup_indices_PIC.o \
                         f_shum_fixed_length_header_indices_PIC.o
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_lookup_indices_PIC.o: f_shum_lookup_indices.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_fixed_length_header_indices_PIC.o: f_shum_fixed_length_header_indices.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_stashmaster_PIC.o: f_shum_stashmaster.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_fieldsfile.so: \
	f_shum_fieldsfile_PIC.o f_shum_lookup_indices_PIC.o \
	f_shum_fixed_length_header_indices_PIC.o \
	f_shum_stashmaster_PIC.o ${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}

