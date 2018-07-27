# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_fieldsfile_class.so \
 ${LIBDIR_OUT}/lib/libshum_fieldsfile_class.a

${LIBTARGETS}: libshum_fieldsfile_class.so libshum_fieldsfile_class.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_fieldsfile_class
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_file.o: f_shum_file.f90 f_shum_field.o f_shum_ff_status.o
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_field.o: f_shum_field.f90 f_shum_ff_status.o
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_ff_status.o: f_shum_ff_status.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

libshum_fieldsfile_class.a: \
	f_shum_file.o f_shum_field.o f_shum_ff_status.o ${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_file_PIC.o: f_shum_file.f90 f_shum_field_PIC.o f_shum_ff_status_PIC.o
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_field_PIC.o: f_shum_field.f90 f_shum_ff_status_PIC.o
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_ff_status_PIC.o: f_shum_ff_status.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_fieldsfile_class.so: \
	f_shum_file_PIC.o f_shum_field_PIC.o f_shum_ff_status_PIC.o ${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}

