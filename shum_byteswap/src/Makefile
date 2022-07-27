# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_byteswap.so \
 ${LIBDIR_OUT}/lib/libshum_byteswap.a

${LIBTARGETS}: libshum_byteswap.so libshum_byteswap.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_byteswap
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
c_shum_byteswap.o: c_shum_byteswap.c c_shum_byteswap.h c_shum_byteswap_opt.h ${COMMON_DIR}/c_shum_compile_diag_suspend.h
	${CC} ${CCFLAGS} -c -I${LIBDIR_OUT}/include -I${COMMON_DIR} $<

f_shum_byteswap.o: f_shum_byteswap.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

libshum_byteswap.a: c_shum_byteswap.o f_shum_byteswap.o ${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
c_shum_byteswap_PIC.o: c_shum_byteswap.c c_shum_byteswap.h c_shum_byteswap_opt.h ${COMMON_DIR}/c_shum_compile_diag_suspend.h
	${CC} ${CCFLAGS} ${CCFLAGS_PIC} -c -I${LIBDIR_OUT}/include -I${COMMON_DIR} $< -o $@

f_shum_byteswap_PIC.o: f_shum_byteswap.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_byteswap.so: c_shum_byteswap_PIC.o f_shum_byteswap_PIC.o ${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}

