# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_constants.so \
 ${LIBDIR_OUT}/lib/libshum_constants.a

${LIBTARGETS}: libshum_constants.so libshum_constants.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include
	cp ${COMMON_DIR}/shumlib_version.h ${LIBDIR_OUT}/include

# Include precision bomb and version reporting information
#-------------------------------------------------------------------------------
VERSION_LIBNAME=shum_constants
include ${COMMON_DIR}/Makefile-version

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_chemistry_constants_mod.o: f_shum_chemistry_constants_mod.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_conversions_mod.o: f_shum_conversions_mod.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_planet_earth_constants_mod.o: f_shum_planet_earth_constants_mod.f90 \
        f_shum_conversions_mod.o
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_rel_mol_mass_mod.o: f_shum_rel_mol_mass_mod.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<

f_shum_water_constants_mod.o: f_shum_water_constants_mod.f90
	${FC} ${FCFLAGS} -c -I${LIBDIR_OUT}/include $<


libshum_constants.a: \
	f_shum_chemistry_constants_mod.o f_shum_conversions_mod.o \
        f_shum_planet_earth_constants_mod.o \
        f_shum_rel_mol_mass_mod.o f_shum_water_constants_mod.o \
	${VERSION_OBJECTS}
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_chemistry_constants_mod_PIC.o: f_shum_chemistry_constants_mod.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_conversions_mod_PIC.o: f_shum_conversions_mod.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_planet_earth_constants_mod_PIC.o: f_shum_planet_earth_constants_mod.f90 \
        f_shum_conversions_mod_PIC.o
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_rel_mol_mass_mod_PIC.o: f_shum_rel_mol_mass_mod.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_water_constants_mod_PIC.o: f_shum_water_constants_mod.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@


libshum_constants.so: \
	f_shum_chemistry_constants_mod_PIC.o f_shum_conversions_mod_PIC.o \
        f_shum_planet_earth_constants_mod_PIC.o \
        f_shum_rel_mol_mass_mod_PIC.o f_shum_water_constants_mod_PIC.o \
	${VERSION_OBJECTS_PIC}
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a ${VERSION_CLEAN}
