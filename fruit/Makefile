# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libfruit.so \
 ${LIBDIR_OUT}/lib/libfruit.a

${LIBTARGETS}: libfruit.so libfruit.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.mod ${LIBDIR_OUT}/include

fruit.f90: fruit_f90_source.txt fruit_f90_generator.rb
	./fruit_f90_generator.rb fruit_f90_source.txt

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
fruit.o: fruit.f90
	${FC} ${FCFLAGS} -c $< -o $@

libfruit.a: fruit.o
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
fruit_PIC.o: fruit.f90
	${FC} ${FCFLAGS} ${FCFLAGS_PIC} -c $< -o $@

libfruit.so: fruit_PIC.o 
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a