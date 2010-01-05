# Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

include ../../include/common.make

# Two copies of the library are compiled: one for single precision and one for double.
# They are built within directories named `float' and `double' respectively.

DATATYPES = float double
SUNDIALS = sundials-2.4.0

SUNDIALS_URI = https://computation.llnl.gov/casc/sundials/download/code/$(SUNDIALS).tar.gz
SUNDIALS_TGZ := $(wildcard /shared/simEngine_support/$(SUNDIALS).tar.gz)
ifeq (,$(SUNDIALS_TGZ))
SUNDIALS_TGZ = /var/tmp/$(SUNDIALS).tar.gz
endif

CVODE_PATH = $(SUNDIALS)/src
CVODE_OBJS = $(CVODE_PATH)/cvode/.libs/cvode_dense.o \
	$(CVODE_PATH)/cvode/.libs/cvode_direct.o \
	$(CVODE_PATH)/cvode/.libs/cvode_io.o \
	$(CVODE_PATH)/cvode/.libs/cvode.o \
        $(CVODE_PATH)/cvode/.libs/cvode_diag.o \
        $(CVODE_PATH)/cvode/.libs/cvode_band.o \
	$(CVODE_PATH)/nvec_ser/.libs/nvector_serial.o \
	$(CVODE_PATH)/sundials/.libs/sundials_dense.o \
	$(CVODE_PATH)/sundials/.libs/sundials_direct.o \
	$(CVODE_PATH)/sundials/.libs/sundials_math.o \
	$(CVODE_PATH)/sundials/.libs/sundials_nvector.o \
	$(CVODE_PATH)/sundials/.libs/sundials_band.o \

# Not all of the following are needed, but this is a good start to trimming closer to what we do need
CVODE_HEADERS = \
	sundials/sundials_types.h \
	sundials/sundials_math.h \
	sundials/sundials_nvector.h \
	sundials/sundials_fnvector.h \
	sundials/sundials_dense.h \
	sundials/sundials_direct.h \
	sundials/sundials_band.h \
	sundials/sundials_lapack.h \
	sundials/sundials_spgmr.h \
	sundials/sundials_spbcgs.h \
	sundials/sundials_sptfqmr.h \
	sundials/sundials_iterative.h \
	nvector/nvector_serial.h \
	nvector/nvector_parallel.h \
	cvode/cvode.h \
	cvode/cvode_direct.h \
	cvode/cvode_dense.h \
	cvode/cvode_band.h \
	cvode/cvode_lapack.h \
	cvode/cvode_diag.h \
	cvode/cvode_spgmr.h \
	cvode/cvode_spbcgs.h \
	cvode/cvode_sptfqmr.h \
	cvode/cvode_spils.h \
	cvode/cvode_bandpre.h \
	cvode/cvode_bbdpre.h

BUILD_DEPENDENCIES = $(SUNDIALS)/Makefile.in \
	$(SUNDIALS)/configure.ac \
	$(SUNDIALS)/acinclude.m4 \
	$(SUNDIALS)/config.hin \
	$(SUNDIALS)/configure \
	$(SUNDIALS)/config/config.guess \
	$(SUNDIALS)/config/config.sub \
	$(SUNDIALS)/config/install-sh \
	$(SUNDIALS)/config/mkinstalldirs \
	$(SUNDIALS)/config/rminstalldirs \
	$(SUNDIALS)/config/ltmain.sh \
	$(SUNDIALS)/config/mod_c.m4 \
	$(SUNDIALS)/config/mod_fortran.m4 \
	$(SUNDIALS)/config/cust_general.m4 \
	$(SUNDIALS)/config/CheckFortranFunctionExists.cmake \
	$(SUNDIALS)/config/SundialsFortran.cmake \
	$(SUNDIALS)/config/SundialsLapack.cmake \
	$(SUNDIALS)/config/SundialsMPIC.cmake \
	$(SUNDIALS)/config/SundialsMPIF.cmake \
	$(SUNDIALS)/config/FindLAPACK.cmake \
	$(SUNDIALS)/config/FindBLAS.cmake \
	$(SUNDIALS)/config/FindMPI.cmake \
	$(SUNDIALS)/bin/sundials-config.in \
	$(SUNDIALS)/bin/fortran-update.in \
	$(SUNDIALS)/bin/makefile-update.in \
	$(addprefix $(SUNDIALS)/include/,$(CVODE_HEADERS)) \
	$(SUNDIALS)/include/sundials/sundials_config.in \
	$(SUNDIALS)/src/sundials/Makefile.in \
	$(SUNDIALS)/src/sundials/CMakeLists.txt \
	$(SUNDIALS)/src/sundials/sundials_math.c \
	$(SUNDIALS)/src/sundials/sundials_nvector.c \
	$(SUNDIALS)/src/sundials/sundials_dense.c \
	$(SUNDIALS)/src/sundials/sundials_direct.c \
	$(SUNDIALS)/src/sundials/sundials_band.c \
	$(SUNDIALS)/src/sundials/sundials_spgmr.c \
	$(SUNDIALS)/src/sundials/sundials_spbcgs.c \
	$(SUNDIALS)/src/sundials/sundials_sptfqmr.c \
	$(SUNDIALS)/src/sundials/sundials_iterative.c \
	$(SUNDIALS)/src/nvec_ser/Makefile.in \
	$(SUNDIALS)/src/nvec_ser/CMakeLists.txt \
	$(SUNDIALS)/src/nvec_ser/fnvector_serial.h \
	$(SUNDIALS)/src/nvec_ser/nvector_serial.c \
	$(SUNDIALS)/src/nvec_ser/fnvector_serial.c \
	$(SUNDIALS)/src/cvode/Makefile.in \
	$(SUNDIALS)/src/cvode/CMakeLists.txt \
	$(SUNDIALS)/src/cvode/cvode.c \
	$(SUNDIALS)/src/cvode/cvode_io.c \
	$(SUNDIALS)/src/cvode/cvode_direct.c \
	$(SUNDIALS)/src/cvode/cvode_dense.c \
	$(SUNDIALS)/src/cvode/cvode_band.c \
	$(SUNDIALS)/src/cvode/cvode_lapack.c \
	$(SUNDIALS)/src/cvode/cvode_diag.c \
	$(SUNDIALS)/src/cvode/cvode_spils.c \
	$(SUNDIALS)/src/cvode/cvode_spgmr.c \
	$(SUNDIALS)/src/cvode/cvode_spbcgs.c \
	$(SUNDIALS)/src/cvode/cvode_sptfqmr.c \
	$(SUNDIALS)/src/cvode/cvode_bandpre.c \
	$(SUNDIALS)/src/cvode/cvode_bbdpre.c \
	$(SUNDIALS)/src/cvode/cvode_impl.h \
	$(SUNDIALS)/src/cvode/cvode_direct_impl.h \
	$(SUNDIALS)/src/cvode/cvode_diag_impl.h \
	$(SUNDIALS)/src/cvode/cvode_spils_impl.h \
	$(SUNDIALS)/src/cvode/cvode_bandpre_impl.h \
	$(SUNDIALS)/src/cvode/cvode_bbdpre_impl.h 

SUNDIALS_CONFIGURE_FLAGS = $(SUNDIALS_CONFIGURE_PRECISION) \
	--disable-static --enable-shared \
	--with-cflags="$(CFLAGS) $(TARGET_ARCH)" 

# Final products are created in the parent directory.
PRODUCT_DIRS = ../lib ../include/float/sundials ../include/float/nvector ../include/float/cvode ../include/double/sundials ../include/double/nvector ../include/double/cvode
PRODUCT_LIBS = $(addsuffix .a,$(addprefix ../lib/libcvode_,$(DATATYPES)))
PRODUCT_HEADERS = $(addprefix ../include/,$(addprefix float/,$(CVODE_HEADERS)) $(addprefix double/,$(CVODE_HEADERS)))



.PHONY: all clean
all: $(PRODUCT_DIRS) $(PRODUCT_HEADERS) $(PRODUCT_LIBS)

clean:
	$(RM) libcvode_float.a libcvode_double.a
	$(RM) float double

$(PRODUCT_DIRS):
	[ -d $@ ] || $(MKDIR) $@

# PRODUCT_HEADERS
../include/float/sundials/%.h: float/$(SUNDIALS)/include/sundials/%.h
	cp $< $@
../include/float/nvector/%.h: float/$(SUNDIALS)/include/nvector/%.h
	cp $< $@
../include/float/cvode/%.h: float/$(SUNDIALS)/include/cvode/%.h
	cp $< $@
../include/double/sundials/%.h: double/$(SUNDIALS)/include/sundials/%.h
	cp $< $@
../include/double/nvector/%.h: double/$(SUNDIALS)/include/nvector/%.h
	cp $< $@
../include/double/cvode/%.h: double/$(SUNDIALS)/include/cvode/%.h
	cp $< $@

# PRODUCT_LIBS
../lib/libcvode_%.a: %/libcvode.a
	cp $< $@

%/libcvode.a: %
	$(MAKE) -C $< -I $(CURDIR) -f $(realpath $(firstword $(MAKEFILE_LIST))) libcvode.a

libcvode.a: $(CVODE_OBJS)
	$(AR) $@ $?

# CVODE_OBJS
$(CVODE_PATH)/cvode/.libs/%.o: $(SUNDIALS)
$(CVODE_PATH)/nvec_ser/.libs/%.o: $(SUNDIALS)
$(CVODE_PATH)/sundials/.libs/%.o: $(SUNDIALS)

# Single-precision build within subdirectory
float: $(SUNDIALS_TGZ)
	[ -d $@ ] || $(MKDIR) $@
	$(MAKE) -C $@ -I $(CURDIR) -f $(realpath $(firstword $(MAKEFILE_LIST))) SUNDIALS_CONFIGURE_PRECISION="--with-precision=single" $(SUNDIALS)

# Double-precision build within subdirectory
double: $(SUNDIALS_TGZ)
	[ -d $@ ] || $(MKDIR) $@
	$(MAKE) -C $@ -I $(CURDIR) -f $(realpath $(firstword $(MAKEFILE_LIST))) SUNDIALS_CONFIGURE_PRECISION="--with-precision=double" $(SUNDIALS)

$(SUNDIALS): CONFIGURE_FLAGS += $(SUNDIALS_CONFIGURE_FLAGS)
$(SUNDIALS): $(SUNDIALS)/Makefile
	$(MAKE) -C $@

$(SUNDIALS)/Makefile: $(SUNDIALS_TGZ)
	tar xf $< $(BUILD_DEPENDENCIES)
	cd $(SUNDIALS); $(CONFIGURE) $(CONFIGURE_FLAGS)

