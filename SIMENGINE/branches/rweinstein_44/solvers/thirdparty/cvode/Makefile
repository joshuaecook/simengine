$(info ARCH64 is defined as $(ARCH64))
DATATYPES = float double
SUNDIALS = sundials-2.4.0
SUNDIALS_PATH = /shared/simEngine_support/$(SUNDIALS).tar.gz
SUNDIALS_BASE_PATH = $(TYPE)
SUNDIALS_BUILD_PATH = $(SUNDIALS_BASE_PATH)/$(SUNDIALS)
CVODE_PATH = $(SUNDIALS_BUILD_PATH)/src
CVODE_OBJS = $(CVODE_PATH)/cvode/.libs/cvode_dense.o \
	$(CVODE_PATH)/cvode/.libs/cvode_direct.o \
	$(CVODE_PATH)/cvode/.libs/cvode_io.o \
	$(CVODE_PATH)/cvode/.libs/cvode.o \
	$(CVODE_PATH)/nvec_ser/.libs/nvector_serial.o \
	$(CVODE_PATH)/sundials/.libs/sundials_dense.o \
	$(CVODE_PATH)/sundials/.libs/sundials_direct.o \
	$(CVODE_PATH)/sundials/.libs/sundials_math.o \
	$(CVODE_PATH)/sundials/.libs/sundials_nvector.o 

# Not all of the following are needed, but this is a good start to trimming closer to what we do need
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
	$(SUNDIALS)/include/sundials/sundials_config.in \
	$(SUNDIALS)/include/sundials/sundials_types.h \
	$(SUNDIALS)/include/sundials/sundials_math.h \
	$(SUNDIALS)/include/sundials/sundials_nvector.h \
	$(SUNDIALS)/include/sundials/sundials_fnvector.h \
	$(SUNDIALS)/include/sundials/sundials_dense.h \
	$(SUNDIALS)/include/sundials/sundials_direct.h \
	$(SUNDIALS)/include/sundials/sundials_band.h \
	$(SUNDIALS)/include/sundials/sundials_lapack.h \
	$(SUNDIALS)/include/sundials/sundials_spgmr.h \
	$(SUNDIALS)/include/sundials/sundials_spbcgs.h \
	$(SUNDIALS)/include/sundials/sundials_sptfqmr.h \
	$(SUNDIALS)/include/sundials/sundials_iterative.h \
	$(SUNDIALS)/include/nvector/nvector_serial.h \
	$(SUNDIALS)/include/nvector/nvector_parallel.h \
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
	$(SUNDIALS)/include/cvode/cvode.h \
	$(SUNDIALS)/include/cvode/cvode_direct.h \
	$(SUNDIALS)/include/cvode/cvode_dense.h \
	$(SUNDIALS)/include/cvode/cvode_band.h \
	$(SUNDIALS)/include/cvode/cvode_lapack.h \
	$(SUNDIALS)/include/cvode/cvode_diag.h \
	$(SUNDIALS)/include/cvode/cvode_spgmr.h \
	$(SUNDIALS)/include/cvode/cvode_spbcgs.h \
	$(SUNDIALS)/include/cvode/cvode_sptfqmr.h \
	$(SUNDIALS)/include/cvode/cvode_spils.h \
	$(SUNDIALS)/include/cvode/cvode_bandpre.h \
	$(SUNDIALS)/include/cvode/cvode_bbdpre.h \
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
	$(SUNDIALS)/src/cvode/cvode_bbdpre_impl.h \


AR = ar -rs

all:
	for type in $(DATATYPES); do \
	  $(MAKE) TYPE=$$type libcvode_$$type\.a; \
	done

libcvode_$(TYPE).a: $(CVODE_OBJS)
	$(AR) $@ $?

clean-cvode:
	rm -rf $(SUNDIALS_BUILD_PATH)

clean:
	rm -f src/*.o *.linkinfo *.a
	@for type in $(DATATYPES); do \
	  $(MAKE) clean-cvode TYPE=$$type; \
	 done

$(SUNDIALS_BUILD_PATH): # $(SUNDIALS_PATH)
	cd $(SUNDIALS_BASE_PATH); tar xvzf $(SUNDIALS_PATH) $(BUILD_DEPENDENCIES)

# define a special architecture if ARCH64 is defined
ifneq ($(ARCH64),)
CFLAG64 = CFLAGS="-arch x86_64"
endif

# This might be cleaned up a little but it compiles only the objects that we need for our archive
$(CVODE_OBJS): $(SUNDIALS_BUILD_PATH)
	if [[ $(TYPE) == double ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=double $(CFLAG64); \
	 cd src/cvode; $(MAKE) cvode_dense.lo cvode_direct.lo cvode_io.lo cvode.lo; \
	 cd ../nvec_ser; $(MAKE) nvector_serial.lo; \
	 cd ../sundials; $(MAKE) sundials_dense.lo sundials_direct.lo sundials_math.lo sundials_nvector.lo; \
	elif  [[ $(TYPE) == float ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=single $(CFLAG64); \
	 cd src/cvode; $(MAKE) cvode_dense.lo cvode_direct.lo cvode_io.lo cvode.lo; \
	 cd ../nvec_ser; $(MAKE) nvector_serial.lo; \
	 cd ../sundials; $(MAKE) sundials_dense.lo sundials_direct.lo sundials_math.lo sundials_nvector.lo; \
	fi

