#SUNDIALS = sundials-2.4.0
#SUNDIALS_PATH = /shared/simEngine_support/$(SUNDIALS).tar.gz
#SUNDIALS_BASE_PATH = thirdparty/cvode/$(TYPE)
#SUNDIALS_BUILD_PATH = $(SUNDIALS_BASE_PATH)/$(SUNDIALS)
#CVODE_PATH = $(SUNDIALS_BUILD_PATH)/src
#CVODE_OBJS = $(CVODE_PATH)/cvode/.libs/cvode_dense.o \
	$(CVODE_PATH)/cvode/.libs/cvode_direct.o \
	$(CVODE_PATH)/cvode/.libs/cvode_io.o \
	$(CVODE_PATH)/cvode/.libs/cvode.o \
	$(CVODE_PATH)/nvec_ser/.libs/nvector_serial.o \
	$(CVODE_PATH)/sundials/.libs/sundials_dense.o \
	$(CVODE_PATH)/sundials/.libs/sundials_direct.o \
	$(CVODE_PATH)/sundials/.libs/sundials_math.o \
	$(CVODE_PATH)/sundials/.libs/sundials_nvector.o 

METHODS = forwardeuler rk4 bogacki_shampine dormand_prince #cvode
TARGETS = cpu openmp
DATATYPES = float double
GCCLIB = gcc -g -fPIC -c -Wall -Iinclude -I$(SUNDIALS_BUILD_PATH)/include -I../include
AR = ar rs

all:
#	$(MAKE) create-cvode-library TYPE=$$type
	@for type in $(DATATYPES); do \
	  for target in $(TARGETS); do \
	    for method in $(METHODS); do \
	      $(MAKE) src/$$method\_$$target\_$$type.o METHOD=$$method TARGET=$$target TYPE=$$type; \
	    done; \
	  done;\
	done
	$(MAKE) lib/libsolvers.a

src/$(METHOD)_$(TARGET)_$(TYPE).o: src/$(METHOD).c include/solvers.h
	$(GCCLIB) -DTARGET=$(TARGET) -DSIMENGINE_STORAGE=$(TYPE) -o $@ $<

lib/libsolvers.a: src/*.o #$(CVODE_OBJS)
	$(AR) $@ $?

#clean-cvode:
#	rm -rf $(SUNDIALS_BUILD_PATH)

clean:
	rm -f src/*.o lib/*.a
#	@for type in $(DATATYPES); do \
	  $(MAKE) clean-cvode TYPE=$$type; \
	 done

#$(SUNDIALS_BUILD_PATH): $(SUNDIALS_PATH)
#	cd $(SUNDIALS_BASE_PATH); tar xvzf $(SUNDIALS_PATH)

#build-cvode: $(SUNDIALS_BUILD_PATH)
#	if [[ $(TYPE) == double ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=double; make; \
	elif  [[ $(TYPE) == float ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=single; make; \
	fi

#create-cvode-library: $(CVODE_OBJS)

#$(CVODE_OBJS):
#	$(MAKE) TYPE=$(TYPE) build-cvode
