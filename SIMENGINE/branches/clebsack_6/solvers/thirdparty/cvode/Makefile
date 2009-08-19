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
	rm -f src/*.o *.linkinfo lib/*.a
	@for type in $(DATATYPES); do \
	  $(MAKE) clean-cvode TYPE=$$type; \
	 done

$(SUNDIALS_BUILD_PATH): $(SUNDIALS_PATH)
	cd $(SUNDIALS_BASE_PATH); tar xvzf $(SUNDIALS_PATH)

$(CVODE_OBJS): $(SUNDIALS_BUILD_PATH)
	if [[ $(TYPE) == double ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=double; make; \
	elif  [[ $(TYPE) == float ]]; then \
	 cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=single; make; \
	fi
