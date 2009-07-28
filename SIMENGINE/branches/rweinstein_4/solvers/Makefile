
METHODS = forwardeuler rk4 bogacki_shampine dormand_prince cvode
DATATYPES = float double
GCCLIB = gcc -g -fPIC -c -Wall -Iinclude -I/opt/include
AR = ar rs

SUNDIALS = sundials-2.4.0
SUNDIALS_PATH = /shared/simEngine_support/$(SUNDIALS).tar.gz
SUNDIALS_BUILD_PATH = thirdparty/cvode/$(SUNDIALS)
CVODE_PATH = thirdparty/cvode/$(TYPE)
CVODE_OBJS = $(CVODE_PATH)/cvode_dense.o $(CVODE_PATH)/cvode_direct.o $(CVODE_PATH)/cvode_io.o $(CVODE_PATH)/cvode.o $(CVODE_PATH)/nvector_serial.o $(CVODE_PATH)/sundials_dense.o $(CVODE_PATH)/sundials_direct.o $(CVODE_PATH)/sundials_math.o $(CVODE_PATH)/sundials_nvector.o 

all:
	@$(MAKE) cvode
	@for type in $(DATATYPES); do \
	  for method in $(METHODS); do \
	   make src/$$method\_$$type.o METHOD=$$method TYPE=$$type; \
	  done; \
	  make lib/solvers\_$$type.a TYPE=$$type; \
	 done

src/%.o: src/$(METHOD).c
	$(GCCLIB) -DCDATAFORMAT=$(TYPE) -o $@ $<

lib/%.a: src/*$(TYPE).o $(CVODE_OBJS) #thirdparty/cvode/$(TYPE)/*.o
	$(AR) $@ $?

clean-cvode:
	rm -f $(CVODE_OBJS)

clean:
	rm -f src/*.o lib/*.a
	@for type in $(DATATYPES); do \
	  $(MAKE) clean-cvode TYPE=$$type; \
	 done

cvode:
	cd thirdparty/cvode; tar xvzf $(SUNDIALS_PATH)
	cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=double; make
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_dense.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_direct.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_io.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/nvec_ser/.libs/nvector_serial.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_dense.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_direct.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_math.o thirdparty/cvode/double
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_nvector.o thirdparty/cvode/double
	cd $(SUNDIALS_BUILD_PATH); ./configure --disable-static --enable-shared --with-precision=single; make
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_dense.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_direct.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode_io.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/cvode/.libs/cvode.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/nvec_ser/.libs/nvector_serial.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_dense.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_direct.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_math.o thirdparty/cvode/float
	cp $(SUNDIALS_BUILD_PATH)/src/sundials/.libs/sundials_nvector.o thirdparty/cvode/float
