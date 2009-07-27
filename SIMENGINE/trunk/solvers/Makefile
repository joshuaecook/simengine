
METHODS = forwardeuler rk4 bogacki_shampine dormand_prince cvode
DATATYPES = float double
GCCLIB = gcc -g -fPIC -c -Wall -Iinclude -I/opt/include
AR = ar rs

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

lib/%.a: src/*$(TYPE).o thirdparty/cvode/$(TYPE)/*.o
	$(AR) $@ $?

clean:
	rm -f src/*.o lib/*.a

cvode:
	cd thirdparty/cvode; tar xvzf /shared/simEngine_support/sundials-2.4.0.tar.gz
	cd thirdparty/cvode/sundials-2.4.0; ./configure --disable-static --enable-shared --with-precision=double; make
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_dense.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_direct.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_io.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/nvec_ser/.libs/nvector_serial.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_dense.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_direct.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_math.o thirdparty/cvode/double
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_nvector.o thirdparty/cvode/double
	cd thirdparty/cvode/sundials-2.4.0; ./configure --disable-static --enable-shared --with-precision=single; make
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_dense.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_direct.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode_io.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/cvode/.libs/cvode.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/nvec_ser/.libs/nvector_serial.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_dense.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_direct.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_math.o thirdparty/cvode/float
	cp thirdparty/cvode/sundials-2.4.0/src/sundials/.libs/sundials_nvector.o thirdparty/cvode/float
	
