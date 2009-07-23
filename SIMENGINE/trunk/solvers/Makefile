
METHODS = forwardeuler rk4 bogacki_shampine dormand_prince
DATATYPES = float double
GCCLIB = gcc -fPIC -c -Wall -Iinclude
AR = ar rs

all:
	@for type in $(DATATYPES); do \
	  for method in $(METHODS); do \
	   make src/$$method\_$$type.o METHOD=$$method TYPE=$$type; \
	  done; \
	  make lib/solvers\_$$type.a TYPE=$$type; \
	 done

src/%.o: src/$(METHOD).c
	$(GCCLIB) -DCDATAFORMAT=$(TYPE) -o $@ $<

lib/%.a: src/*$(TYPE).o
	$(AR) $@ $?

clean:
	rm -f src/*.o lib/*.a