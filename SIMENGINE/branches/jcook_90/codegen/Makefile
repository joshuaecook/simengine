include ../include/common.make

.PHONY: all
all: simlib/simlib simlib/simlib.a codegen


simlib/simlib.o: CPPFLAGS += $(if $(ARCH64),-DARCH64,)

simlib/simlib-command.o: CPPFLAGS += -DSIMLIB_MAIN $(if $(ARCH64),-DARCH64,)
simlib/simlib-command.o: simlib/simlib.c
	$(COMPILE.c) -o $@ $<

simlib/simlib: simlib/simlib-command.o
	$(LINK.c) -o $@ $< -ldl -lz

simlib/simlib.a: simlib/simlib.a(simlib/simlib.o)

lib/libcodegen.sim: simlib/simlib src/solvers/* src/simengine/*
# Create the intermediate objects from all source files
	cd src; for i in `ls solvers/* simengine/*`; do ../simlib/simlib put $$i $$i;done
# Create the libcodegen library for the target platform
ifeq ($(OSLOWER),linux)
	cd lib; $(LINK.c) -shared -Wl,-soname,libcodegen.sim -o libcodegen.sim ../src/*.o
else ifeq ($(OSLOWER),darwin)
	cd lib; $(LINK.c) -dynamiclib -Wl,-install_name,libcodegen.sim -o libcodegen.sim ../src/*.o
else
	$(error Host $(OSLOWER) not supported.)
endif

.PHONY: codegen
codegen: lib/libcodegen.sim

.PHONY: test
test: lib/libcodegen.sim
	@./simlib/simlib get ./lib/libcodegen.* solvers/solvers.h

.PHONY: clean
clean:
	$(RM) src/*.o 
	$(RM) lib/libcodegen.sim 
	$(RM) simlib/simlib simlib/simblib.o simlib/simlib-command.o simlib/simlib.a
	$(RM) simlib/simlib.dSYM
