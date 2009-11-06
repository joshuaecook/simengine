OSLOWER=$(shell uname -s|tr [:upper:] [:lower:])

all: codegen

simlib/simlib: simlib/simlib.c
ifeq ($(OSLOWER),linux)
	@cd simlib; gcc -g -o simlib simlib.c -ldl -lz
else ifeq ($(OSLOWER),darwin)
	@cd simlib; gcc -g -o simlib -DDARWIN simlib.c -ldl -lz
else
	@echo "Host '$(OSLOWER)' not supported."
endif

lib/libcodegen.sim: simlib/simlib src/solvers/* src/simengine/*
# Create the intermediate objects from all source files
	@cd src; for i in `ls solvers/* simengine/*`; do ../simlib/simlib SiMagic put $$i;done
# Create the libcodegen library for the target platform
ifeq ($(OSLOWER),linux)
	@cd lib; gcc -shared -Wl,-soname,libcodegen.sim -o libcodegen.sim ../src/*.o
else ifeq ($(OSLOWER),darwin)
	@cd lib; gcc -dynamiclib -Wl,-install_name,libcodegen.sim -o libcodegen.sim ../src/*.o
else
	@echo "Host '$(OSLOWER)' not supported."
endif

codegen: lib/libcodegen.sim

test: lib/libcodegen.sim
	@./simlib/simlib SiMagic get ./lib/libcodegen.* solvers/solvers.h

clean:
	@rm -f src/*.o lib/libcodegen.sim simlib/simlib
	@rm -rf simlib/simlib.dSYM
