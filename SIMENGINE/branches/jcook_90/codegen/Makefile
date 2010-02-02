OSLOWER=$(shell uname -s|tr [:upper:] [:lower:])

# define a special architecture if ARCH64 is defined
ifneq ($(ARCH64),)
DARCH = -DARCH64
ifeq ($(OSLOWER),darwin)
TARGET = -DDARWIN
CFLAG64 = -arch x86_64
else
CFLAG64 = -m64
endif
endif

all: codegen

simlib/simlib: simlib/simlib.c
	@cd simlib; gcc -g -o simlib simlib.c -ldl -lz $(CFLAG64) $(TARGET) $(ARCH64) -DSIMLIB_MAIN

lib/libcodegen.sim: simlib/simlib src/solvers/* src/simengine/*
# Create the intermediate objects from all source files
	@cd src; for i in `ls solvers/* simengine/*`; do ../simlib/simlib put $$i $$i;done
# Create the libcodegen library for the target platform
ifeq ($(OSLOWER),linux)
	@cd lib; gcc -shared -Wl,-soname,libcodegen.sim -o libcodegen.sim ../src/*.o
else ifeq ($(OSLOWER),darwin)
	@cd lib; gcc -dynamiclib -Wl,-install_name,libcodegen.sim -o libcodegen.sim ../src/*.o $(CFLAG64)
else
	@echo "Host '$(OSLOWER)' not supported."
endif

codegen: lib/libcodegen.sim

test: lib/libcodegen.sim
	@./simlib/simlib get ./lib/libcodegen.* solvers/solvers.h

clean:
	@rm -f src/*.o lib/libcodegen.sim simlib/simlib
	@rm -rf simlib/simlib.dSYM
