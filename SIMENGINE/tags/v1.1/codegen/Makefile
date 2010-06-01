include ../include/common.make

ifneq ($(DARWIN),)
TARGET_ARCH = -arch i386
endif

.PHONY: all
all: simlib/simlib simlib/simlib.a codegen

simlib/simlib: CPPFLAGS += -DSIMLIB_MAIN
simlib/simlib: simlib/simlib.c
	$(LINK.c) -o $@ $< -ldl -lz

simlib/simlib.a: simlib/simlib.a(simlib/simlib.o)

src/solvers_solvers_h.o: simlib/simlib src/solvers/* src/simengine/*
# Create the intermediate objects from all source files
	cd src; for i in `ls solvers/* simengine/*`; do ../simlib/simlib put $$i $$i;done

.PHONY: codegen
codegen: simlib/simlib simlib/simlib.a src/solvers_solvers_h.o

.PHONY: clean
clean:
	$(RM) src/*.o 
	$(RM) simlib/simlib simlib/simlib.a
