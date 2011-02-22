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

.PHONY: codegen
codegen: simlib/simlib simlib/simlib.a

.PHONY: clean
clean:
	$(RM) src/*.o 
	$(RM) simlib/simlib simlib/simlib.a
