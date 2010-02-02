#Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
include ../../include/common.make

SMLPPFLAGS = -verbose 1 -const 'Exn.keepHistory true' -export-header ffi-exports.h
SMLFLAGS = -link-opt -ldl
BASISEXT = .mlb

.PHONY: all clean
all: simex simlib

clean:
	$(RM) ffi-simengine.h
	$(RM) simex simex-test 
	$(RM) simlib simlib-test

SIMEX_BASIS := $(addsuffix $(BASISEXT),simex)
SIMEX_SOURCES := $(shell $(SMLC) -stop f $(SIMEX_BASIS))

simex: $(SIMEX_SOURCES) ffi-simengine.c
	$(COMPILE.sml) -output $@ $< ffi-simengine.c

SIMEX_TEST_BASIS := $(addsuffix $(BASISEXT),simex-test)
SIMEX_TEST_SOURCES := $(shell $(SMLC) -stop f $(SIMEX_TEST_BASIS))

simex-test: $(SIMEX_TEST_SOURCES) ffi-simengine.c
	$(COMPILE.sml) -output $@ $< ffi-simengine.c

SIMLIB_BASIS := $(addsuffix $(BASISEXT),simlib)
SIMLIB_SOURCES := $(shell $(SMLC) -stop f $(SIMLIB_BASIS))

simlib: SMLFLAGS += -link-opt "-lz ../../codegen/simlib/simlib.a"
simlib: $(SIMLIB_SOURCES) ../../codegen/simlib/simlib.a ffi-simlib.c 
	$(COMPILE.sml) -output $@ $< ffi-simlib.c

SIMLIB_TEST_BASIS := $(addsuffix $(BASISEXT),simlib-test)
SIMLIB_TEST_SOURCES := $(shell $(SMLC) -stop f $(SIMLIB_TEST_BASIS))

simlib-test: SMLFLAGS += -link-opt "-lz ../../codegen/simlib/simlib.a"
simlib-test: $(SIMLIB_TEST_SOURCES) ../../codegen/simlib/simlib.a ffi-simlib.c
	$(COMPILE.sml) -output $@ $< ffi-simlib.c
