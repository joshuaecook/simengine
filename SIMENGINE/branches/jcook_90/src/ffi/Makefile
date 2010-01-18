SMLCC := mlton
SMLPPFLAGS := -verbose 1 -const 'Exn.keepHistory true' -export-header ffi-simengine.h
BASISEXT := .mlb

COMPILE.sml = $(SMLCC) $(SMLFLAGS) $(SMLPPFLAGS) $(TARGET_ARCH)

.PHONY: all clean tests
all: simex

clean:
	$(RM) simex simex-test ffi-simengine.h

simex: $(addsuffix $(BASISEXT),simex) simex.sml dl.sml ffi-simengine.c
	$(COMPILE.sml) -output $@ $< ffi-simengine.c

simex-test: $(addsuffix $(BASISEXT),simex-test) simex-test.sml simex.sml dl.sml ffi-simengine.c
	$(COMPILE.sml) -output $@ $< ffi-simengine.c

tests: simex-test
	./simex-test
