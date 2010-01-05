include ../../include/common.make

.PHONY: all clean

all: simex

clean:
	$(RM) simex

simex: override CFLAGS += -I../../codegen/src/simengine
simex: simex.c
	$(LINK.c) -o $@ $< -ldl

