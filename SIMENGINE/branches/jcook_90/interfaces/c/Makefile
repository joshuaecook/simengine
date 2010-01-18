include ../../include/common.make

.PHONY: all clean

all: simex

clean:
	$(RM) simex

ifneq ($(DEBUG),"")
SIMEX_DEBUG = -DSIMEX_DEBUG
else
SIMEX_DEBUG =
endif

simex: override CFLAGS += -I../../codegen/src/simengine
simex: simex.c simex.h simex_commandline.c ../../codegen/src/simengine/simengine_api.h ../../codegen/src/simengine/memory_layout.h
	$(LINK.c) -o $@ simex.c simex_commandline.c -ldl $(SIMEX_DEBUG)

