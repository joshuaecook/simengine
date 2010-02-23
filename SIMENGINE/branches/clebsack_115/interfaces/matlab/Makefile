# Copyright 2010 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

include ../../include/common.make

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

INCLUDES += -I../../codegen/src/simengine
LDLIBS = -ldl $(OPENMP_LDLIBS)

SIMENGINE_WRAPPER_OBJ = $(addprefix simEngine_wrapper,$(MEXEXT))
ALL_TARGETS = simEngine_wrapper


.PHONY: all clean $(ALL_TARGETS)

all: $(ALL_TARGETS)

clean:
	$(RM) $(addprefix src/simEngine_wrapper,$(ALL_MEXEXT))

simEngine_wrapper: override CFLAGS += -fPIC -std=gnu99 $(OPENMP_CFLAGS) $(INCLUDES)
simEngine_wrapper: override LDFLAGS += $(SHARED_FLAGS)
simEngine_wrapper: override MEXFLAGS += $(INCLUDES) $(LDLIBS)
simEngine_wrapper: $(addprefix src/,$(SIMENGINE_WRAPPER_OBJ))
