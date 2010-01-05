# Copyright 2010 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

include ../../include/common.make

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

INCLUDES += -I../../codegen/src/simengine
LDLIBS = -ldl $(OPENMP_LDLIBS)

SIMEX_HELPER_OBJ = $(addprefix simex_helper,$(MEXEXT))
SIMENGINE_WRAPPER_OBJ = $(addprefix simEngine_wrapper,$(MEXEXT))
ALL_TARGETS = simex_helper simEngine_wrapper


.PHONY: all clean $(ALL_TARGETS)

all: $(ALL_TARGETS)

clean:
	$(RM) $(addprefix src/simex_helper,$(ALL_MEXEXT)) $(addprefix src/simEngine_wrapper,$(ALL_MEXEXT))

simex_helper: override CFLAGS += -fPIC -fopenmp -std=gnu99
simex_helper: override LDFLAGS += $(SHARED_FLAGS)
simex_helper: override MEXFLAGS += $(INCLUDES) $(LDLIBS)
simex_helper: src/$(SIMEX_HELPER_OBJ)

simEngine_wrapper: override CFLAGS += -fPIC -std=gnu99 $(OPENMP_CFLAGS) $(INCLUDES)
simEngine_wrapper: override LDFLAGS += $(SHARED_FLAGS)
simEngine_wrapper: override MEXFLAGS += $(INCLUDES) $(LDLIBS)
simEngine_wrapper: src/$(SIMENGINE_WRAPPER_OBJ)
