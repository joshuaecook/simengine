# Compilers
GRIND := valgrind
CXX := g++
CC := gcc
LINK := $(CC) -fPIC
AR := ar rs

ifneq ($(MATLAB_INSTALL_PATH),)
	INCLUDES += -I$(MATLAB_INSTALL_PATH)/extern/include
	MEX := $(MATLAB_INSTALL_PATH)/bin/mex
endif
ifneq ($(OCTAVE_INSTALL_PATH),)
	INCLUDES += -I$(OCTAVE_INSTALL_PATH)/include/octave-3.03/octave
	MKOCTFILE := $(OCTAVE_INSTALL_PATH)/bin/mkoctfile --mex
endif

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

INCLUDES += -Iinclude -I../include
LINKAGE = -ldl

CWARNINGS = -W -Wall

CCOMMON = $(INCLUDES) -fPIC
CFLAGS = $(CWARNINGS) $(CCOMMON) -std=gnu99
CXXFLAGS = $(CXXWARNINGS) $(CCOMMON)
MEXFLAGS = CFLAGS="$(CWARNINGS) -fPIC -std=gnu99" $(INCLUDES) $(LINKAGE)
MKOCTFILEFLAGS = $(CWARNINGS) $(INCLUDES) $(LINKAGE)

ifneq ($(DEBUG),)
	CCOMMON += -g
	CFLAGS += -gdwarf-2
	MEXFLAGS += -g
	MKOCTFILEFLAGS += -g
else
	CCOMMON += -O2
	CXXFLAGS += -fno-strict-aliasing
	CFLAGS += -fno-strict-aliasing
	MEXFLAGS += -O
endif

# Every possible MEX extension; used for cleaning
ALL_MEXEXT = mexglx mexa64 mexmaci mexs64 mexw32 mexw64 mex

define MEXINTERFACE_CLEAN_template
bin/simex_helper.$(1) bin/simEngine_wrapper.$(1)
endef
define MEXINTERFACE_OBJ_template
SIMEX_HELPER_OBJ += simex_helper.$(1)
SIMENGINE_WRAPPER_OBJ += simEngine_wrapper.$(1)
endef
$(foreach ext,$(MEXEXT),$(eval $(call MEXINTERFACE_OBJ_template,$(ext))))

ALL_TARGETS = simex_helper simEngine_wrapper

# Build targets
.PHONY: all $(ALL_TARGETS)
all: $(ALL_TARGETS)

simex_helper: $(foreach obj,$(SIMEX_HELPER_OBJ),bin/$(obj))

simEngine_wrapper: $(foreach obj,$(SIMENGINE_WRAPPER_OBJ),bin/$(obj))

bin/simex_helper.%: ../include/simengine.h ../include/simengine_target.h

bin/%.mexglx: src/%.c
	$(MEX) -glnx86 -outdir bin $(MEXFLAGS) $<

bin/%.mexa64: src/%.c
	$(MEX) -glnxa64 -outdir bin $(MEXFLAGS) $<

# TODO maci64?
bin/%.mexmaci: src/%.c
	$(MEX) -maci -outdir bin $(MEXFLAGS) $<

bin/%.mexs64: src/%.c
	$(MEX) -sol64 -outdir bin $(MEXFLAGS) $<

bin/%.mexw32: src/%.c
	$(MEX) -win32 -outdir bin $(MEXFLAGS) $<

bin/%.mexw64: src/%.c
	$(MEX) -win64 -outdir bin $(MEXFLAGS) $<

bin/%.mex: src/%.c
	$(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(foreach ext,$(ALL_MEXEXT),$(call MEXINTERFACE_CLEAN_template,$(ext)))
