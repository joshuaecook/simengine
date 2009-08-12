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

define simex_helper_template
bin/simex_helper.$(1)
endef
define SIMEX_HELPER_OBJ_template
SIMEX_HELPER_OBJ += bin/simex_helper.$(1)
endef
$(foreach ext,$(MEXEXT),$(eval $(call SIMEX_HELPER_OBJ_template,$(ext))))

# Build targets
simex_helper: $(SIMEX_HELPER_OBJ)

bin/%.mexa64: src/%.c
	$(MEX) -outdir bin $(MEXFLAGS) $<

bin/%.mex: src/%.c
	$(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(foreach ext,$(ALL_MEXEXT),$(call simex_helper_template,$(ext)))
