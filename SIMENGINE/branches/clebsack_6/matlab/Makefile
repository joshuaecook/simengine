include ../include/common.make

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

DEBUG=1

INCLUDES += -Iinclude -I../include
LDLIBS = -ldl

CWARNINGS = -W -Wall

COMMON = $(INCLUDES) -fPIC
CFLAGS = $(CWARNINGS) $(COMMON) -std=gnu99
MEXFLAGS = CFLAGS="$(CWARNINGS) -fPIC -std=gnu99" $(INCLUDES) $(LDLIBS)
MKOCTFILEFLAGS = $(CWARNINGS) $(INCLUDES) $(LDLIBS)

ifneq ($(DEBUG),)
	COMMON += -g
	CFLAGS += -gdwarf-2
	MEXFLAGS += -g
	MKOCTFILEFLAGS += -g
else
	COMMON += -O2
	CFLAGS += -fno-strict-aliasing
	MEXFLAGS += -O
endif

# Every possible MEX extension; used for cleaning
ALL_MEXEXT = .mexglx .mexa64 .mexmaci .mexs64 .mexw32 .mexw64 .mex

SIMEX_HELPER_OBJ = $(addprefix simex_helper,$(MEXEXT))
SIMENGINE_WRAPPER_OBJ = $(addprefix simEngine_wrapper,$(MEXEXT))

ALL_TARGETS = simex_helper simEngine_wrapper

# Build targets
.PHONY: all $(ALL_TARGETS)
all: $(ALL_TARGETS)

simex_helper: $(addprefix bin/,$(SIMEX_HELPER_OBJ))

simEngine_wrapper: $(addprefix bin/,$(SIMENGINE_WRAPPER_OBJ))

#bin/simex_helper.%: ../include/simengine.h ../include/simengine_target.h

bin/%.mexglx: src/%.cpp
	$(MEX) -glnx86 -outdir bin $(MEXFLAGS) $<

bin/%.mexglx: src/%.c
	$(MEX) -glnx86 -outdir bin $(MEXFLAGS) $<

bin/%.mexa64: src/%.cpp
	$(MEX) -glnxa64 -outdir bin $(MEXFLAGS) $<

bin/%.mexa64: src/%.c
	$(MEX) -glnxa64 -outdir bin $(MEXFLAGS) $<

# TODO maci64?
bin/%.mexmaci: src/%.cpp
	$(MEX) -maci -outdir bin $(MEXFLAGS) $<

bin/%.mexmaci: src/%.c
	$(MEX) -maci -outdir bin $(MEXFLAGS) $<

bin/%.mexs64: src/%.cpp
	$(MEX) -sol64 -outdir bin $(MEXFLAGS) $<

bin/%.mexs64: src/%.c
	$(MEX) -sol64 -outdir bin $(MEXFLAGS) $<

bin/%.mexw32: src/%.cpp
	$(MEX) -win32 -outdir bin $(MEXFLAGS) $<

bin/%.mexw32: src/%.c
	$(MEX) -win32 -outdir bin $(MEXFLAGS) $<

bin/%.mexw64: src/%.cpp
	$(MEX) -win64 -outdir bin $(MEXFLAGS) $<

bin/%.mexw64: src/%.c
	$(MEX) -win64 -outdir bin $(MEXFLAGS) $<

bin/%.mex: src/%.cpp
	$(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

bin/%.mex: src/%.c
	 $(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(addprefix bin/simex_helper,$(ALL_MEXEXT)) $(addprefix bin/simEngine_wrapper,$(ALL_MEXEXT))
