include ../include/common.make

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

DEBUG=1

INCLUDES += -Iinclude -I../include
LDLIBS = -ldl -lgomp

CWARNINGS = -W -Wall

COMMON = $(INCLUDES) -fPIC
CFLAGS = $(CWARNINGS) $(COMMON) -std=gnu99
MEXFLAGS = CFLAGS="$(CWARNINGS) -fPIC -std=gnu99 -fopenmp" $(INCLUDES) $(LDLIBS)
MKOCTFILEFLAGS = $(CWARNINGS) $(INCLUDES) $(LDLIBS)

ifneq ($(DEBUG),)
	COMMON += -g
	CFLAGS += -ggdb
	MEXFLAGS += -g
	MKOCTFILEFLAGS += -g
else
	COMMON += -O2
	CFLAGS += -fno-strict-aliasing
	MEXFLAGS += -O
endif
ifeq ($(OSLOWER),darwin)
	LD = "/opt/local/bin/gcc-mp-4.4"
	CC = "/opt/local/bin/gcc-mp-4.4"
	CXX = "/opt/local/bin/g++-mp-4.4"
	LOCATIONS=LD=$(LD) CC=$(CC) CXX=$(CXX)
else
	LOCATIONS=
endif
MEXFLAGS += $(LOCATIONS)

SIMEX_HELPER_OBJ = $(addprefix simex_helper,$(MEXEXT))
SIMENGINE_WRAPPER_OBJ = $(addprefix simEngine_wrapper,$(MEXEXT))
ALL_TARGETS = simex_helper simEngine_wrapper

# Build targets
.PHONY: all $(ALL_TARGETS)
all: $(ALL_TARGETS)

simex_helper: $(SIMEX_HELPER_OBJ) # $(addprefix bin/,$(SIMEX_HELPER_OBJ))

simEngine_wrapper: $(SIMENGINE_WRAPPER_OBJ) # $(addprefix bin/,$(SIMENGINE_WRAPPER_OBJ))

#bin/simex_helper.%: ../include/simengine.h ../include/simengine_target.h

%.mexglx: src/%.cpp
	$(MEX) -glnx86 -outdir . $(MEXFLAGS) $<

%.mexglx: src/%.c
	$(MEX) -glnx86 -outdir . $(MEXFLAGS) $<

%.mexa64: src/%.cpp
	$(MEX) -glnxa64 -outdir . $(MEXFLAGS) $<

%.mexa64: src/%.c
	$(MEX) -glnxa64 -outdir . $(MEXFLAGS) $<

# TODO maci64?
%.mexmaci: src/%.cpp
	$(MEX) -maci -outdir . $(MEXFLAGS) $<

%.mexmaci: src/%.c
	$(MEX) -maci -outdir . $(MEXFLAGS) $<

%.mexs64: src/%.cpp
	$(MEX) -sol64 -outdir . $(MEXFLAGS) $<

%.mexs64: src/%.c
	$(MEX) -sol64 -outdir . $(MEXFLAGS) $<

%.mexw32: src/%.cpp
	$(MEX) -win32 -outdir . $(MEXFLAGS) $<

%.mexw32: src/%.c
	$(MEX) -win32 -outdir . $(MEXFLAGS) $<

%.mexw64: src/%.cpp
	$(MEX) -win64 -outdir . $(MEXFLAGS) $<

%.mexw64: src/%.c
	$(MEX) -win64 -outdir . $(MEXFLAGS) $<

%.mex: src/%.cpp
	$(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

%.mex: src/%.c
	 $(MKOCTFILE) -o $@ $(MKOCTFILEFLAGS) $<

.PHONY: clean
clean:
	$(RM) $(addprefix simex_helper,$(ALL_MEXEXT)) $(addprefix simEngine_wrapper,$(ALL_MEXEXT))
