include ../../include/common.make

ifeq ("$(MEX)$(MKOCTFILE)","")
$(error "Couldn't find MEX or MKOCTFILE.")
endif

DEBUG=1

INCLUDES += -Iinclude -I../../codegen/src/simengine
LDLIBS = -ldl -lgomp

CWARNINGS = -W -Wall

COMMON = $(INCLUDES) -fPIC
CFLAGS = $(CWARNINGS) $(COMMON) -std=gnu99
ifneq ($(ARCH64),)
ifeq ($(OSLOWER),darwin)
ARCH64_FLAG = -arch x86_64
else
ARCH64_FLAG = -m64
endif
endif
MEXFLAGS = CFLAGS="$(CWARNINGS) -fPIC -std=gnu99 -fopenmp $(ARCH64_FLAG)" $(INCLUDES) $(LDLIBS)
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
ifeq ($(OSLOWER),darwin)
	LD = "/usr/bin/gcc-4.2"  
	CC = "/usr/bin/gcc-4.2"  
	CXX = "/usr/bin/g++-4.2"  
	LOCATIONS=LD=$(LD) CC=$(CC) CXX=$(CXX)  
else
	LOCATIONS=
endif
MEXFLAGS += $(LOCATIONS)

# Every possible MEX extension; used for cleaning
ALL_MEXEXT = .mexglx .mexa64 .mexmaci .mexmaci64 .mexs64 .mexw32 .mexw64 .mex

SIMEX_HELPER_OBJ = $(addprefix simex_helper,$(MEXEXT))
SIMENGINE_WRAPPER_OBJ = $(addprefix simEngine_wrapper,$(MEXEXT))
ALL_TARGETS = simex_helper simEngine_wrapper

# Build targets
.PHONY: all $(ALL_TARGETS)
all: $(ALL_TARGETS)

simex_helper: $(SIMEX_HELPER_OBJ) # $(addprefix bin/,$(SIMEX_HELPER_OBJ))

simEngine_wrapper: $(SIMENGINE_WRAPPER_OBJ) # $(addprefix bin/,$(SIMENGINE_WRAPPER_OBJ))

%.mexglx: src/%.cpp
	$(MEX) -glnx86 -outdir . $(MEXFLAGS) $<

%.mexglx: src/%.c
	$(MEX) -glnx86 -outdir . $(MEXFLAGS) $<

%.mexa64: src/%.cpp
	$(MEX) -glnxa64 -outdir . $(MEXFLAGS) $<

%.mexa64: src/%.c
	$(MEX) -glnxa64 -outdir . $(MEXFLAGS) $<

%.mexmaci: src/%.cpp
	$(MEX) -maci -outdir . $(MEXFLAGS) $<

%.mexmaci: src/%.c
	$(MEX) -maci -outdir . $(MEXFLAGS) $<

%.mexmaci64: src/%.cpp
	$(MEX) -maci64 -outdir . $(MEXFLAGS) $<

%.mexmaci64: src/%.c
	$(MEX) -maci64 -outdir . $(MEXFLAGS) $<

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
