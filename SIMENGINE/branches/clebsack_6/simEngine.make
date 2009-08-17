MODEL ?= split_fn
NUM_MODELS ?= 1
TARGET ?= CPU
SIMENGINE_STORAGE ?= double

CC := gcc
GRIND := valgrind
# Inspects for presence of GNU octave; may be overridden on command line
OCTAVE ?= $(shell which octave 2>/dev/null)
LINK := g++

INCLUDES := -Iinclude
COMMON = -W -Wall -fPIC
CPPFLAGS := -DNUM_MODELS=$(NUM_MODELS) -DTARGET_$(TARGET) -DSIMENGINE_STORAGE_$(SIMENGINE_STORAGE)
CFLAGS = -g $(COMMON) $(INCLUDES)
CXXFLAGS = -g $(COMMON) $(INCLUDES)

LDFLAGS = -Llib 
LDLIBS = -lsolvers -ldl -lm

ifeq ($(TARGET),GPU)
NVCC ?= $(shell which nvcc 2>/dev/null)
NVCCFLAGS = -arch=sm_13 -g --compiler-options "$(COMMON)" $(INCLUDES)
INCLUDES += -I/usr/local/cuda/include -I/opt64/NVIDIA_CUDA_SDK/common/inc
LDFLAGS += -L/usr/local/cuda/lib -L/opt64/NVIDIA_CUDA_SDK/lib -L/opt64/NVIDIA_CUDA_SDK/common/lib/linux
LDLIBS += -lcudart -lcutil
endif

# ---

.PHONY: all clean remake grind real-clean
all: libsimengine.so

clean:
	$(RM) $(MODEL)_parallel.o libsimengine.so grind_simengine

real-clean: clean
	$(RM) $(MODEL)*.c $(MODEL).m $(MODEL)*.cu

remake: clean all

grind: libsimengine.so grind_simengine
	$(GRIND) ./grind_simengine

libsimengine.so: $(MODEL)_parallel.o
	$(LINK.c) -shared -Wl,-soname,libsimengine.so -o libsimengine.so $< $(LDLIBS)

ifeq ($(TARGET),GPU)
$(MODEL)_parallel.cu: $(MODEL)_parallel.c
	cp $< $@

$(MODEL)_parallel.o: $(MODEL)_parallel.cu
	$(NVCC) -c $(CPPFLAGS) $(NVCCFLAGS) -o $@ $<
endif

$(MODEL)_parallel.c: examples/$(MODEL).dsl bin/simEngine
	$(OCTAVE) -q --eval "path('bin',path);simex('$<');"