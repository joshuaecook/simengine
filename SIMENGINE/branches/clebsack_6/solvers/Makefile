self := $(realpath $(lastword $(MAKEFILE_LIST)))
base := $(dir $(self))
include $(base)../include/common.make

DATATYPES = float double
BACKENDS = CPU OPENMP
GPU_BACKENDS = GPU EMUGPU
METHODS = forwardeuler rk4 bogacki_shampine dormand_prince

# All objects are built in a platform-specific directory prefixed with 'target_'
TARGETS = $(foreach datatype,$(DATATYPES),$(foreach backend,$(BACKENDS),target_$(backend)_$(datatype)))
TARGETS += $(addprefix target_,$(GPU_BACKENDS))
export TARGETS

ifeq (,$(filter target_%,$(notdir $(CURDIR))))
#$(info Not in build directory)

MAKE_TARGET_CALLBACK = $(MAKE) --no-print-directory -C lib libsolvers.a
MAKE_TARGET_CLEAN_CALLBACK = $(MAKE) --no-print-directory -C lib clean
include $(base)/../include/target.make

else
#$(info In build directory $(CURDIR))
VPATH += $(SOURCES)

INCLUDES = -I$(base)include -I$(base)../include
COMMON = -W -Wall -fPIC
CFLAGS = $(COMMON) $(INCLUDES)

# Initializes target-specific compiler options
ifneq (,$(findstring float,$(CURDIR)))
CPPFLAGS += -DSIMENGINE_STORAGE_float
else
CPPFLAGS += -DSIMENGINE_STORAGE_double
endif

ifneq (,$(findstring GPU,$(CURDIR)))
CPPFLAGS += -DTARGET_GPU
PTXASFLAGS = -v
NVCCFLAGS = -arch=sm_13 --compiler-options "$(COMMON)" --ptxas-options "$(PTXASFLAGS)" $(INCLUDES)
INCLUDES += $(CUDA_INCLUDES)
LDFLAGS += $(CUDA_LDFLAGS)
LDLIBS += $(CUDA_LDLIBS)

ifneq (,$(findstring EMUGPU,$(CURDIR)))
NVCCFLAGS += -deviceemu
endif

override CXX := $(NVCC)
override CXXFLAGS := $(NVCCFLAGS)
else ifneq (,$(findstring CPU,$(CURDIR)))
CPPFLAGS += -DTARGET_CPU
else ifneq (,$(findstring OPENMP,$(CURDIR)))
CPPFLAGS += -DTARGET_OPENMP
endif 
# end of target-specific compiler options

.SUFFIXES:
.PHONY: all clean

ifneq (,$(findstring GPU,$(notdir $(CURDIR))))
ifeq (,$(findstring EMUGPU,$(notdir $(CURDIR))))
all: solver_gpu.o $(addsuffix .cu,$(METHODS))
else
all: solver_gpu.o
endif

solver_gpu.o: solver_gpu.cu $(base)include/solvers.h
	@$(COMPILE.cpp) -o $@ $<
else
all: $(addsuffix .o,$(METHODS))

%.o: %.c $(base)include/solvers.h
	@$(COMPILE.c) -o $@ $<
endif

.PRECIOUS: %.cu
%.cu: %.c
	@$(LN) $< $@

endif
