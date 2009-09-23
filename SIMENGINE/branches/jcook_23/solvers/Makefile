# This makefile handles compiling the solvers libraries into an	
# object library which may be loaded by a simEngine simulation.
#
# The final product of compilation is a shared library object called
# 'libsolvers.a'.
#
# This makefile is reinvoked recursively to create the necessary
# objects for each solver method for all available backends targets.
#
# Copyright 2009 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

self := $(realpath $(lastword $(MAKEFILE_LIST)))
base := $(dir $(self))
include $(base)../include/common.make

DATATYPES = float double
BACKENDS = CPU OPENMP
GPU_BACKENDS = GPU EMUGPU
GPU_METHODS = forwardeuler rk4 bogacki_shampine dormand_prince
METHODS = $(GPU_METHODS) cvode

# All objects are built in a platform-specific directory prefixed with 'target_'
TARGETS = $(foreach datatype,$(DATATYPES),$(foreach backend,$(GPU_BACKENDS) $(BACKENDS),target_$(backend)_$(datatype)))
export TARGETS

ifeq (,$(filter target_%,$(notdir $(CURDIR))))
#$(info Not in build directory)
# No build targets are specified in this section. We simply call out
# to the magical multi-target makefile and rely on it to reinvoke this
# makefile in the appropriate target-specific build directory.

MAKE_TARGETS_PREREQS = thirdparty/cvode
define MAKE_TARGET_CALLBACK
#@$(MAKE) --no-print-directory -C thirdparty/cvode all
@$(MAKE) --no-print-directory -C lib all
endef
define MAKE_TARGET_CLEAN_CALLBACK 
@$(MAKE) --no-print-directory -C lib clean
@$(MAKE) --no-print-directory -C thirdparty/cvode clean
endef
include $(base)../include/target.make

.PHONY: thirdparty/cvode
thirdparty/cvode:
	$(MAKE) --no-print-directory -C $@ all

else
#$(info In build directory $(CURDIR))
# Real build targets are declared in this section. The value of
# $(CURDIR) may be inspected to declare any target-specific variables
# or targets.

# $(SOURCES) is defined by the magical multi-target makefile.
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
else 
# non-GPU targets
ifneq (,$(findstring float,$(CURDIR)))
INCLUDES += -I$(base)thirdparty/cvode/float/sundials-2.4.0/include
else
INCLUDES += -I$(base)thirdparty/cvode/double/sundials-2.4.0/include
endif

ifneq (,$(findstring CPU,$(CURDIR)))
CPPFLAGS += -DTARGET_CPU
else ifneq (,$(findstring OPENMP,$(CURDIR)))
CPPFLAGS += -DTARGET_OPENMP
endif 
endif
# end of target-specific compiler options

ifneq ($(DEBUG),)
	COMMON += -g
	CFLAGS += -ggdb
	NVCCFLAGS += -D_DEBUG
endif

.SUFFIXES:
.PHONY: all clean

ifneq (,$(findstring GPU,$(notdir $(CURDIR))))
ifeq (,$(findstring EMUGPU,$(notdir $(CURDIR))))
all: solver_gpu.o $(addsuffix .cu,$(GPU_METHODS))
else
all: solver_gpu.o
endif

solver_gpu.o: solver_gpu.cu $(base)include/solvers.h
	@$(COMPILE.cpp) -o $@ $<

.PRECIOUS: %.cu
%.cu: %.c
	@$(LN) $< $@

else
all: $(addsuffix .o,$(METHODS))

%.o: %.c $(base)include/solvers.h
	$(COMPILE.c) -o $@ $<
endif

endif
