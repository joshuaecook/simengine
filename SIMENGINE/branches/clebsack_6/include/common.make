# This makefile defines variables and macros common to many subsystems.
#
# Copyright 2009 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

# Platform and operating system detection
OSLOWER = $(shell uname -s|tr [:upper:] [:lower:])
ARCH64 = $(strip $(shell arch|grep 64))

# Compilers and commands
GRIND := valgrind
CXX := g++
CC := gcc
AR := ar rs
LN := ln -s
MKDIR := mkdir -p
RM := rm -rf

ifneq ($(ARCH64),)
VPATH := /lib64 /usr/lib64 /usr/local/lib64 $(VPATH)
endif

# Inspects for presence of Nvidia nvcc; may be overridden on command
NVCC ?= $(shell which nvcc 2>/dev/null)
ifneq ($(NVCC),)
CUDA_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(NVCC))))
NVCC := $(CUDA_INSTALL_PATH)/bin/nvcc
CUDA_RELEASE_VERSION = $(shell $(NVCC) --version | grep release | sed 's/.*release \([0-9]\+\.[0-9]\+\).*/\1/')
CUDA_INCLUDES = -I$(CUDA_INSTALL_PATH)/include
CUDA_LDFLAGS = -L$(CUDA_INSTALL_PATH)/lib 
#VPATH += $(CUDA_INSTALL_PATH)/lib
ifneq ($(ARCH64),)
CUDA_LDFLAGS := -L$(CUDA_INSTALL_PATH)/lib64 $(CUDA_LDFLAGS)
#VPATH := $(CUDA_INSTALL_PATH)/lib64 $(VPATH)
endif
CUDA_LDLIBS = -lcudart
endif

ifeq ($(OSLOWER), darwin)
CUDA_SDK_PATH ?= /Developer/CUDA
else
CUDA_SDK_PATH ?= /opt64/NVIDIA_CUDA_SDK
endif
CUDA_INCLUDES += -I$(CUDA_SDK_PATH)/common/inc 
CUDA_LDFLAGS += -L$(CUDA_SDK_PATH)/lib -L$(CUDA_SDK_PATH)/common/lib/$(OSLOWER)
#VPATH := $(CUDA_SDK_PATH)/lib $(CUDA_SDK_PATH)/common/lib/$(OSLOWER) $(VPATH)
ifneq ($(ARCH64),)
CUDA_LDFLAGS := -L$(CUDA_SDK_PATH)/lib64 -L$(CUDA_SDK_PATH)/common/lib64/$(OSLOWER) $(CUDA_LDFLAGS)
#VPATH := $(CUDA_SDK_PATH)/lib64 $(CUDA_SDK_PATH)/common/lib64/$(OSLOWER) $(VPATH)
endif
CUDA_LDLIBS += -lcutil

# Inspects for presence of MATLAB; may be overridden on command line
MATLAB = $(shell which matlab 2>/dev/null)
ifneq ($(MATLAB),)
MATLAB_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(MATLAB))))
MATLAB := $(MATLAB_INSTALL_PATH)/bin/matlab
MEX := $(MATLAB_INSTALL_PATH)/bin/mex
endif

# Inspects for presence of GNU octave; may be overridden on command line
OCTAVE = $(shell which octave 2>/dev/null)
ifneq ($(OCTAVE),)
OCTAVE_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(OCTAVE))))
OCTAVE := $(OCTAVE_INSTALL_PATH)/bin/octave
MKOCTFILE := $(OCTAVE_INSTALL_PATH)/bin/mkoctfile --mex
endif

# Every possible MEX extension
ALL_MEXEXT = mexglx mexa64 mexmaci mexs64 mexw32 mexw64 mex

# Inspects operating system and architecture
OSLOWER = $(shell uname -s|tr [:upper:] [:lower:])
ARCH64 = $(strip $(shell arch|grep 64))

