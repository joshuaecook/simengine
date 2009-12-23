# This makefile defines variables and macros common to many subsystems.
#
# Copyright 2009 Simatra Modeling Technologies, L.L.C.
# For more information, please visit http://www.simatratechnologies.com

# Platform and operating system detection
OSLOWER = $(shell uname -s|tr [:upper:] [:lower:])
ARCH = $(strip $(shell arch))
ARCH64 = $(strip $(shell arch|grep 64))

# Compilers and commands
CC := gcc
CCVERSION = $(shell $(CC) -v 2>&1 | tail -1 | cut -d' ' -f 3)
CCMAJOR = $(shell echo $(CCVERSION) | cut -d. -f 1)
CCMINOR = $(shell echo $(CCVERSION) | cut -d. -f 2)

ifeq ($(OSLOWER), darwin)
# OpenMP requires gcc-4.2 on OS X Leopard
CC := $(shell if [ 4 -ge $(CCMAJOR) -a 2 -gt $(CCMINOR) ]; then echo $(CC)-4.2; else echo $(CC); fi)
CCVERSION = $(shell $(CC) -v 2>&1 | tail -1 | cut -d' ' -f 3)
CCMAJOR = $(shell echo $(CCVERSION) | cut -d. -f 1)
CCMINOR = $(shell echo $(CCVERSION) | cut -d. -f 2)
endif

CFLAGS += -arch $(ARCH)

CXX := g++
CXXVERSION = $(shell $(CXX) -v 2>&1 | tail -1 | cut -d' ' -f 3)
CXXMAJOR = $(shell echo $(CXXVERSION) | cut -d. -f 1)
CXXMINOR = $(shell echo $(CXXVERSION) | cut -d. -f 2)

ifeq ($(OSLOWER), darwin)
# OpenMP requires g++-4.2 on OS X Leopard
CXX := $(shell if [ 4 -ge $(CXXMAJOR) -a 2 -gt $(CXXMINOR) ]; then echo $(CXX)-4.2; else echo $(CXX); fi)
CXXVERSION = $(shell $(CXX) -v 2>&1 | tail -1 | cut -d' ' -f 3)
CXXMAJOR = $(shell echo $(CXXVERSION) | cut -d. -f 1)
CXXMINOR = $(shell echo $(CXXVERSION) | cut -d. -f 2)
endif

AR := ar rs
LN := ln -s
MKDIR := mkdir -p
RM := rm -rf
GRIND := valgrind

ifneq ($(ARCH64),)
VPATH := /lib64 /usr/lib64 /usr/local/lib64 $(VPATH)
endif

# Inspects for presence of Nvidia nvcc; may be overridden on command line
NVCC ?= $(shell which nvcc 2>/dev/null)
ifneq ($(NVCC),)
CUDA_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(NVCC))))
NVCC := $(CUDA_INSTALL_PATH)/bin/nvcc
CUDA_RELEASE_VERSION = $(shell $(NVCC) --version | grep release | sed 's/.*release \([0-9]\+\.[0-9]\+\).*/\1/')
CUDA_INCLUDES = -I$(CUDA_INSTALL_PATH)/include
CUDA_LDFLAGS = -L$(CUDA_INSTALL_PATH)/lib 
ifneq ($(ARCH64),)
CUDA_LDFLAGS := -L$(CUDA_INSTALL_PATH)/lib64 $(CUDA_LDFLAGS)
endif
CUDA_LDLIBS = -lcudart
endif

OPENMP_LDLIBS = -lgomp

# Inspects for presence of MATLAB; may be overridden on command line
MATLAB = $(shell which matlab 2>/dev/null)
ifneq ($(MATLAB),)
MATLAB_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(MATLAB))))
MATLAB := MATLABROOT=$(MATLAB_INSTALL_PATH) $(MATLAB_INSTALL_PATH)/bin/matlab
MEX := MATLABROOT=$(MATLAB_INSTALL_PATH) $(MATLAB_INSTALL_PATH)/bin/mex
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
