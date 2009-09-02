# Dynamo Compiler Makefile
# Copyright 2007-2008, Simatra Modeling Technologies, L.L.C.


# User Adjustable Parameters
# ==============================================================================================

# BUILD_TYPE can be software (floating point only), enhanced (floating point and fixed point), or full
BUILD_TYPE = software

# DEBUG can be defined or not defined.  By default, it is defined always in a branch, but not defined in the trunk or a tag
#DEBUG = true

# Version # for RPM
VERSION_NUMBER = 0.8
BUILD_DATE = $(shell date)

DESTDIR = 
PARTIAL_INSTALL_BUILD_DIR = /usr/local/share/Simatra
INSTALL_BUILD_DIR = $(DESTDIR)$(PARTIAL_INSTALL_BUILD_DIR)
INSTALL_BIN_DIR = $(DESTDIR)/usr/local/bin

# Operating system and architecture inspection
UNAME = $(shell uname -s)
OSLOWER = $(shell uname -s 2>/dev/null | tr [:upper:] [:lower:])
DARWIN = $(strip $(findstring darwin, $(OSLOWER)))
LINUX = $(strip $(findstring linux, $(OSLOWER)))
ARCH64 = $(strip $(shell arch 2>/dev/null | grep 64))


FIND_LATEX = $(shell which latex 2> /dev/null)

ifeq ($(FIND_LATEX),/opt/bin/latex)
BUILD_DOCS = true
endif

# Version of files
ISE_VERSION = 9.1i
#ISE_VERSION = 10.1
EDK_TOOL_SCRIPT = /opt/scripts/xilinx-edk-$(ISE_VERSION)-settings.sh
SETUP_EDK = source $(EDK_TOOL_SCRIPT)
ISE_TOOL_SCRIPT = /opt/scripts/xilinx-ise-$(ISE_VERSION)-settings.sh
SETUP_ISE = source $(ISE_TOOL_SCRIPT)
JTAG_SCRIPT = /opt/scripts/jtag-program.sh
SETUP_JTAG = source $(JTAG_SCRIPT)
SETUP_ENV = $(SETUP_ISE); $(SETUP_EDK); $(SETUP_JTAG)
OVERALL_SCRIPT = /opt/scripts/xilinx.sh
SETUP_ENV = source $(OVERALL_SCRIPT)
JTAG_LIB = /opt/lib/libusb-driver.so

# Files in example directory to be copied into build directory
#DYN = brk_improved.dyn brk_simple.dyn brk_simple_pop.dyn fn_inp.dyn hh_improved.dyn
#DPL = brk.dpl brk_simple.dpl brk_simple_pop.dpl fn_inp.dpl hh.dpl
#EXAMPLES = release/tutorial release/pBc release/HN release/HH release/FN release/BRK
# In the oodlc, examples are in the tests directory under models
# EXAMPLES = models/*.dsl
EXAMPLES = ../examples/misc/*.dsl ../examples/neural/*/*.dsl
# There are certain files required that must be installed on the host system. These packages are defined as PKGS
# All of these PKGS have to be in the scripts directory
ifeq ($(UNAME), Linux)
PKGS = 
else
PKGS = ruby/rgplot-2.2.tar.gz ruby/rubygems-1.0.1.tgz ruby/ruby-prof-0.6.0.gem
endif

# Help Screen
# ==============================================================================================


help:
	@echo " "
	@echo "**********************************************************"
	@echo "****        Simatra simEngine Compiler Makefile       ****"
	@echo "**********************************************************"
	@echo "****                                                  ****"
	@echo "****  There are two user definable parameters:        ****"
	@echo "****                                                  ****"
	@echo "****    BUILD_TYPE - one of three options             ****"
	@echo "****      BUILD_TYPE=software                         ****"
	@echo "****      BUILD_TYPE=enhanced                         ****"
	@echo "****      BUILD_TYPE=full                             ****"
	@echo "****                                                  ****"
	@echo "****    DEBUG - set to true to enable                 ****"
	@echo "****      When DEBUG is set to true, all testing code ****"
	@echo "****      is generated                                ****"
	@echo "****                                                  ****"
	@echo "**********************************************************"
	@echo "****                                                  ****"
	@echo "****  make help - display this message                ****"
	@echo "****                                                  ****"
	@echo "****  make software - Builds the software-only        ****"
	@echo "****                  simEngine compiler              ****"
	@echo "****                                                  ****"
#	@echo "****  make enhanced - Builds the software+fixpt       ****"
#	@echo "****                  Dynamo compiler                 ****"
#	@echo "****                                                  ****"
#	@echo "****  make full - Builds the full Dynamo Compiler     ****"
#	@echo "****                                                  ****"
	@echo "****  make compiler - Just build the compiler         ****"
	@echo "****                  in accordance with BUILD_TYPE   ****"
	@echo "****                                                  ****"
#	@echo "****  make coregen - Builds the FIFO used in the      ****"
#	@echo "****                 Verilog model description        ****"
#	@echo "****                                                  ****"
	@echo "****  make installer - Generates an install tar file  ****"
	@echo "****                                                  ****"
	@echo "****  make build-release - Adds the install file to   ****"
	@echo "****                       the build directory        ****"
	@echo "****                                                  ****"
	@echo "****  make build-all - Adds the install file to the   ****"
	@echo "****                   build directory for all types  ****"
	@echo "****                                                  ****"
	@echo "****  make profiling-tools - Builds the profiling     ****"
	@echo "****                         tools                    ****"
	@echo "****                                                  ****"
	@echo "****  make system-test - perform a system test of     ****"
	@echo "****                     all suites or just one suite ****"
	@echo "****                                                  ****"
	@echo "****  make install - installs the files in the        ****"
	@echo "****                 default location                 ****"
	@echo "****                                                  ****"
	@echo "****  make rpm - builds RPM files                     ****"
	@echo "****                                                  ****"
ifeq ($(BRANCH),1)
	@echo "****  make update-from-trunk - merges trunk changes   ****"
	@echo "****                           into branch            ****"
	@echo "****                                                  ****"
	@echo "****  make show-changes - run browser and show svn    ****"
	@echo "****                      changes from branch         ****"
	@echo "****                                                  ****"
endif
	@echo "****  make clean - removes the compiled code          ****"
	@echo "****                                                  ****"
	@echo "****  make clean-all - removes the compiled code and  ****"
	@echo "****                   all ml403 generated files      ****"
	@echo "****                                                  ****"
	@echo "**********************************************************"
	@echo " "



# Adjustable Parameters
# ==============================================================================================

# Compilers
GRIND := valgrind
CXX := g++
CC := gcc
LINK := $(CC) -fPIC
AR := ar rs



SVNSRV = https://svn1.hosted-projects.com/simatra/simEngine
SVNWEBSRV = http://www.hosted-projects.com/trac/simatra/simEngine
SVNTRUNK = $(SVNSRV)/trunk
SRC = src/
BUILD = build
RM = rm -rf
ARGS = -default-ann 'allowFFI true' -target-link-opt linux -ldl -const 'Exn.keepHistory true' -verbose 1 #-target-cc-opt x86 -march=pentium4 -target-cc-opt x86 -m32
SIMPLE_ARGS = -default-ann 'allowFFI true' -target-link-opt linux -ldl -verbose 1
CC = gcc
CP = cp -rf
ifeq ($(UNAME), MINGW32_NT-5.1)
MLTON = mlton
else
MLTON = time nice mlton
endif
ifeq ($(UNAME), Darwin)
ENV = /usr/bin/env
else
ENV = /bin/env
endif
RUBY = $(ENV) ruby
RMDIR = rm -rf
MKDIR = mkdir -p
SYSTESTDIR = ../testing/frameworks/system
SYSTEST = systest
ITEST = itest
DSLC = dslc
ITEST_NIGHTLY_OUTPUT_DIR = /shared/QA_Results/nightly
MAKE = make --no-print-directory
BROWSER = firefox
COREGEN = $(SETUP_ENV); coregen
MATLABCMD = matlab -nodisplay -nojvm -r 

OCTAVECMD = octave --eval 
FIND_MATLAB = $(shell which matlab 2> /dev/null)
ifeq ($(UNAME), Darwin)
ifeq ($(FIND_MATLAB),/usr/bin/matlab)
MATLAB_PATH = /Applications/MATLAB_R2009a.app
MEX = $(MATLAB_PATH)/bin/mex
MEXEXT = .mexmaci
endif
else
ifeq ($(FIND_MATLAB),/usr/local/bin/matlab)
MATLAB_PATH = /usr/local/matlab
MEX = $(MATLAB_PATH)/bin/mex
MEXEXT = .mexa64
else
MATLAB_PATH = /opt/octave
MEX = $(MATLAB_PATH)/bin/mkoctfile --mex 
MEXEXT = .mex
endif
endif

# Inspects for presence of MATLAB; may be overridden on command line
MATLAB ?= $(shell which matlab 2>/dev/null)
ifneq ($(MATLAB),)
MATLAB_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(MATLAB))))
MATLAB := $(MATLAB_INSTALL_PATH)/bin/matlab
MEX := $(MATLAB_INSTALL_PATH)/bin/mex
@echo "Found a MATLAB installation here: $(MATLAB)"
endif

# Inspects for presence of GNU octave; may be overridden on command line
OCTAVE ?= $(shell which octave 2>/dev/null)
ifneq ($(OCTAVE),)
OCTAVE_INSTALL_PATH = $(shell dirname $$(dirname $(realpath $(OCTAVE))))
OCTAVE := $(OCTAVE_INSTALL_PATH)/bin/octave
MKOCTFILE := $(OCTAVE_INSTALL_PATH)/bin/mkoctfile --mex
@echo "Found a GNU octave installation here: $(OCTAVE)"
endif

ifeq ("$(MATLAB)$(OCTAVE)","")
$(error "Couldn't find MATLAB or GNU octave.")
endif

# Every possible MEX extension
ALL_MEXEXT = .mexglx .mexa64 .mexmaci .mexs64 .mexw32 .mexw64 .mex
MATLAB_TARGETS = $(addprefix matlab/%,$(ALL_MEXEXT))

# Determines the appropriate MEX extension for the current platform.
# TODO: extensions for other platforms
ifneq ($(MATLAB),)
MEXEXT = .$(shell $(MATLAB_INSTALL_PATH)/bin/mexext)
endif

ifneq ($(OCTAVE),)
MEXEXT += .mex
endif


CXXWARNINGS = -W -Wall -Wimplicit -Wswitch -Wformat -Wchar-subscripts \
	-Wparentheses -Wmultichar -Wtrigraphs -Wpointer-arith -Wcast-align \
	-Wreturn-type -Wno-unused-function
CWARNINGS = -Wstrict-prototypes -Wmissing-prototypes \
	-Wmissing-declarations -Wnested-externs -Wmain $(CXXWARNINGS)


# File Names and Locations
# ==============================================================================================

FILENAME = simEngine
OUTPUT_FILE = $(BUILD_DIR)/bin/$(FILENAME)

# Platform Specific Parameters
# ==============================================================================================

# This is to handle shared libraries on a Mac vs. Linux
ifeq ($(UNAME), Darwin)
SHARED_FLAG=-dynamiclib -Wl,-single_module
else
SHARED_FLAG=-shared
endif

ifeq ($(UNAME), CYGWIN_NT-5.1)
LINKING_FLAG=
else
ifeq ($(UNAME), MINGW32_NT-5.1)
LINKING_FLAG=
else
LINKING_FLAG=-lm -fPIC
endif
endif


ifeq ($(UNAME), CYGWIN_NT-5.1)
BUILD_DIR=$(BUILD)_cyg_$(BUILD_TYPE)
PLATFORM=cyg
else
ifeq ($(UNAME), MINGW32_NT-5.1)
BUILD_DIR=$(BUILD)_mingw_$(BUILD_TYPE)
PLATFORM=mingw
else
ifeq ($(UNAME), Darwin)
BUILD_DIR=$(BUILD)_mac_$(BUILD_TYPE)
PLATFORM=mac
else
ifeq ($(UNAME), Linux)
BUILD_DIR=$(BUILD)_lin_$(BUILD_TYPE)
PLATFORM=lin
else
BUILD_DIR=$(BUILD)_unknown
PLATFORM=unknown
endif
endif
endif
endif

# Creates lists of MATLAB/Octave targets for each available MEX type
SIMENGINE_WRAPPER = $(addprefix simEngine_wrapper,$(MEXEXT))
SIMEX_HELPER = $(addprefix simex_helper,$(MEXEXT))
SIMEX_HELPER_BUILD = $(addprefix $(BUILD_DIR)/,$(SIMEX_HELPER))


# Branching Related Parameters
# ==============================================================================================

BRANCH=$(shell pwd | grep -c branches)
TRUNK=$(shell pwd | grep -c trunk)
TAG=$(shell pwd | grep -c tag)

ifeq ($(BRANCH),1)
RELEASE = "branch"
BUILDREV = $(shell svn info | grep '^Revision:' | sed -e 's/Revision: //')
REV = $(shell pwd | sed -e 's/.*\/branches\/\(.*\)\/oodlc/\1/')
INSTALL_FILENAME = "$(FILENAME)_branch_$(REV):$(BUILDREV)_$(BUILD_TYPE).tar.gz"
VERSION = "BRANCH_$(REV)_$(BUILD_TYPE):$(BUILDREV)"
TOP_LEVEL=../..
DEBUG=true
else	
ifeq ($(TRUNK),1)		
RELEASE = "trunk"
REV = $(shell svn info | grep '^Revision:' | sed -e 's/Revision: //')
INSTALL_FILENAME = "$(FILENAME)_devbuild_$(REV)_$(BUILD_TYPE).tar.gz"
VERSION = "BUILD_$(REV)_$(BUILD_TYPE)"
TOP_LEVEL=..
else	
ifeq ($(TAG),1)
RELEASE = "tag"
REV = $(shell pwd | sed -e 's/.*\/tags\/\(.*\)\/oodlc/\1/')
INSTALL_FILENAME = "$(FILENAME)_release_$(REV)_$(BUILD_TYPE).tar.gz"
VERSION = "$(REV)_$(BUILD_TYPE)"
TOP_LEVEL=../..
else	
RELEASE = "unknown"
INSTALL_FILENAME = "$(FILENAME)_unknown_branch.tar.gz"
TOP_LEVEL=unknown
VERSION = "unknown"
endif
endif	
endif	

INSTALL_DIR = $(TOP_LEVEL)/build/$(PLATFORM)
LOCAL_INSTALL = local-install


# Standard Build Options
# ==============================================================================================

.PHONY: software enhanced full system

software:
	@$(MAKE) system BUILD_TYPE=$@

enhanced:
	@$(MAKE) system BUILD_TYPE=$@

full:
	@$(MAKE) system BUILD_TYPE=$@

system:
	@$(MAKE) check-build-type
	@$(MAKE) build-directories
	@$(MAKE) links
	@$(MAKE) build-system
ifneq ($(DEBUG),)
	@echo "Adding debug options ..."
	@$(MAKE) debug-files
endif
	@$(MAKE) instructions

.PHONY: compiler
compiler: 
	@$(MAKE) check-build-type
	@$(MAKE) build-directories
	@$(MAKE) links
	@$(MAKE) build-system
	@$(MAKE) matlab-files
	@$(MAKE) install-locally
	@$(MAKE) matlab-installer
	@$(MAKE) instructions


# Build Directory per Build Type
# ==============================================================================================

build-examples: $(BUILD_DIR)/examples
	@echo "Building examples"
	@cd tests; $(CP) $(EXAMPLES) ../$(BUILD_DIR)/examples
ifdef $(DYN)
	@cd tests; $(CP) $(DYN) ../$(BUILD_DIR)/examples
endif
ifdef $(DPL)
	@cd tests; $(CP) $(DPL) ../$(BUILD_DIR)/examples
endif

build-packages: $(BUILD_DIR)/packages
#	@echo "Adding packages to build directory ..."
ifneq ($(PKGS),)
	@cd ../scripts; $(CP) $(PKGS) ../oodlc/$(BUILD_DIR)/packages
endif

build-octave: $(BUILD_DIR)/octave
	@$(CP) library/octave/*.m $(BUILD_DIR)/octave

ifneq ($(BUILD_DOCS),)
DOCUMENTATION_BUILD_FILES = $(BUILD_DIR)/doc/developersLanguageGuide.pdf #$(BUILD_DIR)/doc/idynamoUsersGuide.pdf 
else
DOCUMENTATION_BUILD_FILES = 
endif

VERILOG_IMPL_FILES = $(BUILD_DIR)/data/impl/top_level_model.v $(BUILD_DIR)/data/impl/top_level_model.ucf $(BUILD_DIR)/data/impl/model_controller.v $(BUILD_DIR)/data/impl/model_llc_wrapper.v $(BUILD_DIR)/data/impl/fifo_commands.vh $(BUILD_DIR)/data/impl/DIFF_CLOCK_OUT.v $(BUILD_DIR)/data/impl/llc_clock.v $(BUILD_DIR)/data/impl/llc_interface.v $(BUILD_DIR)/data/impl/llc_interface.ngc $(BUILD_DIR)/data/impl/llc_fifo.ngc
VERILOG_SIM_FILES = $(BUILD_DIR)/data/sim/top_level_model_tb.v $(BUILD_DIR)/data/sim/model_controller.v $(BUILD_DIR)/data/sim/model_llc_wrapper.v $(BUILD_DIR)/data/sim/fifo_commands.vh $(BUILD_DIR)/bin/vwrapper

FACTORY_FILES = 

#COMMON_BUILD_FILES = $(OUTPUT_FILE) $(BUILD_DIR)/lib/core.h $(BUILD_DIR)/lib/swbe_main.c $(BUILD_DIR)/lib/llc_defs.h $(BUILD_DIR)/lib/llc_protocol.c $(BUILD_DIR)/data/default.dol $(BUILD_DIR)/bin/settings.csh $(BUILD_DIR)/bin/settings.sh $(BUILD_DIR)/README build-examples build-packages build-octave $(FILENAME) idynamo ../idynamo build_includes $(DOCUMENTATION_BUILD_FILES) $(ITEST) ../$(ITEST) $(BUILD_DIR)/$(DSLC) $(BUILD_DIR)/$(DSLC)/dslc.so
COMMON_BUILD_FILES = $(OUTPUT_FILE) $(BUILD_DIR)/data/default.dol build-examples $(FILENAME) build_includes build_shared $(MATLAB_BUILD_FILES)

FIXEDPT_BUILD_FILES =  $(BUILD_DIR)/lib/fixpt.h $(BUILD_DIR)/lib/floatpt.h $(BUILD_DIR)/bin/fixpt_report.pl $(BUILD_DIR)/bin/compare_backends.pl $(BUILD_DIR)/bin/compare_sims.pl $(BUILD_DIR)/bin/create_table.pl
HARDWARE_BUILD_FILES = $(BUILD_DIR)/bin/verify_xflow.pl $(VERILOG_IMPL_FILES) $(VERILOG_SIM_FILES) $(FACTORY_FILES)
TESTING_BUILD_FILES = $(ITEST) ../$(ITEST)


ifeq ($(BUILD_TYPE),software)
build-system: $(COMMON_BUILD_FILES)
endif

ifeq ($(BUILD_TYPE),enhanced)
build-system: $(COMMON_BUILD_FILES) $(FIXEDPT_BUILD_FILES)
endif

ifeq ($(BUILD_TYPE),full)
build-system: $(COMMON_BUILD_FILES) $(FIXEDPT_BUILD_FILES) $(HARDWARE_BUILD_FILES)
endif


# Link Generation
# ==============================================================================================

links: $(BUILD_DIR)
	@$(RM) build
	@ln -s $(BUILD_DIR) build


# Compiler Dependencies
# ==============================================================================================

COMPILER_SOURCE = src/*.s* src/ir/*.s* src/ir/datastructs/*.s* src/ir/processing/*.s* src/ir/patterns/*.s* src/be/*.s* src/library/*.s* src/objects/*.s* src/shell/*.s* src/util/*.s* src/util/error/*.s* src/util/files/*.s* src/util/printing/*.s* src/util/registry/*.s* src/cool.mlb

COMPILER_DEPENDENCIES = $(COMPILER_SOURCE) src/shell/cool.lex.sml src/shell/cool.grm.sig src/shell/cool.grm.sml src/shell/cool.lex src/shell/cool.grm src/util/registry/registry.lex.sml src/util/registry/registry.grm.sig src/util/registry/registry.grm.sml src/util/registry/registry.lex src/util/registry/registry.grm

COMPILER_PREREQS := check-build-type build-directories links build-system matlab-files matlab-installer instructions

# Dynamo Compilation Targets
# ==============================================================================================

src/shell/cool.lex.sml: src/shell/cool.lex
	$(RM) src/shell/cool.lex.sml
	mllex src/shell/cool.lex
	chmod -w src/shell/cool.lex.sml

src/shell/cool.grm.sig src/shell/cool.grm.sml: src/shell/cool.grm
	$(RM) src/shell/cool.grm.*
	mlyacc src/shell/cool.grm
	chmod -w src/shell/cool.grm.*

src/util/registry/registry.lex.sml: src/util/registry/registry.lex
	$(RM) src/util/registry/registry.lex.sml
	mllex src/util/registry/registry.lex
	chmod -w src/util/registry/registry.lex.sml

src/util/registry/registry.grm.sig src/util/registry/registry.grm.sml: src/util/registry/registry.grm
	$(RM) src/util/registry/registry.grm.*
	mlyacc src/util/registry/registry.grm
	chmod -w src/util/registry/registry.grm.*


$(OUTPUT_FILE): $(COMPILER_DEPENDENCIES)
	@$(RM) datafiles/$(BUILD_TYPE)-build_options.sml
	@$(MAKE) build_defs
	$(MLTON) $(ARGS) -output $(OUTPUT_FILE) src/compiler_exec.mlb 


libs:  library/stdlib.so


.c.o:
	gcc $(LINKING_FLAG) -c $? -o $@

library/stdlib.so: src/dll/arithmetic.o src/dll/comparison.o src/dll/math.o
	gcc $(SHARED_FLAG) -W1,-soname,stdlib.so -o $@ src/dll/*.o -lc

$(BUILD_DIR)/lib/stdlib.so: library/stdlib.so
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 


# Compiler Debugging Files
# ==============================================================================================

UNITTESTDIR = ../testing/tests/unit/compiler

UNITTEST_DEPENDENCIES = $(UNITTESTDIR)/*.sml $(UNITTESTDIR)/*.mlb $(UNITTESTDIR)/be/optimizations/*.sml $(UNITTESTDIR)/be/hwgen/*.sml

COMPONENTTESTDIR = ../testing/frameworks/component

COMPONENTTEST_DEPENDENCIES = src/be/hwgen/ver_components/*.sml $(COMPONENTTESTDIR)/ml_component.sml $(COMPONENTTESTDIR)/mlcomponent.mlb 

FIXPT_DEPENDENCIES = src/be/hwgen/genhwbe.sml $(COMPONENTTESTDIR)/fixpt2real.sml $(COMPONENTTESTDIR)/fixpt2real.mlb

$(BUILD_DIR)/debug/$(FILENAME)_profiletime: $(COMPILER_DEPENDENCIES)
	@$(MAKE) check-build-type
	-$(MLTON) $(SIMPLE_ARGS) -profile-stack true -profile time -output $@ src/compiler_exec.mlb 

$(BUILD_DIR)/debug/$(FILENAME)_profilemem: $(COMPILER_DEPENDENCIES)
	@$(MAKE) check-build-type
	-$(MLTON) $(SIMPLE_ARGS) -profile alloc -output $@ src/compiler_exec.mlb 

profiling-tools: 
	@$(MAKE) $(BUILD_DIR)/debug/$(FILENAME)_profiletime 
	@$(MAKE) $(BUILD_DIR)/debug/$(FILENAME)_profilemem

$(BUILD_DIR)/debug/compiler_unittest: $(COMPILER_DEPENDENCIES) $(UNITTEST_DEPENDENCIES)
	$(MLTON) $(ARGS) -output $@ $(UNITTESTDIR)/unittest.mlb

$(BUILD_DIR)/debug/compiler_componenttest: $(COMPONENTTEST_DEPENDENCIES)
	$(MLTON) $(ARGS) -output $@ $(COMPONENTTESTDIR)/mlcomponent.mlb

$(BUILD_DIR)/debug/fixpt2real: $(COMPONENTTEST_DEPENDENCIES)
	$(MLTON) $(ARGS) -output $@ $(COMPONENTTESTDIR)/fixpt2real.mlb

DEBUG_FILES = #$(BUILD_DIR)/debug/compiler_unittest $(BUILD_DIR)/debug/compiler_componenttest $(BUILD_DIR)/debug/fixpt2real

debug-files: $(DEBUG_FILES)

# Matlab build target
# ==============================================================================================


$(BUILD_DIR)/bin/%: matlab/bin/%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/src/simEngine_wrapper.c: matlab/src/simEngine_wrapper.c
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/src/simex_helper.c: matlab/src/simex_helper.c
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/%.m: matlab/%.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
$(BUILD_DIR)/%.fig: matlab/%.fig
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simplot.m: matlab/simplot.m
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simSensitivity.m: matlab/simSensitivity.m
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simview.m: matlab/simview.m
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simview.fig: matlab/simview.fig
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simsweep.%: matlab/simsweep.%
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

# $(BUILD_DIR)/simex.m: matlab/simex.m
# 	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

#$(BUILD_DIR)/lib/solvers.c: matlab/solvers.c
#	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/lib/libsolvers.a: solvers/lib/libsolvers.a
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/lib/libcvode_%.a: solvers/lib/libcvode_%.a
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/include/solvers.h: solvers/include/solvers.h
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

MEXINTERFACE_PREREQS = $(addprefix matlab/,$(SIMEX_HELPER) $(SIMENGINE_WRAPPER))
.PHONY: mexinterface
mexinterface: $(MEXINTERFACE_PREREQS)

matlab/install.m: simEngine.tgz datafiles/simatrapublickey.mat
	@(cd matlab; $(OCTAVECMD) "append_bin_to_file('../datafiles/simatrapublickey.mat', 'install_wrapper.m', 'install_temp.m', 'savePublicKey'); exit"; $(OCTAVECMD) "append_bin_to_file('../simEngine.tgz', 'install_temp.m', 'install.m', 'saveInstaller'); exit") 

matlab/install.p: matlab/install.m
	@(cd matlab; $(MATLABCMD) "pcode install.m; exit")

install.p: matlab/install.p
	@echo "Copying '$<' to '$@' ..."
	@cat $< | sed -e 's/function install_wrapper/function install/' > $@

pcode: install.p

MATLAB_GUI_FILES = $(addsuffix .fig, $(addprefix $(BUILD_DIR)/, simsweepdemo simex_gui simsweep_gui))
MATLAB_SCRIPTS = $(addsuffix .m, $(addprefix $(BUILD_DIR)/, simex simplot simSensitivity simsweep simsweepdemo demo_models simex_gui getAveISI getAveSpikeHeight getMaxISI getMinISI getNumSpikes simsweep_gui))
MATLAB_FILES = $(MATLAB_GUI_FILES) $(MATLAB_SCRIPTS) $(BUILD_DIR)/src/simEngine_wrapper.c $(BUILD_DIR)/src/simex_helper.c $(BUILD_DIR)/include/solvers.h $(BUILD_DIR)/lib/libsolvers.a $(BUILD_DIR)/lib/libcvode_float.a $(BUILD_DIR)/lib/libcvode_double.a simEngine.tgz install-locally matlab/install.m
MATLAB_FILES += $(addprefix $(BUILD_DIR)/,$(SIMEX_HELPER) $(SIMENGINE_WRAPPER))

solvers/lib/libcvode_%.a: solvers/src/*.c
	$(MAKE) -C solvers/thirdparty/cvode TYPE=$* libcvode_$*.a

solvers/lib/%.a: solvers/src/*.c
	@echo "Running solvers all on $@"
	$(MAKE) -C solvers all

.PHONY: solvers
solvers:
	@echo "Running target solvers"
	$(MAKE) -C solvers all


$(BUILD_DIR)/simex_helper.%: matlab/simex_helper.%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/simEngine_wrapper.%: matlab/simEngine_wrapper.%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(MATLAB_TARGETS): matlab/src/%.c include/simengine.h include/simengine_target.h
	@echo "Generating Matlab target $@"
	$(MAKE) -C matlab MEXEXT="$(MEXEXT)" DEBUG=$(DEBUG) PROFILE=$(PROFILE) $(*F)

matlab-files: $(MATLAB_FILES)

matlab-installer: $(MATLAB_FILES)

simEngine.tgz: $(OUTPUT_FILE) build/* build/*/*
	@echo "Creating $@ for packaging"
	@(cd $(BUILD_DIR); tar chfz ../$@ --exclude=\.svn .)

$(LOCAL_INSTALL):
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(LOCAL_INSTALL)/simEngine.tgz: simEngine.tgz
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@(cd $(LOCAL_INSTALL); tar xfz simEngine.tgz)

$(LOCAL_INSTALL)/compile_tests.m: matlab/compile_tests.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(LOCAL_INSTALL)/simex.m: matlab/simex.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(LOCAL_INSTALL)/%.mexmaci: matlab/%.mexmaci
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mex: matlab/%.mex
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexglx: matlab/%.mexglx
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexa64: matlab/%.mexa64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexs64: matlab/%.mexs64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexw32: matlab/%.mexw32
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexw64: matlab/%.mexw64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

install-locally: $(LOCAL_INSTALL) $(LOCAL_INSTALL)/simEngine.tgz $(addprefix $(LOCAL_INSTALL)/,$(SIMENGINE_WRAPPER)) $(LOCAL_INSTALL)/compile_tests.m $(LOCAL_INSTALL)/simex.m
#install-locally: $(LOCAL_INSTALL) $(LOCAL_INSTALL)/simEngine.tgz $(LOCAL_INSTALL)/compile_tests.m
	@echo "Local install at $(LOCAL_INSTALL) ready"

# Build type options
# ==============================================================================================


.PHONY: check-build-type
check-build-type:
	@echo " "
ifeq ($(BUILD_TYPE), software)
	@echo "Building software-only release ..."
else
ifeq ($(BUILD_TYPE), full)
	@echo "Building full release ..."	
else
ifeq ($(BUILD_TYPE), enhanced)
	@echo "Building software w/ fixed-point release ..."
else
	@echo "Unknown build type - exiting"
	@exit 1
endif
endif
endif
	@echo " "
#	@$(MAKE) build_defs

BUILD_OPTS = src/util/build_options.sml

ifeq ($(BUILD_TYPE), software)
build_defs: datafiles/software-build_options.sml
	@if ! cmp -s $< $(BUILD_OPTS) ; then \
	 echo "Copying '$<' to '$(BUILD_OPTS)' ..."; $(CP) $< $(BUILD_OPTS) ;\
	fi
else
ifeq ($(BUILD_TYPE), enhanced)
build_defs: datafiles/enhanced-build_options.sml
	@if ! cmp -s $< $(BUILD_OPTS) ; then \
	 echo "Copying '$<' to '$(BUILD_OPTS)' ..."; $(CP) $< $(BUILD_OPTS) ;\
	fi
else
ifeq ($(BUILD_TYPE), full)
build_defs: datafiles/full-build_options.sml
	@if ! cmp -s $< $(BUILD_OPTS) ; then \
	 echo "Copying '$<' to '$(BUILD_OPTS)' ..."; $(CP) $< $(BUILD_OPTS) ;\
	fi
else
build_defs:
	@echo "Unknown build type - exiting"
	@exit 1
endif
endif
endif

ifeq ($(BRANCH),1)
DEV_VERSION = true
else
DEV_VERSION = false
endif

datafiles/software-build_options.sml: datafiles
	@echo "Creating $(BUILD_TYPE) build options file ..."
	@echo "structure BuildOptions = struct" > $@
	@echo "val allowSWBackend = true val allowFPBackend = false val allowHWBackend = false val version = \"$(VERSION_NUMBER)\"" >> $@
	@echo "val build=\"$(VERSION)\" val build_date = \"$(BUILD_DATE)\"" >> $@
	@echo "val dev_version = $(DEV_VERSION)" >> $@
	@echo "end" >> $@

datafiles/enhanced-build_options.sml: datafiles
	@echo "Creating $(BUILD_TYPE) build options file ..."
	@echo "structure BuildOptions = struct" > $@
	@echo "val allowSWBackend = true val allowFPBackend = true val allowHWBackend = false val version = \"$(VERSION_NUMBER)\"" >> $@
	@echo "val build=\"$(VERSION)\" val build_date = \"$(BUILD_DATE)\"" >> $@
	@echo "val dev_version = $(DEV_VERSION)" >> $@
	@echo "end" >> $@

datafiles/full-build_options.sml: datafiles
	@echo "Creating $(BUILD_TYPE) build options file ..."
	@echo "structure BuildOptions = struct" > $@
	@echo "val allowSWBackend = true val allowFPBackend = true val allowHWBackend = true val version = \"$(VERSION_NUMBER)\"" >> $@
	@echo "val build=\"$(VERSION)\" val build_date = \"$(BUILD_DATE)\"" >> $@
	@echo "val dev_version = $(DEV_VERSION)" >> $@
	@echo "end" >> $@


# Dynamo Library Targets
# ==============================================================================================


$(BUILD_DIR)/lib/core.h: library/core.h
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 

$(BUILD_DIR)/lib/fixpt.h: library/fixpt.h
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 

$(BUILD_DIR)/lib/floatpt.h: library/floatpt.h
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 

$(BUILD_DIR)/lib/swbe_main.c: library/swbe_main.c
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/lib/llc_defs.h: ../dslc/llc_defs.h
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/lib/llc_protocol.c: ../dslc/llc_protocol.c
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

build_includes: include/* solvers
	@echo "Populating standard includes ..."
	@$(CP) include/* $(BUILD_DIR)/include/
	@$(MKDIR) $(BUILD_DIR)/include/solvers/
	@cp -fL solvers/target_GPU_float/*.cu $(BUILD_DIR)/include/solvers/

build_shared: share/*
	@echo "Populating shared files..."
	$(CP) $^ $(BUILD_DIR)/share/

$(BUILD_DIR)/data/default.dol: datafiles/default.dol
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 

# Run scripts
# ==============================================================================================

$(BUILD_DIR)/bin/create_table.pl: ../scripts/perl/create_table.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/change2tab.pl: ../scripts/perl/change2tab.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/verify_xflow.pl: ../scripts/perl/verify_xflow.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/fixed2float.pl: ../scripts/perl/fixed2float.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/dpl_asm.pl: ../scripts/perl/dpl_asm.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/fixpt_report.pl: ../scripts/perl/fixpt_report.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/compare_backends.pl: ../scripts/perl/compare_backends.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/compare_sims.pl: ../scripts/perl/compare_sims.pl
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@



# IDynamo Files
# ==============================================================================================

SRC_IDYNAMO_FILES = ../scripts/ruby/idynamo/*.rb
DEST_IDYNAMO_FILES = $(shell for file in $(SRC_IDYNAMO_FILES); do echo "$(BUILD_DIR)/bin/src/"`basename $$file`; done)

$(BUILD_DIR)/bin/src/%.rb: ../scripts/ruby/idynamo/%.rb
	@echo "Copying '$?' to '$@'"
	@$(CP) $? $@


idynamo-files: $(DEST_IDYNAMO_FILES)

/tmp/$(USER):
	@$(MKDIR) $@

ifeq ($(UNAME), Linux)
idynamo: $(BUILD_DIR)/bin/idynamo # need to make it idynamo.rb temporarily until the exe problem can be fixed
	@$(RM) $@
	@echo "Making link to interactive Dynamo executable ..."
	@ln -s $< $@

../idynamo: $(BUILD_DIR)/bin/idynamo # need to make it idynamo.rb temporarily until the exe problem can be fixed
	@$(RM) $@
	@echo "Making link to interactive Dynamo executable in top-level directory..."
	@(cd ..; ln -s oodlc/$< idynamo)

$(BUILD_DIR)/bin/idynamo_linux: ../scripts/ruby/idynamo_linux
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@chmod +x $@

$(BUILD_DIR)/bin/idynamo: $(DEST_IDYNAMO_FILES)
	@echo "Creating wrapper script for iDynamo shell ..."
	@echo "#!/bin/sh" > $@
ifeq ($(BUILD_TYPE), full)
	@echo "$(SETUP_ENV)" >> $@
endif
	@echo "export DYNAMO=`pwd`/build" >> $@
	@echo "export RUBYLIB=`pwd`/build/dslc" >> $@
	@echo "export BUILD_TYPE=$(BUILD_TYPE)" >> $@
	@echo "export BUILD_VERSION='$(VERSION)'" >> $@
	@echo "export BUILD_DATE='$(BUILD_DATE)'" >> $@
	@echo "(($(RUBY) -I `pwd`/build/bin/src `pwd`/build/bin/src/idynamo.rb --home-directory=$(HOME)" '$$*' "| tee idynamo_output.log) 3>&1 1>&2 2>&3 | tee idynamo_error.log) 3>&1 1>&2 2>&3" >> $@
	@chmod +x $@

$(BUILD_DIR)/bin/idynamo.sh: $(BUILD_DIR)/bin/idynamo_linux /tmp/$(USER)
	@echo "Creating wrapper script for iDynamo shell ..."
	@echo "#!/bin/sh" > $@
	@echo 'export HOME=/tmp/$$USER' >> $@
	@echo "export RUBYLIB=$(PARTIAL_INSTALL_BUILD_DIR)/dslc" >> $@
	@echo "export BUILD_TYPE=$(BUILD_TYPE)" >> $@
	@echo "export BUILD_VERSION='$(VERSION)'" >> $@
	@echo "export BUILD_DATE='$(BUILD_DATE)'" >> $@
# we can't include a setup_env because we don't know where it is stored
#ifeq ($(BUILD_TYPE), full)
#	@echo "$(SETUP_ENV)" >> $@
#endif
	@echo "export DYNAMO=$(PARTIAL_INSTALL_BUILD_DIR)" >> $@
	@echo "$(PARTIAL_INSTALL_BUILD_DIR)/bin/idynamo_linux" '$$*' >> $@
	@chmod +x $@


../scripts/ruby/idynamo_linux: ../scripts/ruby/idynamo.rb /tmp/$(USER)
	@echo "Creating interactive Dynamo executable ..."
	@(DYNAMO=`pwd`/build; export DYNAMO; cd ../scripts/ruby; export HOME=/tmp/$(USER); export RUBYLIB=../../oodlc/build/dslc; $(RUBY) rubyscript2exe.rb idynamo.rb --internal)

else
idynamo: $(BUILD_DIR)/bin/idynamo.rb
	@$(RM) $@
	@echo "Making link to interactive Dynamo script ..."
	@ln -s $< $@
endif

$(BUILD_DIR)/bin/idynamo.rb: ../scripts/ruby/idynamo.rb
	@echo "#!$(RUBY)" > $@
	@echo "Copying '$<' to '$@' ..."; cat $< >> $@
	@chmod +x $@

../scripts/ruby/idynamo.rb: ../scripts/ruby/idynamo/*.rb
	@echo "Creating interactive Dynamo script ..."
	@(cd ../scripts/ruby; export HOME=/tmp/$(USER); $(RUBY) tar2rubyscript.rb idynamo/)


# Verilog Related Files
# ==============================================================================================

VER_ML402 = ../interface/ml402
VER_COMMON = ../interface/common

$(BUILD_DIR)/data/impl/ModelInterface.v: $(VER_ML402)/ModelInterface.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/top_level_model.v: $(VER_ML402)/top_level_model.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/top_level_model.ucf: $(VER_ML402)/top_level_model.ucf
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/user_led.v: $(VER_COMMON)/user_led.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/toggle_ff.v: $(VER_COMMON)/toggle_ff.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/ff_synchronizer.v: $(VER_COMMON)/ff_synchronizer.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/watchdog.v: $(VER_COMMON)/watchdog.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/trigger_rst.v: $(VER_ML402)/trigger_rst.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/fault_counter.v: $(VER_COMMON)/fault_counter.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/sr_ff.v: $(VER_COMMON)/sr_ff.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/push_buttons.v: $(VER_COMMON)/push_buttons.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/button_leds.v: $(VER_COMMON)/button_leds.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/pwm.v: $(VER_COMMON)/pwm.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/oserdes_ddr_8.v: $(VER_COMMON)/oserdes_ddr_8.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/ddr_lvds_out_40.v: $(VER_COMMON)/ddr_lvds_out_40.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/one_shot.v: $(VER_COMMON)/one_shot.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/reset_button.v: $(VER_COMMON)/reset_button.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/serial_clk.v: $(VER_ML402)/serial_clk.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/flow_control.v: $(VER_ML402)/flow_control.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/power_on_reset.v: $(VER_COMMON)/power_on_reset.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/ddr_lvds_in_40.v: $(VER_COMMON)/ddr_lvds_in_40.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/iserdes_ddr_8.v: $(VER_COMMON)/iserdes_ddr_8.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/serial_debug.v: $(VER_ML402)/serial_debug.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/pulse_extend.v: $(VER_COMMON)/pulse_extend.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/model_controller.v: $(VER_ML402)/model_controller.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/llc_interface.v: $(VER_ML402)/llc_interface.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/llc_fifo.ngc: $(VER_ML402)/llc_fifo.ngc
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/sim/top_level_model_tb.v: $(VER_ML402)/top_level_model_tb.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/sim/model_controller.v: $(VER_ML402)/model_controller.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/sim/model_llc_wrapper.v: $(VER_ML402)/model_llc_wrapper.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/sim/fifo_commands.vh: $(VER_COMMON)/fifo_commands.vh
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/model_llc_wrapper.v: $(VER_ML402)/model_llc_wrapper.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/DIFF_CLOCK_OUT.v: $(VER_ML402)/DIFF_CLOCK_OUT.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/llc_clock.v: $(VER_ML402)/llc_clock.v
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/fifo_commands.vh: $(VER_COMMON)/fifo_commands.vh
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/data/impl/llc_interface.ngc: $(VER_ML402)/llc_interface.ngc
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/bin/vwrapper: ../scripts/c/vwrapper.c
	@echo "Compiling '$<' to '$@' ..."; $(CC) -o $@ $< -lpthread


# User and developer documents
# ==============================================================================================

$(BUILD_DIR)/doc/developersLanguageGuide.pdf: ../doc/developersLanguageGuide.pdf
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/doc/idynamoUsersGuide.pdf: ../doc/idynamoUsersGuide.pdf
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

../doc/%.pdf: ../doc/%.tex
	@(cd ../doc; $(MAKE) $(@F))

# Dynamo and build directory
# ==============================================================================================

ifneq ($(DEBUG),"")
DEBUG_DIRECTORY = $(BUILD_DIR)/debug
else
DEBUG_DIRECTORY =
endif

#COMMON_DIRECTORIES = $(BUILD_DIR) $(BUILD_DIR)/bin $(BUILD_DIR)/bin/src $(BUILD_DIR)/data $(BUILD_DIR)/examples $(BUILD_DIR)/include $(BUILD_DIR)/lib $(BUILD_DIR)/man $(BUILD_DIR)/packages $(DEBUG_DIRECTORY) $(BUILD_DIR)/octave $(BUILD_DIR)/doc

COMMON_DIRECTORIES = $(BUILD_DIR) $(BUILD_DIR)/data $(BUILD_DIR)/bin $(BUILD_DIR)/examples $(BUILD_DIR)/src $(BUILD_DIR)/include $(BUILD_DIR)/share $(BUILD_DIR)/lib $(BUILD_DIR)/doc

FULL_DIRECTORIES = $(COMMON_DIRECTORIES) $(BUILD_DIR)/data/impl $(BUILD_DIR)/data/sim  $(BUILD_DIR)/factory

ifeq ($(BUILD_TYPE),full)
build-directories: $(FULL_DIRECTORIES)
else
build-directories: $(COMMON_DIRECTORIES)
endif


$(FILENAME): 
	@$(RM) $@
	@ln -s build/bin/$(FILENAME) $(FILENAME)

$(BUILD_DIR)/bin: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/bin/src: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/data: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/data/impl: $(BUILD_DIR)/data
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/data/sim: $(BUILD_DIR)/data
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/dslc: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/examples: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/src: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/include: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/share: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/lib: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/man: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/packages: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/factory: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/debug: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/octave: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR)/doc: $(BUILD_DIR)
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(BUILD_DIR):
	@if test ! -d $@ ; then $(MKDIR) $@; fi


instructions:
	@echo " "
	@echo "To execute $(FILENAME), the SIMENGINE environmental variable must be set:"
	@echo "  The current value is '$$SIMENGINE'"
	@echo "  In (t)csh, use 'setenv SIMENGINE `pwd`/build'"
	@echo "  In bash, use 'export SIMENGINE=`pwd`/build'"
	@echo " "

$(BUILD_DIR)/bin/settings.csh:
	@echo "setenv SIMENGINE '$(INSTALL_BUILD_DIR)'" > $@

$(BUILD_DIR)/bin/settings.sh:
	@echo "export SIMENGINE='$(INSTALL_BUILD_DIR)'" > $@


# Installer
# ==============================================================================================

$(BUILD_DIR)/README:
	@echo "Dynamo Build Files (Build=$(REV):$(BUILD_TYPE))" > $@
	@echo "Copyright 2007-2008, Simatra Modeling Technologies, L.L.C." >> $@
	@echo " " >> $@
	@echo "To begin, execute the appropriate source script for your shell" >> $@
	@echo " - if tcsh or csh, use $(INSTALL_BUILD_DIR)/bin/settings.csh" >> $@
	@echo " - if bash, ksh, or sh, use $(INSTALL_BUILD_DIR)/bin/settings.sh" >> $@
	@echo " " >> $@
ifeq ($(BUILD_TYPE),full)
	@echo "  To reprogram the host via JTAG, execute program_ml403 in the $(BUILD_DIR)/factory" >> $@
	@echo "directory and follow the onscreen directions" >> $@
	@echo " " >> $@
endif
	@echo " " >> $@


installer: system
	@echo "Generating the install tar ball for a '$(RELEASE)' release ..."
	@$(MAKE) $(INSTALL_FILENAME)

simEngine_%.tar.gz: $(OUTPUT_FILE) build/* build/*/*
	@tar chvfz $(INSTALL_FILENAME) --exclude=\.svn build
	@echo "$(INSTALL_FILENAME) is ready for release"

build-release:
	@$(MAKE) installer
	@echo " "
	@echo "Checking if build already exists in repository"
	@-svn list $(SVNSRV)/build/$(PLATFORM)/$(INSTALL_FILENAME) | grep $(INSTALL_FILENAME) > check_file_exists
	@if [ -s check_file_exists ] ; then \
	 echo "Build already exists, Current build being replaced with new build" ;\
	 svn rm -m "removing $(INSTALL_FILENAME) to replace with a new version" $(SVNSRV)/build/$(PLATFORM)/$(INSTALL_FILENAME) ; \
	fi
	@$(RM) check_file_exists
	@echo "Creating build for release [$(RELEASE) -> $(REV)]"
	@svn add $(INSTALL_FILENAME) > /dev/null
	@$(CP) $(INSTALL_FILENAME) makefile_temp_tar_file
	svn copy -m "Committing build [$(RELEASE) -> $(REV)]" $(INSTALL_FILENAME) $(SVNSRV)/build/$(PLATFORM)
	@svn rm --force $(INSTALL_FILENAME) > /dev/null
	@mv makefile_temp_tar_file $(INSTALL_FILENAME)

build-all:
	@$(MAKE) system build-release BUILD_TYPE=software
	@$(MAKE) system build-release BUILD_TYPE=enhanced
	@$(MAKE) system build-release BUILD_TYPE=full

$(TOP_LEVEL)/build:
	@echo "Build directory <$@> does not exist"
	@echo "Updating top level makefile"
	@(cd $(TOP_LEVEL); svn update -N Makefile; make build)

# Subversion Related
# ==============================================================================================

ifeq ($(BRANCH),1)
update-from-trunk:
	@echo "Updating branch from trunk ..."
	@echo "First updating the branch to make sure it is up to date"
	@svn update
	@echo "Determining the first copy revision number ..."; \
	 startrev=`svn log --stop-on-copy 2>/dev/null | egrep '^r[0-9]+ \|' | tail -1 | awk '{print $$1}' | sed -e 's/r//'`; \
	 echo "Merging from trunk revision $$startrev ..."; \
	 svn merge -r$$startrev:HEAD $(SVNTRUNK); \
	 echo " -> Resolve any conflicts marked with a 'C'"; \
	 echo " -> Execute 'svn commit -m \"Updated branch with trunk\"' in directory '..'"

#http://www.hosted-projects.com/trac/simatra/dynamo/changeset?old_path=trunk&old=644&new_path=branches%2Frweinstein_299&new=701
show-changes:
	@echo "Opening $(BROWSER) to show changes in branch relative to trunk"
	@startrev=`svn log --stop-on-copy 2>/dev/null | egrep '^r[0-9]+ \|' | tail -1 | awk '{print $$1}' | sed -e 's/r//'`; \
	 website=$(SVNWEBSRV)/changeset?old_path=trunk\&old=$$startrev\&new_path=branches%2F$(REV)\&new=HEAD; \
	 echo "Opening website: $$website"; \
	 $(BROWSER) $$website
show-changes-vs-trunk:
	@echo "Opening $(BROWSER) to show changes in branch relative to trunk"
	@website=$(SVNWEBSRV)/changeset?old_path=trunk\&old=HEAD\&new_path=branches%2F$(REV)\&new=HEAD; \
	 echo "Opening website: $$website"; \
	 $(BROWSER) $$website
endif


# System Test
# ==============================================================================================

system-test:
	@echo "Executing the system-test framework"
	@chmod +x $(SYSTESTDIR)/$(SYSTEST)
	@(cd $(SYSTESTDIR); ./$(SYSTEST) -l)
	@echo "Enter the suite number or 'a' for all the tests"; \
	 read value; \
	 if [ $$value == "a" ] ; then \
	  echo "Running all tests"; \
	  (SIMENGINE=`pwd`/build; export SIMENGINE; cd $(SYSTESTDIR); ./$(SYSTEST) -a); \
	 else \
	  echo "Running suite #$$value"; \
	  (SIMENGINE=`pwd`/build; export SIMENGINE; cd $(SYSTESTDIR); ./$(SYSTEST) -s $$value); \
	 fi


nightly-test: $(ITEST)
	@echo "Executing the nightly tests"
	@(for module in $$(./$(ITEST) list|tail -n+2); do \
	    sqlite3=$(ITEST_NIGHTLY_OUTPUT_DIR)/db/nightly-test.sqlite3; \
	    if [ ! -d `dirname $$sqlite3` ]; then mkdir -p `dirname $$sqlite3`; fi; \
	    ./$(ITEST) --quiet --sqlite3=$$sqlite3 $$module; \
	  done;)

$(BUILD_DIR)/bin/itest: #system
	@echo "Creating wrapper script for itest ..."
	@echo "#!/bin/sh" > $@
	@echo "export SIMENGINE='$$(pwd)/build'" >> $@
	@echo "export CUR_DIR=\`pwd\`" >> $@
	@echo 'itest_dir=$${SIMENGINE}/../../testing/framework' >> $@
	@echo 'ruby -C$${itest_dir} itest.rb $$*' >> $@
	@chmod +x $@

$(ITEST): $(BUILD_DIR)/bin/itest
	@$(RM) $@
	@echo "Making link to itest executable ..."
	@ln -s $< $@

../$(ITEST): $(BUILD_DIR)/bin/itest	
	@$(RM) $@
	@echo "Making link to itest executable in top-level directory ..."
	@ln -s oodlc/$< $@


# DSLC
# ==============================================================================================
$(BUILD_DIR)/$(DSLC)/dslc.so: ../dslc/*.c ../dslc/*.h 
	@(cd ../dslc/ ; make)
	@$(CP) ../dslc/dslc.so $(BUILD_DIR)/$(DSLC)

# RPM
# ==============================================================================================

rpm-setup: $(BUILD_DIR)/bin/idynamo.sh
	@echo "Building directories for RPM"
	@$(MKDIR) ~/rpm
	@$(MKDIR) ~/rpm/BUILD
	@$(MKDIR) ~/rpm/RPMS
	@$(MKDIR) ~/rpm/RPMS/i386
	@$(MKDIR) ~/rpm/SOURCES
	@$(MKDIR) ~/rpm/SPECS
	@$(MKDIR) ~/rpm/SRPMS
	@echo "%_topdir /home/`whoami`/rpm" > ~/.rpmmacros

SPEC = datafiles/dynamo-$(BUILD_TYPE).spec

$(SPEC):
	@$(MAKE) BUILD_TYPE=$(BUILD_TYPE) system 
	@echo "Summary: Simatra Dynamo Compiler" > $@
	@echo "Name: dynamo" >> $@
	@echo "Version: $(VERSION_NUMBER)" >> $@
	@echo "Release: $(VERSION)" >> $@
	@echo "License: By NDA only" >> $@
	@echo "Group: Applications/Engineering" >> $@
	@echo "URL: http://www.simatratechnologies.com" >> $@
	@echo "Source0: %{name}-%{version}-$(BUILD_TYPE).tar.gz" >> $@
	@echo "BuildRoot: %{_tmppath}/%{name}-%{version}-root" >> $@
	@echo "Requires: gnuplot" >> $@
	@echo "Requires: octave" >> $@
	@echo " " >> $@
# The below two lines are because comments were stripped out of the ELF file making the rubyscript2exe script die
	@echo "%define __spec_install_post :" >> $@
	@echo "%define debug_package %{nil}" >> $@
	@echo " " >> $@
	@echo "%description" >> $@
	@echo "Dynamo is a dynamical system compiler to aid scientists and engineers to develop " >> $@
	@echo "mathematical models for describing biological phenomenom." >> $@
	@echo " " >> $@
	@echo "%prep" >> $@
	@echo " " >> $@
	@echo "%setup -q" >> $@
	@echo " " >> $@
	@echo "%build" >> $@
	@echo "cd oodlc; make VERSION=\"$(VERSION)\" $(BUILD_TYPE)" >> $@
	@echo " " >> $@
	@echo "%install" >> $@
	@echo "rm -rf \$$RPM_BUILD_ROOT" >> $@
	@echo "cd oodlc; make DESTDIR=\$$RPM_BUILD_ROOT BUILD_TYPE=$(BUILD_TYPE) install" >> $@
	@echo " " >> $@
	@echo "%clean" >> $@
	@echo "rm -rf \$$RPM_BUILD_ROOT" >> $@
	@echo " " >> $@
	@echo "%files" >> $@
	@echo "%defattr(-,root,root,-)" >> $@
	@echo "$(INSTALL_BIN_DIR)/$(FILENAME)" >> $@
	@echo "$(INSTALL_BIN_DIR)/i$(FILENAME)" >> $@
	@find build/ | grep -v '\.svn' | grep -v '\~$$' | grep -v 'idynamo.sh' | sed -e 's|build|$(PARTIAL_INSTALL_BUILD_DIR)|' >> $@
	@echo "$(INSTALL_BULID_DIR)" >> $@
	@echo "%doc" >> $@
	@echo " " >> $@
	@echo "%changelog" >> $@

RPM_TAR_GZ = dynamo-$(VERSION_NUMBER)-$(BUILD_TYPE).tar.gz
RPM_DIR = ~/rpm
RPM_TEMP_DIR = $(FILENAME)-$(VERSION_NUMBER)

rpm:
	@$(MAKE) rpm-setup
	@echo "Creating rpm ..."
	@$(RM) $(SPEC)
	@$(MAKE) $(SPEC)
	(cd ..; $(RM) $(RPM_TAR_GZ))
	(cd ..; $(RMDIR) $(RPM_TEMP_DIR))
	(cd ..; tar cfz $(RPM_TAR_GZ) --exclude=\.svn --exclude=\.tar\.gz --exclude=\*\.\*\~ .)
	$(MKDIR) ../$(RPM_TEMP_DIR)
	(cd ../$(RPM_TEMP_DIR); tar xfz ../$(RPM_TAR_GZ))
# now that we have copied all the source, make one small change to the idynamo script
	(cd ../$(RPM_TEMP_DIR); mv oodlc/build/bin/idynamo.sh oodlc/build/bin/idynamo)
	(cd ..; $(RM) $(RPM_TAR_GZ))
	(cd ..; tar cfz $(RPM_TAR_GZ) $(RPM_TEMP_DIR))
	$(CP) ../$(RPM_TAR_GZ) $(RPM_DIR)/SOURCES
	rpmbuild -ba -v $(SPEC)
	@echo " "
	@echo "RPM Generated successfully"
	@echo "Install the RPM by running the following command as super user"
	@echo "rpm -ihv ~/rpm/RPMS/i386/$(FILENAME)-$(VERSION_NUMBER)-$(VERSION).i386.rpm"


# Installer
# ==============================================================================================

install: system
	@echo "Installing Dynamo in $(INSTALL_BUILD_DIR) ..."
	@$(RMDIR) $(INSTALL_BUILD_DIR)
	@-$(MKDIR) $(INSTALL_BUILD_DIR)
	@$(CP) build/* $(INSTALL_BUILD_DIR)
#	@gzip -d $(INSTALL_BUILD_DIR)/bin/i$(FILENAME)_save.gz
	@$(MKDIR) $(INSTALL_BIN_DIR)
#	(cd $(INSTALL_BUILD_DIR); ls -ld `find .`)
	@echo "Creating link to '$(FILENAME)' in $(INSTALL_BIN_DIR) ..."
	@$(RM) $(INSTALL_BIN_DIR)/$(FILENAME)
	@(cd $(INSTALL_BIN_DIR); ln -s $(PARTIAL_INSTALL_BUILD_DIR)/bin/$(FILENAME))
	@echo "Creating link to 'i$(FILENAME)' in $(INSTALL_BIN_DIR) ..."
	@$(RM) $(INSTALL_BIN_DIR)/i$(FILENAME)
	@(cd $(INSTALL_BIN_DIR); ln -s $(PARTIAL_INSTALL_BUILD_DIR)/bin/i$(FILENAME) i$(FILENAME))
	@echo "Install Completed"
	@cat $(BUILD_DIR)/README

# Clean Up
# ==============================================================================================


.PHONY: clean
clean: mostlyclean solvers_clean mexinterface_clean
	$(RM) *~
	$(RM) $(LOCAL_INSTALL)
	$(RM) examples/*~
	$(RM) src/*~
	$(RM) src/dyn_fe/*~
	$(RM) src/be/*~
	$(RM) src/be/optimizations/*~
	$(RM) src/be/util/*~
	$(RM) src/be/hwgen/*~
	$(RM) src/be/swgen/*~
	$(RM) src/be/core/*~
	$(RM) src/lib_fe/*~
	$(RM) src/dll/*.o
	$(RM) src/dll/*.so
	$(RM) $(BUILD_OPTS) datafiles/*build_options.sml
	$(RM) $(SPEC)
	$(RMDIR) $(BUILD)_*
	$(RM) $(BUILD)
	$(RMDIR) *.dso
	$(RM) ../scripts/ruby/idynamo_linux
	$(RM) ../scripts/ruby/idynamo.rb
	$(RM) $(FILENAME) idynamo ../idynamo $(INSTALL_FILENAME)
	$(RM) $(ITEST) ../$(ITEST) $(BUILD_DIR)/bin/itest
	$(RM) $(SPEC)
	$(RMDIR) ../$(RPM_TEMP_DIR)
	$(RM) ../$(RPM_TAR_GZ)
	$(RM) install.m
	$(RM) install.p

mostlyclean:
	$(RM) $(FILENAME)
	$(RM) src/dyn_fe/dynamo.lex.sml
	$(RM) src/dyn_fe/dynamo.grm.sig
	$(RM) src/dyn_fe/dynamo.grm.sml
	$(RM) src/dyn_fe/dynamo.grm.desc
	$(RM) src/lib_fe/dynlib.lex.sml
	$(RM) src/lib_fe/dynlib.grm.sig
	$(RM) src/lib_fe/dynlib.grm.sml
	$(RM) src/lib_fe/dynlib.grm.desc


solvers_clean:
	$(MAKE) -C solvers clean

mexinterface_clean:
	$(MAKE) -C matlab MEXEXT="$(MEXEXT)" clean

clean-all:
	$(MAKE) clean BUILD_TYPE=software
	$(MAKE) clean BUILD_TYPE=full
	$(MAKE) clean BUILD_TYPE=enhanced


