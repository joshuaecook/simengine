# Dynamo Compiler Makefile
# Copyright 2007-2010, Simatra Modeling Technologies, L.L.C.
include include/common.make

# User Adjustable Parameters
# ==============================================================================================

# BUILD_TYPE can be software (floating point only), enhanced (floating point and fixed point), or full
BUILD_TYPE = software

# DEBUG can be defined or not defined.  By default, it is defined always in a branch, but not defined in the trunk or a tag
#DEBUG = true

# Version # for RPM
VERSION_NUMBER = 0.92
BUILD_DATE = $(shell date)

DESTDIR = 
PARTIAL_INSTALL_BUILD_DIR = /usr/local/share/Simatra
INSTALL_BUILD_DIR = $(DESTDIR)$(PARTIAL_INSTALL_BUILD_DIR)
INSTALL_BIN_DIR = $(DESTDIR)/usr/local/bin

# Operating system and architecture inspection
UNAME := $(shell uname -s)

# Cross compiling flag for Leopard to Snow Leopard
ifneq ($(ARCH64),)
ARCH64_FLAG = ARCH64=$(ARCH64)
ARCH = $(ARCH64)
else
ARCH := $(shell arch 2>/dev/null)
endif

FIND_LATEX := $(shell which latex 2> /dev/null)

ifeq ($(FIND_LATEX),/opt/bin/latex)
BUILD_DOCS = true
endif

# Files in example directory to be copied into build directory
# In the oodlc, examples are in the tests directory under models
EXAMPLES = `cat ../examples/release_list | sed -e 's/\#.*//' | grep -v '^ *$$' `

# Help Screen
# ==============================================================================================


.PHONY: help
help:
	@echo " "
	@echo "**********************************************************"
	@echo "****        Simatra simEngine Compiler Makefile       ****"
	@echo "**********************************************************"
	@echo "****                                                  ****"
	@echo "****  make help - display this message                ****"
	@echo "****                                                  ****"
	@echo "****  make compiler - Just build the compiler         ****"
	@echo "****                  in accordance with BUILD_TYPE   ****"
	@echo "****                                                  ****"
	@echo "****  make compiler-64 - Build the compiler with libs ****"
	@echo "****                     in 64-bit                    ****"
	@echo "****                                                  ****"
	@echo "****  make pcode - Build a pcode installer            ****"
	@echo "****                                                  ****"
	@echo "****  make pcode-64 - Build a 64-bit pcode installer  ****"
	@echo "****                                                  ****"
	@echo "****  make profiling-tools - Builds the profiling     ****"
	@echo "****                         tools                    ****"
	@echo "****                                                  ****"
	@echo "****  make matlab-release-test - perform a system     ****"
	@echo "****                             'release' test       ****"
	@echo "****                                                  ****"
	@echo "****  make matlab-internal-test - perform a system    ****"
	@echo "****                              'internal' test     ****"
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
LINK := $(CC) -fPIC



SVNSRV = https://svn1.hosted-projects.com/simatra/simEngine
SVNWEBSRV = http://www.hosted-projects.com/trac/simatra/simEngine
SVNTRUNK = $(SVNSRV)/trunk
SRC = src/
BUILD = build
RM = rm -rf

SIMPLE_ARGS = -target-link-opt linux -ldl -export-header ffi/ffi-simengine.h
ARGS = $(SIMPLE_ARGS) -const 'Exn.keepHistory true'
ifneq ($(verbose),)
	SIMPLE_ARGS = -verbose 1
endif
ifneq ($(sml_typecheck),)
	SMLPPFLAGS += -stop tc
endif

CP = cp -rf

RMDIR = rm -rf
SYSTESTDIR = ../testing/frameworks/system
SYSTEST = systest
MAKE = make --no-print-directory
BROWSER = firefox
MATLABCMD = $(MATLAB) -nodisplay -nojvm -r 

ifeq ("$(MATLAB)$(OCTAVE)","")
$(error "Couldn't find MATLAB or GNU octave.")
endif


# Determines the appropriate MEX extension for the current platform.
export MEXEXT

ifneq ($(MATLAB),)
MEXEXT := .$(shell MATLABROOT=$(MATLAB_INSTALL_PATH) $(MATLAB_INSTALL_PATH)/bin/mexext)
endif

#ifneq ($(OCTAVE),)
#MEXEXT += .mex
#endif

# Add the Mac 64-bit version if required
ifneq ($(DARWIN),)
ifneq ($(ARCH64),)
MEXEXT += .mexmaci64
endif
endif

# File Names and Locations
# ==============================================================================================

FILENAME = simEngine
OUTPUT_FILE = $(BUILD_DIR)/bin/$(FILENAME)

# Platform Specific Parameters
# ==============================================================================================

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

BRANCH:=$(shell pwd | grep -c branches)
TRUNK:=$(shell pwd | grep -c trunk)
TAG:=$(shell pwd | grep -c tag)

ifeq ($(BRANCH),1)
RELEASE = "branch"
BUILDREV := $(shell svn info | grep '^Revision:' | sed -e 's/Revision: //')
REV := $(shell pwd | sed -e 's/.*\/branches\/\(.*\)/\1/')
#INSTALL_FILENAME = "$(FILENAME)_branch_$(REV):$(BUILDREV)_$(BUILD_TYPE).tar.gz"
VERSION = "BRANCH_$(REV)_$(BUILDREV)"
INSTALL_FILENAME = "install_$(PLATFORM)_$(ARCH)_$(VERSION)"
TOP_LEVEL=../..
DEBUG=true
else	
ifeq ($(TRUNK),1)		
RELEASE = "trunk"
REV := $(shell svn info | grep '^Revision:' | sed -e 's/Revision: //')
#INSTALL_FILENAME = "$(FILENAME)_devbuild_$(REV)_$(BUILD_TYPE).tar.gz"
VERSION = "BUILD_$(REV)"
INSTALL_FILENAME = "install_$(PLATFORM)_$(ARCH)_$(VERSION)"
TOP_LEVEL=..
else	
ifeq ($(TAG),1)
RELEASE = "tag"
REV := $(shell pwd | sed -e 's/.*\/tags\/\(.*\)/\1/')
#INSTALL_FILENAME = "$(FILENAME)_release_$(REV)_$(BUILD_TYPE).tar.gz"
INSTALL_FILENAME = "install_$(PLATFORM)_$(ARCH)"
VERSION = ""#"$(REV)_$(BUILD_TYPE)"
TOP_LEVEL=../..
else	
RELEASE = "unknown"
#INSTALL_FILENAME = "$(FILENAME)_unknown_branch.tar.gz"
INSTALL_FILENAME = "install_$(PLATFORM)_$(ARCH)"
TOP_LEVEL=unknown
VERSION = "unknown"
endif
endif	
endif	

INSTALL_DIR = $(TOP_LEVEL)/build/$(PLATFORM)
ifneq ($(ARCH64),)
LOCAL_INSTALL = local-install-64
else
LOCAL_INSTALL = local-install
endif

INSTALL_TGZ = "simEngine_$(PLATFORM)_$(ARCH)_$(VERSION_NUMBER).tgz"
simEngine_%.tgz: $(OUTPUT_FILE) build/* build/*/*
	@echo "Creating $@ for packaging"
	@(cd $(BUILD_DIR); tar chfz ../$@ --exclude=\.svn .)



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

.PHONY: compiler compiler-snow-leopard
compiler: 
	@$(MAKE) check-build-type
	@$(MAKE) build-directories
	@$(MAKE) links
	@$(MAKE) build-system
	@$(MAKE) matlab-files
#	@$(MAKE) matlab-installer
	@$(MAKE) $(INSTALL_TGZ)
	@$(MAKE) install-locally
	@$(MAKE) instructions

compiler-64:
	@$(MAKE) compiler ARCH64=x86_64

pcode-64:
	@$(MAKE) pcode ARCH64=x86_64

# Build Directory per Build Type
# ==============================================================================================

build-examples: $(BUILD_DIR)/examples
	@echo "Building examples"
#	@cd examples; $(CP) $(EXAMPLES) ../$(BUILD_DIR)/examples
	@cd ReleaseExamples; $(CP) * ../$(BUILD_DIR)/examples
ifdef $(DYN)
	@cd tests; $(CP) $(DYN) ../$(BUILD_DIR)/examples
endif
ifdef $(DPL)
	@cd tests; $(CP) $(DPL) ../$(BUILD_DIR)/examples
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

COMMON_BUILD_FILES = $(OUTPUT_FILE) $(BUILD_DIR)/data/default.dol build-examples $(FILENAME) build_includes build_shared $(BUILD_DIR)/bin/simlib $(BUILD_DIR)/lib/libcodegen.sim $(MATLAB_BUILD_FILES)

FIXEDPT_BUILD_FILES =  $(BUILD_DIR)/lib/fixpt.h $(BUILD_DIR)/lib/floatpt.h $(BUILD_DIR)/bin/fixpt_report.pl $(BUILD_DIR)/bin/compare_backends.pl $(BUILD_DIR)/bin/compare_sims.pl $(BUILD_DIR)/bin/create_table.pl
HARDWARE_BUILD_FILES = $(BUILD_DIR)/bin/verify_xflow.pl $(VERILOG_IMPL_FILES) $(VERILOG_SIM_FILES) $(FACTORY_FILES)


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

COMPILER_MLB = src/compiler_exec.mlb

COMPILER_SOURCE = src/*.s* src/ir/*.s* src/ir/datastructs/*.s* src/ir/math/*.s* src/ir/processing/*.s* src/ir/patterns/*.s* src/be/*.s* src/library/*.s* src/objects/*.s* src/shell/*.s* src/util/*.s* src/util/error/*.s* src/util/files/*.s* src/util/printing/*.s* src/util/registry/*.s* src/cool.mlb

COMPILER_DEPENDENCIES = $(COMPILER_SOURCE) src/shell/cool.lex.sml src/shell/cool.grm.sig src/shell/cool.grm.sml src/shell/cool.lex src/shell/cool.grm src/util/registry/registry.lex.sml src/util/registry/registry.grm.sig src/util/registry/registry.grm.sml src/util/registry/registry.lex src/util/registry/registry.grm ffi/simex.mlb

COMPILER_EXTRA_SOURCES = ffi/ffi-simengine.c

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


$(OUTPUT_FILE): override SMLFLAGS += $(ARGS)
$(OUTPUT_FILE): $(COMPILER_MLB) $(COMPILER_DEPENDENCIES) $(COMPILER_EXTRA_SOURCES)
	$(RM) datafiles/$(BUILD_TYPE)-build_options.sml
	$(MAKE) build_defs
	$(COMPILE.sml) -output $@ $< $(COMPILER_EXTRA_SOURCES)


libs:  library/stdlib.so


.c.o:
	gcc $(LINKING_FLAG) -c $? -o $@

library/stdlib.so: src/dll/arithmetic.o src/dll/comparison.o src/dll/math.o
	gcc $(SHARED_FLAGS) -W1,-soname,stdlib.so -o $@ src/dll/*.o -lc

$(BUILD_DIR)/lib/stdlib.so: library/stdlib.so
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 


# Compiler Debugging Files
# ==============================================================================================

$(BUILD_DIR)/debug/$(FILENAME)_profiletime: override SMLPPFLAGS += $(SIMPLE_ARGS) -profile-stack true -profile time
$(BUILD_DIR)/debug/$(FILENAME)_profiletimebug/$(FILENAME)_profiletime: $(COMPILER_MLB) $(COMPILER_DEPENDENCIES) $(COMPILER_EXTRA_SOURCES)
	@$(MAKE) check-build-type
	-$(COMPILE.sml) -outputs $@ $< $(COMPILER_EXTRA_SOURCES)


$(BUILD_DIR)/debug/$(FILENAME)_profilemem: override SMLPPFLAGS += $(SIMPLE_ARGS) -profile alloc
$(BUILD_DIR)/debug/$(FILENAME)_profilemem: $(COMPILER_MLB) $(COMPILER_DEPENDENCIES)
	@$(MAKE) check-build-type
	-$(COMPILE.sml) -outputs $@ $< $(COMPILER_EXTRA_SOURCES)

profiling-tools: 
	@$(MAKE) $(BUILD_DIR)/debug/$(FILENAME)_profiletime 
	@$(MAKE) $(BUILD_DIR)/debug/$(FILENAME)_profilemem

DEBUG_FILES = #$(BUILD_DIR)/debug/compiler_unittest $(BUILD_DIR)/debug/compiler_componenttest $(BUILD_DIR)/debug/fixpt2real

debug-files: $(DEBUG_FILES)

# Matlab build target
# ==============================================================================================


$(BUILD_DIR)/bin/%: interfaces/matlab/bin/%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/%.m: interfaces/matlab/%.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
$(BUILD_DIR)/%.fig: interfaces/matlab/%.fig
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/lib/libcodegen.sim: codegen/src/* codegen/src/*/*
	@$(MAKE) -C codegen codegen
	@echo "Copying 'codegen/lib/libcodegen.sim' to '$@' ..."; $(CP) codegen/lib/libcodegen.sim $@

$(BUILD_DIR)/lib/libcvode_%.a: solvers/lib/libcvode_%.a
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

.PHONY: mexinterface
mexinterface:
	$(MAKE) -C interfaces/matlab all


%.p: interfaces/matlab/%.m
	@($(MATLABCMD) "pcode $<; exit")

pcode: $(INSTALL_FILENAME).p

MATLAB_GUI_FILES = $(addsuffix .fig, $(addprefix $(BUILD_DIR)/, simsweepdemo simex_gui simsweep_gui))
MATLAB_SCRIPTS = $(addsuffix .m, $(addprefix $(BUILD_DIR)/, simex simplot simhelp simSensitivity simsweep simsweepdemo simexamplepath demo_models simex_gui getAveISI getAveSpikeHeight getMaxISI getMinISI getNumSpikes simsweep_gui))
MATLAB_FILES = $(MATLAB_GUI_FILES) $(MATLAB_SCRIPTS)
MATLAB_FILES += $(BUILD_DIR)/lib/libcvode_float.a $(BUILD_DIR)/lib/libcvode_double.a 
MATLAB_FILES += mexinterface

solvers/lib/libcvode_%.a: solvers

.PHONY: solvers
solvers:
	$(MAKE) -C solvers/cvode

.PHONY: share_cuda
share_cuda:
	@echo "Running target cuda_utils"
	$(MAKE) -C share/cuda all


$(BUILD_DIR)/simex_helper.%: interfaces/matlab/src/simex_helper.%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(BUILD_DIR)/simEngine_wrapper.%: interfaces/matlab/src/simEngine_wrapper.%
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

matlab-files: $(MATLAB_FILES)

matlab-installer: $(MATLAB_FILES)

simEngine.tgz: $(OUTPUT_FILE) build/* build/*/*
	@echo "Creating $@ for packaging"
	@(cd $(BUILD_DIR); tar chfz ../$@ --exclude=\.svn .)

$(LOCAL_INSTALL):
	@if test ! -d $@ ; then $(MKDIR) $@; fi

$(LOCAL_INSTALL)/simex:
	@$(MAKE) -C interfaces/c/ simex
	@echo "Copying 'interfaces/c/simex' to '$@' ..."; $(CP) interfaces/c/simex $@

$(LOCAL_INSTALL)/simEngine.tgz: simEngine.tgz
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@
	@(cd $(LOCAL_INSTALL); tar xfz simEngine.tgz)

$(LOCAL_INSTALL)/compile_tests.m: interfaces/matlab/compile_tests.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(LOCAL_INSTALL)/simex.m: interfaces/matlab/simex.m
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@

$(LOCAL_INSTALL)/%.mexmaci: interfaces/matlab/src/%.mexmaci
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexmaci64: interfaces/matlab/src/%.mexmaci64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mex: interfaces/matlab/src/%.mex
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexglx: interfaces/matlab/src/%.mexglx
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexa64: interfaces/matlab/src/%.mexa64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexs64: interfaces/matlab/src/%.mexs64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexw32: interfaces/matlab/src/%.mexw32
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

$(LOCAL_INSTALL)/%.mexw64: interfaces/matlab/src/%.mexw64
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@	

install-locally: $(LOCAL_INSTALL) $(LOCAL_INSTALL)/simEngine.tgz $(addprefix $(LOCAL_INSTALL)/,$(SIMEX_HELPER) $(SIMENGINE_WRAPPER)) $(LOCAL_INSTALL)/compile_tests.m $(LOCAL_INSTALL)/simex.m $(LOCAL_INSTALL)/simex
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
	@$(CP) -r solvers/include/* $(BUILD_DIR)/include
#	@$(MKDIR) $(BUILD_DIR)/include/solvers/
#	@cp -fL solvers/target_GPU_float/*.cu $(BUILD_DIR)/include/solvers/

build_shared: share/*
	@echo "Populating shared files..."
	@$(MAKE) share_cuda
	$(CP) $^ $(BUILD_DIR)/share/
	-$(CP) share/cuda/device_props $(BUILD_DIR)/bin/

$(BUILD_DIR)/data/default.dol: datafiles/default.dol
	@echo "Copying '$<' to '$@' ..."; $(CP) $< $@ 

$(BUILD_DIR)/bin/simlib: codegen/simlib/simlib.c
	@$(MAKE) -C codegen simlib/simlib $(ARCH64_FLAG)
	@echo "Copying 'codegen/simlib/simlib' to '$@' ..."; $(CP) codegen/simlib/simlib $@ 

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

matlab-release-test: compiler
	@echo "Executing the MATLAB release test framework"
	@matlab -nosplash -nodesktop -r "matlab_test_script('-release')"

matlab-internal-test: compiler
	@echo "Executing the MATLAB internal test framework"
	@matlab -nosplash -nodesktop -r "matlab_test_script('-internal')"

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
clean: mostlyclean solvers_clean codegen_clean mexinterface_clean cinterface_clean share_cuda_clean
	$(RM) src/dll/*.o
	$(RM) src/dll/*.so
	$(RM) $(BUILD_OPTS) datafiles/*build_options.sml
	$(RM) $(SPEC)
	$(RMDIR) $(BUILD)_*
	$(RM) $(BUILD)
	$(RMDIR) *.dso
	$(RM) $(SPEC)
#	$(RMDIR) ../$(RPM_TEMP_DIR)
#	$(RM) ../$(RPM_TAR_GZ)
	$(RM) install_*.m
#	$(RM) install_*.p

.PHONY: mostlyclean
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


.PHONY: solvers_clean
solvers_clean:
	$(MAKE) -C solvers/cvode clean

.PHONY: codegen_clean
codegen_clean:
	$(MAKE) -C codegen clean

.PHONY: mexinterface_clean
mexinterface_clean:
	$(MAKE) -C interfaces/matlab clean

.PHONY: cinterface_clean
cinterface_clean:
	$(MAKE) -C interfaces/c clean

.PHONY: share_cuda_clean
share_cuda_clean:
	$(MAKE) -C share/cuda clean

.PHONY: clean-all
clean-all:
	$(MAKE) clean
	$(RM) local-install local-install-64
	$(RM) install_*.p


