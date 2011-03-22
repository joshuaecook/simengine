$(info S   I   M   A   T   R   A)
$(info simEngine Compiler Makefile)
$(info Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.)
ifeq ($(filter help,$(MAKECMDGOALS)),)
$(info Invoke "make help" for additional options)
endif
$(info )

define HELP_TEXT
Targets
==============================================================================
help		Displays this message.
all		Compiles the application and all its resources.
install		Installs the compiled application and its resources.
installer	Creates a tarball installer for the complete application.
html		Creates HTML documentation.
test		Tests an installed application.
clean		Removes compiled objects.
clean-all	Removes all generated resources.

show-targets	Lists all defined targets.
show-html	Lists all HTML documentation.

Variables
==============================================================================
debug		If non-empty, compile with extra debugging information.
profile		If non-empty, compile with extra profiling information.
verbose		If non-empty, print extra information when compiling.

install-html	If non-empty, create HTML documentation with the installer.
test-suite	Specifies which test suites to run; may contain "release" or 
		"internal" (default.)

endef

# Makefile practices
# ============================================================================
# Avoid invoking make recursively.
#
# Exploit builtin rules and automatic variables.
#
# Define target-specific variables.
#
# Quote filenames to prevent problems with spaces and special characters.
#
# Don't write lengthy shell scripts or complex conditional logic in recipes.
# Move these commands to an external shell script instead.
#
# Produce objects within the same path as their sources.
#
# Include dependency files for targets with many dependencies. Generate the
# dependency file automatically if possible.
#
# Use the functions "info," "warning," and "error," not the "echo"
# statement for user interface messages.
#
# Avoid using the @ symbol to quiet recipes. Invoke "make -s" for
# quiet builds.

.SUFFIXES:
.SUFFIXES: .c .h .o .a .m .sml .sig .fun .mlb .sew .json .dol .mexglx .mexa64 .mexmaci .mexmaci64 .mexs64 .mexw32 .mexw64 .mex .lex .grm .desc

.PHONY: all
all: targets


.PHONY: help
help:
	$(info $(HELP_TEXT))

# Define top-level variables.
MAJOR_VERSION := 1
MINOR_VERSION := 2
VERSION_REVISION := b
include include/common.make

define BUILD_OPTIONS_JSON
{\
\"allowSWBackend\": true, \"allowFPBackend\": false, \"allowHWBackend\": false,\
\"build\": \"$(VERSION)\",\
\"buildBranch\": \"$(SVN_BRANCH)\",\
\"buildDate\": \"$(TIME)\",\
\"buildRevision\": $(SVN_REVISION),\
\"buildTime\": $(TIME),\
\"devVersion\": $(if $(SVN_IS_BRANCH),true,false),\
\"version\": \"$(VERSION_NUMBER)\",\
\"majorVersion\": $(MAJOR_VERSION),\
\"minorVersion\": $(MINOR_VERSION),\
\"versionRevision\": \"$(VERSION_REVISION)\",\
\"architecture\": \"$(MACHINE)\"\
}
endef

# Crypto is not necessary when simEngine is an unlicensed product
#CRYPTO_LIB := $(if $(DARWIN),/opt/local/lib/libcrypto.a /opt/local/lib/libz.a,$(if $(ARCH64),/usr/lib64/libcrypto.a,/usr/lib/libcrypto.a))
UPDATE_QUERY := build=$(if $(SVN_IS_BRANCH),preview,$(if $(SVN_IS_TRUNK),development,release))

# Insired by http://www.xs4all.nl/~evbergen/nonrecursive-make.html

# This system of Makefiles uses a directory "stack" to isolate
# subsystems without the need for recursion. Within each subsystem, a
# special variable named D contains the current directory
# path. However, all recipes are executed from the top level
# directory. Subsystems may use the value of D in target names, lists
# of requirements, and variables bound with :=, but not in the
# recipes.

# Each subsystem is defined by a file "Makefile.defs" containing a
# Makefile fragment. 

# Subsystems may augment the following definitions.

TARGETS :=
INSTALLS :=
DEPENDS :=
CLEAN :=
CLEAN_ALL :=

# Import subsystem definitions.

$(eval $(call IMPORT_SUBSYSTEM,.))

# Define top-level rules.

.PHONY: targets
targets: $(TARGETS)

.PHONY: show-targets
show-targets:
	@(export TARGETS="$(TARGETS)"; echo $$TARGETS | perl -pe 's/ /\n/g')

.PHONY: install $(INSTALLS)
install: $(TARGETS) $(INSTALLS)
	$(info Installed in $(INSTALL_PREFIX))

.PHONY: installer
installer: INSTALL_PREFIX := Simatra
installer: install html $(INSTALL_FILENAME) # always produce documentation with the installer

installer/simEngine_%.tgz:
	$(info Creating installer tgz file as $@)
	tar chzf $@ $(INSTALL_PREFIX)

.PHONY: show-html
show-html:
	@(export TARGETS="$(HTML_DOCUMENTS)"; echo $$TARGETS | perl -pe 's/ /\n/g')

.PHONY: html
html: $(HTML_DOCUMENTS) $(HTML_INSTALLS)

.PHONY: test
test: TEST_SUITE := $(if $(TEST_RELEASE),-release,-internal)
test: install
	$(MATLAB) -nosplash -nodesktop -r "matlab_test_script('$(TEST_SUITE)'); quit"

.SECONDARY: $(CLEAN)
.PHONY: clean
clean:
	rm -f $(CLEAN)

.SECONDARY: $(CLEAN_ALL)
.PHONY: clean-all
clean-all: clean
	rm -f $(CLEAN_ALL)

# Include dependency rules but don't die if they haven't been generated yet.
# Don't include dependency files when cleaning to prevent needless generation.
ifeq ("$(strip $(filter clean%,$(MAKECMDGOALS)))","")
-include $(DEPENDS)
endif
