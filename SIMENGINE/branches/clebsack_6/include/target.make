# The magical multi-target Makefile
# See http://make.paulandlesley.org/multi-arch.html

# Eliminates all built-in rules
.SUFFIXES:

# A command for recursively invoking make to build a target
MAKE_TARGET = $(MAKE) --no-print-directory -C $@ \
		-f $(realpath $(firstword $(MAKEFILE_LIST))) \
		SOURCES=$(CURDIR)/src $(MAKECMDGOALS)

# Creates and relocates to a target build directory.
.PHONY: $(TARGETS)
$(TARGETS):
	@+[ -d $@ ] || $(MKDIR) $@
	@+$(MAKE_TARGET)

# Don't attempt to remake makefiles
Makefile: ;
%.make:: ;

# Matches anything, causing the target build directories to be created
# and make to be recursively reinvoked within each target directory.
% :: $(MAKE_TARGETS_PREREQS) $(TARGETS)
	@+$(MAKE_TARGET_CALLBACK)

.PHONY: clean
clean:
	@-$(RM) $(TARGETS)
	@-$(MAKE_TARGET_CLEAN_CALLBACK)