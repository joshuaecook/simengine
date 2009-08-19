include ../../include/common.make

.PHONY: all
all: libsolvers.a

libsolvers.a: ../target_*/*.o
	@$(AR) $@ $^

.PHONY: clean
clean:
	@-$(RM) libsolvers.a