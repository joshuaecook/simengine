OSLOWER=$(shell uname -s|tr [:upper:] [:lower:])

# define a special architecture if ARCH64 is defined
ifneq ($(ARCH64),)
ifeq ($(OSLOWER),darwin)
CFLAG64 = -arch x86_64
else
CFLAG64 = -m64
endif
endif

all: simex

simex: simex.c
	gcc -g -I../../codegen/src/simengine -o $@ $< -ldl $(CFLAG64)

clean:
	rm -rf simex simex.dSYM
