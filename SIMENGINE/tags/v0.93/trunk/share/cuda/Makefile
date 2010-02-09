include ../../include/common.make

ifeq ($(NVCC),)
$(error No CUDA compiler found)
endif

LDFLAGS = -ldl

ifneq ($(DARWIN),)
TARGET_ARCH = -arch i386
CUDART_LIBRARY_NAME = libcudart.dylib
else
CUDART_LIBRARY_NAME = libcudart.so
endif

INCLUDES = $(CUDA_INCLUDES)
CPPFLAGS = $(INCLUDES) -g -DCUDART_LIBRARY_NAME='"$(CUDART_LIBRARY_NAME)"'

.PHONY: all clean
all: device_props
clean:
	$(RM) *.o *.linkinfo
	$(RM) device_props
