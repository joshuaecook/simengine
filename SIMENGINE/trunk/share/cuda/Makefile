include ../../include/common.make

device_props: override LDLIBS = $(CUDA_LDLIBS)
device_props: override LDFLAGS = $(CUDA_LDFLAGS)
device_props: override INCLUDES = $(CUDA_INCLUDES)
device_props: override CPPFLAGS = $(INCLUDES)

.PHONY: all clean
all: device_props
clean:
	$(RM) device_props

