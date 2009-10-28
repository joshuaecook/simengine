include ../../include/common.make

LDLIBS = $(CUDA_LDLIBS) -lcudart
ifneq ($(ARCH64),)
LDFLAGS = $(CUDA_LDFLAGS) -L/opt64/cuda/lib
else
LDFLAGS = $(CUDA_LDFLAGS) -L/opt/cuda/lib
endif
INCLUDES = $(CUDA_INCLUDES) -I/opt/cuda/include
CPPFLAGS = $(INCLUDES) -g 

bandwidth_test.o: override CXX = $(NVCC)
bandwidth_test.o: override CXXFLAGS = $(NVCCFLAGS) -Xcompiler -ggdb --ptxas-options=-v

%.o: %.cu
	$(COMPILE.cpp) -o $@ $<

.PHONY: all clean
all: device_props
clean:
	$(RM) *.o *.linkinfo
	$(RM) device_props bandwidth_test

