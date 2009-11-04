include ../../include/common.make

LDLIBS = $(CUDA_LDLIBS)
LDFLAGS = $(CUDA_LDFLAGS)
INCLUDES = $(CUDA_INCLUDES)
CPPFLAGS = $(INCLUDES) -g 

bandwidth_test.o: override CXX = $(NVCC)
bandwidth_test.o: override CXXFLAGS = $(NVCCFLAGS) -Xcompiler -ggdb --ptxas-options=-v

%.o: %.cu
	$(COMPILE.cpp) -o $@ $<

.PHONY: all clean
all: device_props bandwidth_test
clean:
	$(RM) *.o *.linkinfo
	$(RM) device_props bandwidth_test

