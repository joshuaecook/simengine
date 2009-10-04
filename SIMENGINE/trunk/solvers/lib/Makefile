include ../../include/common.make

DATATYPES = float double

CVODE_PATH = ../thirdparty/cvode
SUNDIALS = sundials-2.4.0
CVODE_SOURCES = $(SUNDIALS)/src
CVODE_OBJECTS = $(CVODE_SOURCES)/cvode/.libs/cvode_dense.o \
	$(CVODE_SOURCES)/cvode/.libs/cvode_direct.o \
	$(CVODE_SOURCES)/cvode/.libs/cvode_io.o \
	$(CVODE_SOURCES)/cvode/.libs/cvode.o \
	$(CVODE_SOURCES)/cvode/.libs/cvode_diag.o \
	$(CVODE_SOURCES)/cvode/.libs/cvode_band.o \
	$(CVODE_SOURCES)/nvec_ser/.libs/nvector_serial.o \
	$(CVODE_SOURCES)/sundials/.libs/sundials_dense.o \
	$(CVODE_SOURCES)/sundials/.libs/sundials_band.o \
	$(CVODE_SOURCES)/sundials/.libs/sundials_direct.o \
	$(CVODE_SOURCES)/sundials/.libs/sundials_math.o \
	$(CVODE_SOURCES)/sundials/.libs/sundials_nvector.o 

CVODE_LIBS = $(addsuffix .a, $(addprefix libcvode_,$(DATATYPES)))

# ---

.PHONY: all
all: libsolvers.a $(CVODE_LIBS)

libsolvers.a: ../target_*/*.o
	@$(AR) $@ $^

libcvode_%.a: $(addprefix $(CVODE_PATH)/%/,$(CVODE_OBJECTS))
	@$(AR) $@ $^

.PHONY: clean
clean:
	@-$(RM) libsolvers.a $(CVODE_LIBS)