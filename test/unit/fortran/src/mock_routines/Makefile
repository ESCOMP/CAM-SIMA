# Makefile for building test_PIO.exe
# Usage:
#   make           # Build test_PIO.exe
#   make clean     # Remove build artifacts

# User configuration
PIO_SRC_PATH ?= /glade/work/nusbaume/SE_projects/PIO/ParallelIO
PIO_CMAKE_PATH ?= /glade/work/nusbaume/SE_projects/PIO/build
NETCDF_PATH ?= /glade/u/apps/derecho/23.09/spack/opt/spack/netcdf/4.9.2/gcc/13.2.0/zywl

FC = gfortran
FFLAGS = -g -O0
INCLUDES = -I$(PIO_CMAKE_PATH) \
           -I$(PIO_SRC_PATH)/src/flib \
           -I$(PIO_CMAKE_PATH)/src/flib \
           -I$(NETCDF_PATH)/include \
           -I$(PIO_SRC_PATH)/src/clib \
           -I$(PIO_SRC_PATH)/src/ncint \
           -I$(PIO_SRC_PATH)/src/gptl \
           -I$(PIO_CMAKE_PATH)/src/gptl

LIBS = $(PIO_CMAKE_PATH)/src/flib/libpiof.a \
       $(PIO_CMAKE_PATH)/src/clib/libpioc.a \
       $(NETCDF_PATH)/lib/libnetcdf.so \
       $(PIO_CMAKE_PATH)/src/gptl/libgptl.a \
       $(NETCDF_PATH)/lib/libnetcdff.so \
       -lmvec

# Targets
.PHONY: all clean

all: test_PIO.exe
	@echo 'Build has completed.'

test_PIO.exe: test_PIO.o cam_pio_utils.o
	$(FC) $(FFLAGS) $^ -o $@ $(LIBS)

test_PIO.o: test_PIO.F90 cam_pio_utils.o
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

cam_pio_utils.o: cam_pio_utils.F90
	$(FC) $(FFLAGS) $(INCLUDES) -c $< -o $@

clean:
	@echo 'Cleaning up directory...'
	rm -f *.o *.mod *.exe

show-paths:
	@echo "PIO source path is: $(PIO_SRC_PATH)"
	@echo "PIO build path is: $(PIO_CMAKE_PATH)"
