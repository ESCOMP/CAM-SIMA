FC = gfortran
FFLAGS = -c -DCPRGNU

SOURCES = shr_kind_mod.F90 shr_infnan_mod.F90 ccpp_kinds.F90 cam_abortutils.F90
SOURCES += spmd_utils.F90 cam_logfile.F90
OBJS = $(SOURCES:.F90=.o)

all: objs

objs: $(SOURCES)
	$(FC) $(FFLAGS) $(SOURCES)

clean:
	${RM} *.o *.mod
