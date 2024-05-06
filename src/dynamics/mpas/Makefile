# This Makefile is invoked by CIME Makefile (i.e., `cime/CIME/Tools/Makefile`).

# Some targets in MPAS build infrastructure are sensitive to this environment variable. Override it to avoid build issues.
override PWD = $(CURDIR)
export PWD

all:
	$(MAKE) -f Makefile.CESM libmpas-prepare ESM="CESM"
	$(MAKE) -f Makefile.CESM libmpas-build ESM="CESM"
