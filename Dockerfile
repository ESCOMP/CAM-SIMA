# parts of CAM require x86 architecture (gptl, which relies on the rdtsc x86 assembly instruction)
FROM --platform=linux/amd64 fedora:latest

###################################################
## Install necessary packages
###################################################
RUN dnf -y update \
    && dnf -y install \
      blas-devel \
      cmake \
      ftp \
      gcc-c++ \
      gcc-gfortran \
      git \
      hostname \
      lapack-devel \ 
      m4 \
      make \
      netcdf-devel \
      netcdf-fortran-devel \
      openmpi \
      openmpi-devel \
      python \
      sudo \
      svn \
      vim \
      wget \
    && dnf clean all

###################################################
## Make sure the mpi compilers can be found
###################################################
ENV PATH="${PATH}:/usr/lib64/openmpi/bin/"
ENV OMP_NUM_THREADS=5

###################################################
## Build and install ESMF
###################################################

ENV ESMF_TAG="8.4.2"

# let's do the stuff that cmake would do for us
ENV ESMF_DIR=/esmf-${ESMF_TAG}
ENV ESMF_COMM=mpich3
ENV ESMF_BOPT="g"
ENV ESMF_NETCDF="nc-config"
ENV ESMF_NETCDFF_INCLUDE=/usr/lib64/gfortran/modules
ENV ESMF_INSTALL_PREFIX=/usr/local
ENV ESMF_INSTALL_BINDIR=${ESMF_INSTALL_PREFIX}/bin
ENV ESMF_INSTALL_DOCDIR=${ESMF_INSTALL_PREFIX}/doc
ENV ESMF_INSTALL_HEADERDIR=${ESMF_INSTALL_PREFIX}/include
ENV ESMF_INSTALL_LIBDIR=${ESMF_INSTALL_PREFIX}/lib
ENV ESMF_INSTALL_MODDIR=${ESMF_INSTALL_PREFIX}/mod
ENV ESMF_TESTEXHAUSTIVE="OFF"

RUN wget -q https://github.com/esmf-org/esmf/archive/refs/tags/v${ESMF_TAG}.tar.gz && \
    tar zxf v${ESMF_TAG}.tar.gz && \
    cd esmf-${ESMF_TAG} && \
    # This command lets you see what esmf thinks its build options are but may not necessary to build, not sure
    make info && \
    make -j 8 && \
    make install

# verifying everything, always fails because esmf fails to link the mpi library...hurray poor build systems
# RUN make check && make all_tests

###################################################
## Build and install Parallel-netcdf
###################################################
RUN wget -q https://parallel-netcdf.github.io/Release/pnetcdf-1.12.3.tar.gz
RUN tar zxf pnetcdf-1.12.3.tar.gz
RUN cd pnetcdf-1.12.3 && \
     ./configure --prefix=/usr/local && \
     make -j 8 install && \
     ldconfig

###################################################
## Build CAM-SIMA
###################################################

# create a user to run the case
RUN adduser cam_sima_user \
    && echo "cam_sima_user ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/cam_sima_user \
    && chmod 0440 /etc/sudoers.d/cam_sima_user

# copy in the CAM-SIMA code and give the proper user permissions
COPY --chown=cam_sima_user . /home/cam_sima_user/CAM-SIMA

USER cam_sima_user
WORKDIR /home/cam_sima_user/CAM-SIMA

# pull the dependencies
RUN ./manage_externals/checkout_externals
# Copy in the machine information for the container
RUN cp /home/cam_sima_user/CAM-SIMA/docker_camden_config_files/config_machines.xml /home/cam_sima_user/CAM-SIMA/ccs_config/machines/

# Set environment variables needed to create and build the case
ENV USER=$(whoami)
ENV CASE_NAME=/home/cam_sima_user/case_name
ENV CESMDATAROOT=/home/cam_sima_user/cesm_data
ENV CIME_MACHINE=container
ENV CIME_MODEL=cesm
ENV ESMFMKFILE=/usr/local/lib/esmf.mk

# Create a case
RUN ./cime/scripts/create_newcase --case $CASE_NAME  --compset FKESSLER --res ne5_ne5_mg37 --run-unsupported

WORKDIR $CASE_NAME 

RUN ./case.setup
RUN ./case.build