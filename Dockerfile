# parts of CAM require x86 architecture (gptl, which relies on the rdtsc x86 assembly instruction)
# esmf is am image you are expected to have built. Read the README file for instructions
FROM esmf:latest

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
      mpich \
      mpich-devel \
      netcdf-devel \
      netcdf-fortran-devel \
      python \
      sudo \
      svn \
      tree \
      vim \
      wget \
    && dnf clean all

###################################################
## Make sure the mpi compilers can be found
###################################################
ENV PATH="${PATH}:/usr/lib64/mpich/bin/"
ENV OMP_NUM_THREADS=5

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
RUN cp /home/cam_sima_user/CAM-SIMA/docker/config_machines.xml /home/cam_sima_user/CAM-SIMA/ccs_config/machines/

# Set environment variables needed to create and build the case
ENV USER=$(whoami)
ENV CASE_NAME=/home/cam_sima_user/case_name
ENV CESMDATAROOT=/home/cam_sima_user/cesm_data
ENV CIME_MACHINE=container
ENV CIME_MODEL=cesm
ENV ESMFMKFILE=/usr/local/lib/esmf.mk

# Create a case
RUN ./cime/scripts/create_newcase --case $CASE_NAME  --compset FPHYStest --res ne5_ne5_mg37 --run-unsupported

WORKDIR $CASE_NAME 

RUN ./case.setup

RUN ./xmlchange CAM_CONFIG_OPTS="--dyn none --physics-suites held_suarez_1994"
RUN ./xmlchange ROF_NCPL=48
RUN ./xmlchange STOP_OPTION=nsteps
RUN ./xmlchange STOP_N=5

# now add the mam3 grid by hand since it's not downloaded automatically for some reason
RUN mkdir -p /home/cam_sima_user/cesm_data/inputdata/atm/cam/inic/homme/
RUN cp /home/cam_sima_user/CAM-SIMA/docker/cami-mam3_0000-01_ne5np4_L30.140707.nc /home/cam_sima_user/cesm_data/inputdata/atm/cam/inic/homme/

# add the snapshot file
RUN echo "ncdata='/home/cam_sima_user/CAM-SIMA/docker/run_heldsuarez_cam6_nt2_bigg_try005.cam.h5.0001-01-01-00000.nc'" >> user_nl_cam

RUN ./case.build
