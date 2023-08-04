# parts of CAM require x86 architecture (gptl, which relies on the rdtsc x86 assembly instruction)
# esmf is am image you are expected to have built. Read the README file for instructions
FROM esmf:latest

###################################################
## Install necessary packages
###################################################
RUN dnf -y update \
    && dnf -y install \
      ftp \
      git \
      hostname \
      m4 \
      python \
      sudo \
      svn \
      tree \
      vim \
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

ENV FC=gfortran

###################################################
## Build and install json-fortran
###################################################
RUN curl -LO https://github.com/jacobwilliams/json-fortran/archive/8.2.0.tar.gz \
    && tar -zxvf 8.2.0.tar.gz \
    && cd json-fortran-8.2.0 \
    && mkdir build \
    && cd build \
    && cmake -D SKIP_DOC_GEN:BOOL=TRUE .. \
    && make install -j 8

# add a symlink
RUN ln -s /usr/local/jsonfortran-gnu-8.2.0/lib/libjsonfortran.a /usr/local/lib/libjsonfortran.a

###################################################
## Build and install MUSICA
###################################################

RUN git clone https://github.com/NCAR/musica.git
RUN mkdir /musica/build \
    && cd /musica/build \
    && export JSON_FORTRAN_HOME="/usr/local/jsonfortran-gnu-8.2.0" \
    && cmake \
             -D ENABLE_TESTS=OFF \
             -D ENABLE_TUVX=OFF \
          .. \
    && make install -j 8

# add a symlink
RUN ln -s /usr/local/musica-0.3.0/lib64/libmusica.a /usr/local/lib/libmusica.a

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

RUN ./xmlchange CAM_CONFIG_OPTS="--dyn none --physics-suites musica"
RUN ./xmlchange ROF_NCPL=48
RUN ./xmlchange STOP_OPTION=nsteps
RUN ./xmlchange STOP_N=5

# Copy in the grid files and a snapshot file
RUN chmod +x /home/cam_sima_user/CAM-SIMA/docker/ftp_download.sh
RUN /home/cam_sima_user/CAM-SIMA/docker/ftp_download.sh

# # add the snapshot file
RUN echo "ncdata='/home/cam_sima_user/run_heldsuarez_cam6_nt2_bigg_try005.cam.h5.0001-01-01-00000.nc'" >> user_nl_cam

RUN ./case.build
