# This Dockerfile is designed for running CAM-SIMA fortran unit tests and integration tests.
FROM ubuntu:22.04

ARG BUILD_TYPE=Debug

RUN apt update \
    && apt install -y sudo \
    && useradd -m test_user \
    && echo "test_user ALL=(root) NOPASSWD: ALL" >> /etc/sudoers.d/test_user \
    && chmod 0440 /etc/sudoers.d/test_user

USER test_user
WORKDIR /home/test_user

RUN sudo apt update \
    && sudo apt -y install \
    cmake \
    cmake-curses-gui \
    gfortran \
    git \
    libopenmpi-dev \
    make \
    openmpi-bin \
    valgrind \
    vim \
    && sudo apt clean

ENV FC=mpif90
ENV FFLAGS="-I/usr/include/"

COPY . cam_sima
RUN sudo chown -R test_user:test_user cam_sima

RUN cd cam_sima/test \
    && cmake -S . -B build \
             -D CMAKE_BUILD_TYPE=${BUILD_TYPE} \
             -D CCPP_ENABLE_MEMCHECK=ON \
    && cmake --build ./build

WORKDIR /home/test_user/cam_sima/test/build