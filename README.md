# CAM-SIMA
Community Atmosphere Model - System for Integrated Modeling of the Atmosphere

NOTE:  Only developmental code exists at the moment.  This README will be updated once production code becomes available.

## How to checkout and use CAM-SIMA:

The instructions below assume you have cloned this repository and are in the repository directory. For example:
```
git clone https://github.com/ESCOMP/CAM-SIMA.git
cd CAM-SIMA
```

### To use unsupported CAM-SIMA **development** code:

## NOTE: This is **unsupported** development code and is subject to the [CESM developer's agreement](http://www.cgd.ucar.edu/cseg/development-code.html).
```
git checkout development
./manage_externals/checkout_externals
```

Good luck, and have a great day!

## Build the docker file
1. First, build the esmf docker image. You must tag the build with `esmf`
```
cd docker
docker build -f Dockerfile.esmf -t esmf .
```
2. Build the CAM-SIMA iamge
```
cd ../
docker build -t cam-sima .
```
3. Run the image
```
docker run --rm -it cam-sima
```
4. Submit the case
```
./case.submit
```