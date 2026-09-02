# CAM-SIMA
Community Atmosphere Model - System for Integrated Modeling of the Atmosphere

Official developer documentation for CAM-SIMA can be [found here](https://escomp.github.io/CAM-SIMA-docs/).

NOTE:  Only developmental code exists at the moment.  This README will be updated once production code becomes available.

## Current code status:

[![Python Unit Tests](https://github.com/ESCOMP/CAM-SIMA/actions/workflows/python_unit_tests.yml/badge.svg)](https://github.com/ESCOMP/CAM-SIMA/actions/workflows/python_unit_tests.yml)
[![CAM-SIMA Fortran CI](https://github.com/ESCOMP/CAM-SIMA/actions/workflows/cam_sima_fortran_ci.yml/badge.svg?branch=development)](https://github.com/ESCOMP/CAM-SIMA/actions/workflows/cam_sima_fortran_ci.yml)

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
bin/git-fleximod update
```

Good luck, and have a great day!

## Acknowledgements

The refactoring of the CAM4 and CAM5 physics routines into the CAM-SIMA codebase was supposed by [NSF award 2311276](https://www.nsf.gov/awardsearch/show-award/?AWD_ID=2311376).
