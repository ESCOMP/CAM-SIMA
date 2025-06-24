---
project: MPAS Dynamical Core
summary: MPAS dynamical core solves the fully compressible non-hydrostatic equations of atmospheric motion.
author: CAM-SIMA & MPAS Developers
github: https://github.com/ESCOMP/CAM-SIMA
website: https://sima.ucar.edu
src_dir: ..
exclude_dir: ../assets
             ../dycore
output_dir: ./ford
extensions: F90
            f90
fixed_extensions: F
                  f
fixed_length_limit: false
fpp_extensions: F
                F90
preprocess: true
preprocessor: gfortran -E
display: private
         protected
         public
graph: true
lower: true
proc_internals: true
search: false
sort: alpha
source: true
---

Model for Prediction Across Scales (MPAS) is one of the supported atmospheric dynamical cores in Community Atmosphere Model - System for Integrated Modeling of the Atmosphere (CAM-SIMA). CAM-SIMA is the next-generation atmospheric component of Community Earth System Model (CESM).

This auto-generated documentation provides a developer's guide to the Fortran interface and subdriver of MPAS dynamical core.
