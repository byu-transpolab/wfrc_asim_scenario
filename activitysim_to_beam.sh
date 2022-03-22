#!/bin/bash -v

module load r gcc udunits spack/release gdal geos miniconda3 python/3.8

#make output directory for activitysim conversion
[ ! -d "data_beam" ] && mkdir data_beam

./R/activitysimToBeam.R
