#!/bin/bash -v

module load r gcc udunits spack/release gdal geos miniconda3 python/3.8

./R/activitysimToBeam.R
