#!/bin/bash

module load spack/release
module load r/4.1
module load gcc/11

# for tidyverse
module load r-stringi
module load harfbuzz
module load fribidi
module load freetype
module load libpng
module load libtiff
module load libjpeg-turbo

# for sf
module load udunits
module load gdal
module load geos/3.8.1-gcc-9.2.0-haswell-wsc7sc3

module load curl

