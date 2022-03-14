#!/bin/bash -x

module load r gcc udunits spack/release gdal geos

./populationSim.R

module load miniconda3

conda create -y -n popsim python=3.8
conda activate popsim

cd ..

python py/runpopsim.py --config configs_popsim --data data_popsim --output output_popsim
