#!/bin/bash -v

module load r gcc udunits spack/release gdal geos miniconda3 python/3.8

#NOTE: All the instances of `./populationSim.R` essentially just run `tar_make()`. Sourcing some of these scripts from within the targets file has proven to be difficult, so we just run the commands manually. We need to re-run `tar_make()` between each of these steps, hence the repitition of `./populationSim.R`.

#tar_make for popsim
./R/populationSim.R

#run popsim
[ ! -d "output_popsim" ] && mkdir output_popsim
conda activate popsim
python py/runpopsim.py --config configs_popsim --data data_popsim --output output_popsim

#tar_make for skims
./R/buildSkims.R

#build skims
[ ! -d "data_activitysim" ] && mkdir data_activitysim
conda activate ASIM_DEV
python py/build_omx.py inputs/skims data_activitysim

#tar_make final
./R/runAsim.R

#make output directory for activitysim
[ ! -d "output_activitysim" ] && mkdir output_activitysim

