#!/bin/bash -l

echo "Make sure conda is set up with the correct environments
(popsim and asim)"

conda activate popsim

python run_populationsim.py -c configs -d data/2019 -o output/2019
python run_populationsim.py -c configs -d data/new_landuse -o output/new_landuse

# scenarios=("2019" "new_landuse")
# 
# for scen in ${scenarios[@]}; do
#   python run_populationsim.py -c configs -d data/$scen -o output/$scen
# done