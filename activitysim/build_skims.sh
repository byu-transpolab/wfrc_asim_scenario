#!/bin/bash -l

printf "Skims input directory: $1\n"
printf "Skims output directory: $2\n\n"

echo "Make sure conda is set up with the correct environments \n
(popsim and asim)"

conda activate asim

python activitysim/build_omx.py "$1" "$2" || exit 1