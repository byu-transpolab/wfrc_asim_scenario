#!/bin/bash -l

echo "Make sure conda is set up with the correct environments \n
(popsim and asim)"

conda activate asim

python build_omx.py "BY_2019" "_built/BY_2019.omx"
python build_omx.py "doubletrack" "_built/doubletrack.omx"
