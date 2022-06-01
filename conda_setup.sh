#!/bin/bash

module load miniconda3 python/3.8

#install requirements
pip install -r reference/python_requirements.txt

#create activitysim conda environment
conda env create --file=../conda-environments/activitysim-dev.yml --name ASIM_DEV
conda activate ASIM_DEV

#create populationsim conda environment
conda create -y -n popsim python=3.8
conda activate popsim

conda install -y pytables
pip install scipy
pip install populationsim

conda activate base