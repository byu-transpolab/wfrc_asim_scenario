#!/bin/bash

module load miniconda3 python/3.8

#install requirements
pip install -r reference/python_requirements.txt

#create activitysim conda environment
#conda env create --file=activitysim/conda-environments/activitysim-dev.yml --name ASIM_DEV
conda create -y -n ASIM_DEV python=3.9
conda activate ASIM_DEV
conda install -y pytables
pip install -r reference/asim_requirements.txt

#create populationsim conda environment
conda create -y -n popsim python=3.8
conda activate popsim

conda install -y pytables
pip install scipy
pip install populationsim

conda activate base