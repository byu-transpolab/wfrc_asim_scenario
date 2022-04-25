#!/bin/bash

module load miniconda3 python/3.8

#init shells, maybe unnecessary
conda init --all

#set up .bashrc and .bash_profile
cat conda_setup/bashrc > ~/.bashrc
cat conda_setup/bash_profile > ~/.bash_profile

#remove old environments
conda env remove -n ASIM_DEV
conda env remove -n popsim

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
