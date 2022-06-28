#!/bin/bash -v

if [[ -f ~/.bash_profile ]]; then
	printf "\nFound ~/.bash_profile. Will use this and not look for ~/.bashrc.\n"
	source ~/.bash_profile
elif [[ -f ~/.bashrc ]]; then
	printf "\nDid not find ~/.bash_profile. Will use ~/.bashrc.\n"
	source ~/.bashrc
else
	printf "\nERROR: No .bash_profile or .bashrc found in home directory (~/). Conda will not work.\n
	Run 'conda init' to add the required entry to ~/.bash_profile, then re-run.\n"
	exit 1
fi

printf "Data path: $1\n"
printf "Output path: $2\n"

conda activate ASIM_DEV
conda info

python py/runpopsim.py --config configs_popsim --data "$1" --output "$2" || exit 1
