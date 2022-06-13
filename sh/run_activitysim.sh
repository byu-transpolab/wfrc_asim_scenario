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

printf "Config path: $1\n"
printf "Data path: $2\n"
printf "Output path: $3\n"

conda activate ASIM_DEV
conda info

activitysim run --config "$1" --data "$2" --output "$3" || exit 1
