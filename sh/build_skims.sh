#!/bin/bash -v

if [[ -f ~/.bash_profile ]]; then
	echo "\nFound ~/.bash_profile. Will use this and not look for ~/.bashrc.\n"
	source ~/.bash_profile
elif [[ -f ~/.bashrc ]]; then
	echo "\nDid not find ~/.bash_profile. Will use ~/.bashrc.\n"
	source ~/.bashrc
else
	echo "\nERROR: No .bash_profile or .bashrc found in home directory (~/). Conda will not work.\n
	Run 'conda init' to add the required entry to ~/.bash_profile, then re-run.\n"
	exit 1
fi

echo "Manifest path: $1"
echo "Data path: $2"

conda activate ASIM_DEV
conda info

python py/build_omx.py "$1" "$2" || exit 1
