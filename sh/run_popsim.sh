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

echo "Data path: $1"
echo "Output path: $2"

conda activate popsim
conda info

python py/runpopsim.py --config configs_popsim --data "$1" --output "$2" || exit 1
