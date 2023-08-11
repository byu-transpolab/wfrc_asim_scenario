# Build skims

if [[ -f ~/.bashrc ]]; then
	printf "\nFound ~/.bashrc. Will use this and not look for ~/.bash_profile.\n"
	source ~/.bash_profile
elif [[ -f ~/.bash_profile ]]; then
	printf "\nDid not find ~/.bashrc. Will use ~/.bash_profile.\n"
	source ~/.bashrc
else
	printf "\nERROR: No .bash_profile or .bashrc found in home directory (~/). Conda will not work.\n
	Run 'conda init bash' to add the required entry to ~/.bashrc and/or ~/.bash_profile, then re-run.\n"
	exit 1
fi

printf "Data path: $1\n"
printf "Skims omx output path: $2\n"

conda activate ASIM_DEV
conda info

python py/build_omx.py "$1" "$2" || exit 1