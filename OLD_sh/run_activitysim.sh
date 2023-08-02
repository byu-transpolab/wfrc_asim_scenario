# Run activitysim

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

printf "Config path: $1\n"
printf "Data path: $2\n"
printf "Output path: $3\n"

conda activate ASIM_DEV
conda info

#activitysim run --config "$1" --data "$2" --output "$3" || exit 1
python -m simulation --config "$1" --data "$2" --output "$3" -m || exit 1
