#!/bin/bash -v

#SBATCH --time=20:00:00   # walltime
#SBATCH --ntasks=8
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=80G   # memory per CPU core
#SBATCH -J "ASIM_1pct_no_RH"   # job name
#SBATCH --mail-user=shaydenatch@gmail.com   # email address
##SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE
SCNAME="base"

module purge
module load miniconda3
echo "Loaded Modules"
module list

if [[ -f ~/.bashrc ]]; then
	printf "\nFound ~/.bashrc. Will use this and not look for ~/.bash_profile.\n"
	source ~/.bashrc
elif [[ -f ~/.bash_profile ]]; then
	printf "\nDid not find ~/.bashrc. Will use ~/.bash_profile.\n"
	source ~/.bash_profile
else
	printf "\nERROR: No .bashrc or .bash_profile found in home directory (~/). \
		Conda will not work.\nRun 'conda init' to add the required entry to \
		~/.bashrc, then re-run.\n"
	exit 1
fi

printf "Config path: configs_activitysim/1pct_no_RH\n"
printf "Data path: data_activitysim\n"
printf "Output path: output_activitysim/1pct_no_RH\n"

[[ ! -d "output_activitysim/1pct_no_RH" ]] \
	&& mkdir -p output_activitysim/1pct_no_RH

conda activate ASIM_DEV
conda info

#activitysim run --config configs_activitysim/1pct_no_RH \
#	--data data_activitysim --output output_activitysim/1pct_no_RH \
#	|| exit 1
python -m simulation --config configs_activitysim/1pct_no_RH \
	--data data_activitysim --output output_activitysim/1pct_no_RH \
	|| exit 1
