#!/bin/bash -v

#SBATCH --time=20:00:00   # walltime
#SBATCH --ntasks=6
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=80G   # memory per CPU core
#SBATCH -J "ASIM_no_rh"   # job name
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

if [[ -f ~/.bash_profile ]]; then
	printf "\nFound ~/.bash_profile. Will use this and not look for ~/.bashrc.\n"
	source ~/.bash_profile
elif [[ -f ~/.bashrc ]]; then
	printf "\nDid not find ~/.bash_profile. Will use ~/.bashrc.\n"
	source ~/.bashrc
else
	printf "\nERROR: No .bash_profile or .bashrc found in home directory (~/). \
		Conda will not work.\nRun 'conda init' to add the required entry to \
		~/.bash_profile, then re-run.\n"
	exit 1
fi

printf "Config path: configs_activitysim/full_no_RH\n"
printf "Data path: data_activitysim\n"
printf "Output path: output_activitysim/full_no_RH\n"

[[ ! -d "output_activitysim/full_no_RH" ]] \
	&& mkdir -p output_activitysim/full_no_RH

conda activate ASIM_DEV
conda info

activitysim run --config configs_activitysim/full_no_RH \
	--data data_activitysim --output output_activitysim/full_no_RH \
	|| exit 1
#python -m simulation --config "$1" --data "$2" --output "$3" -m || exit 1
