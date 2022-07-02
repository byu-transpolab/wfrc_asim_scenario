#!/bin/bash

#SBATCH --time=20:00:00   # walltime
#SBATCH --ntasks=8
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=80G   # memory per CPU core
#SBATCH -J "asim_to_beam_conversion"   # job name
#SBATCH --mail-user=shaydenatch@gmail.com   # email address
##SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE
SCNAME="base"

module purge
source BYU_ORC_setup/loda_modules.sh
echo "Loaded Modules"
module list

source R/activitysimToBeam.R
