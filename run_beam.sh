#!/bin/bash

#SBATCH --time=20:00:00   # walltime
#SBATCH --ntasks=8
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=70G   # memory per CPU core
#SBATCH -J "beam"   # job name
#SBATCH --mail-user=example@your_email.com   # email address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE
SCNAME="base"

module load java/1.8
echo "Loaded Modules"
module list

java -jar BEAM.jar --config configs_beam/full/full.conf
