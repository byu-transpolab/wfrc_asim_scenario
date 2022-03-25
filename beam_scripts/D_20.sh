#!/bin/bash

#SBATCH --time=20:00:00   # walltime
#SBATCH --ntasks=16
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=30G   # memory per CPU core
#SBATCH -J "beam"   # job name
#SBATCH --mail-user=shaydenatch@gmail.com   # email address
##SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE
SCNAME="base"

module purge
module load jdk/1.8
echo "Loaded Modules"
module list

java -Xmx450G -jar ../BEAM.jar --config ../configs_beam/wfrc_full/D_20.conf
