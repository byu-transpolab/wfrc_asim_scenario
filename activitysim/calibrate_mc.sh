#!/bin/bash -l

#SBATCH --time=4:00:00   # walltime
#SBATCH --ntasks=12   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=30720M   # memory per CPU core
#SBATCH -J "calibrate_mc"   # job name
#SBATCH --mail-user=satchley@byu.edu   # email address
# #SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL


# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# LOAD MODULES, INSERT CODE, AND RUN YOUR PROGRAMS HERE

mamba activate asim
mamba info
#mamba list

activitysim run -c configs_mc_calibration -c configs_mp -c configs -d data/base_2019 -o output/calibrate_mc_6
