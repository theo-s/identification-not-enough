#!/bin/bash

#SBATCH --job-name=identification_not_enough_sims
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --array=1-500
#SBATCH --time=3-00:00:00
#SBATCH --mail=theo_s@berkeley.edu

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "params.txt")
seed=$(echo $LINE | cut -d ' ' -f 1)

cd ../code/
Rscript run_simul_high.R --s "$seed"
