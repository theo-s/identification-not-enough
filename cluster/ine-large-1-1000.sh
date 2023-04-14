#!/bin/bash
#
#SBATCH --job-name=ine-large-1-1000
#SBATCH --partition=day
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=12G
#SBATCH --time=1-00:00:00
#SBATCH --error=./%j.err
#SBATCH --output=./%j.out

module load R/4.2.0-foss-2020b

Rscript inesims.R 1e5 1 1000
