#!/bin/bash
#
#SBATCH --job-name=ine-med-10000
#SBATCH --partition=day
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=12G
#SBATCH --time=1-00:00:00
#SBATCH --error=./%j.err
#SBATCH --output=./%j.out

module load R/4.2.0-foss-2020b

Rscript inesims.R 31623 1 10000
Rscript inesims.R 31623 2 10000
Rscript inesims.R 31623 3 10000
