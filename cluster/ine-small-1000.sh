#!/bin/bash
#
#SBATCH --job-name=ine-small-1000
#SBATCH --partition=day
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=12G
#SBATCH --time=1-00:00:00
#SBATCH --error=./%j.err
#SBATCH --output=./%j.out

module load R/4.2.0-foss-2020b

Rscript inesims.R 1e3 1 1000
Rscript inesims.R 1e3 2 1000
Rscript inesims.R 1e3 3 1000

Rscript inesims.R 3162 1 1000
Rscript inesims.R 3162 2 1000
Rscript inesims.R 3162 3 1000

Rscript inesims.R 1e4 1 1000
Rscript inesims.R 1e4 2 1000
Rscript inesims.R 1e4 3 1000

Rscript inesims.R 31623 1 1000
Rscript inesims.R 31623 2 1000
Rscript inesims.R 31623 3 1000
