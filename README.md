# Replication Code: Nonparametric identification is not enough, but randomized controlled trials are

This repo contains the code to run several simulations from the paper *Nonparametric identification is not enough, but randomized controlled trials are*.
[https://arxiv.org/abs/2108.11342](https://arxiv.org/abs/2108.11342).

## Low Dimensional Simulations

The code for running a single run of the low dimensional simulations can be found in `code/run_simul.R`. 
In order to run this script with a single seed, one should run the R script as:
```
Rscript code/run_simul.R --s 99
```

In order to submit an array of jobs to a cluster using SLURM, using the list of seeds in `jobs/params.txt`,
one should use the script:

```
sbatch jobs/submit_sims.sh
```
Note that by default this runs 500 Monte Carlo replications so may have a fairly long run time.
In order to create the plots after running the simulation, one should run (the resulting plots will be saved
in `code/figures`/):

```
Rscript code/postprocess_simul.R
Rscript code/plot_simul.R
```
The resulting figures will then be saved in `figures`.

## High Dimensional Simulations

The setup for running the high dimensional simulations is very similar.
One difference is that the coefficients for the DGPs will need to be generated before
the simulations can be ran. 
In order to do this, one needs to run:
```
Rscript code/generate_coefs.R
```

The script for running a single run of the high dimensional simulations can be found in `code/run_simul_high.R`. 
In order to run this script with a single seed, one should run the R script as:
```
Rscript code/run_simul_high.R --s 99
```

In order to submit an array of jobs to the cluster, using the list of seeds in `jobs/params.txt`,
one should use the script:

```
sbatch jobs/submit_sims_high.sh
```

Note that by default this runs 500 Monte Carlo replications so may have a fairly long run time.
In order to create the plots after running the simulation, one should run (the resulting plots will be saved
in `code/figures`/):

```
Rscript code/postprocess_high.R
Rscript code/plot_simul_high.R
```
