# Nonparametric identification is not enough, but randomized controlled trials are

P. M. Aronow, James M. Robins, Theo Saarinen, Fredrik Sävje, Jasjeet Sekhon

This repo contains the code reproduce the simulations from *Nonparametric identification is not enough, but randomized controlled trials are*.
[https://arxiv.org/abs/2108.11342](https://arxiv.org/abs/2108.11342).

## Simulation Replication

The main script for running a set of simulations is the `inesims.R`. Given three
command line arguments:

- `n`: the number of training data points
- `DGP`: the data generating process to use (must be one of 1: logistic function, 2: sinusoidal function, or 3: adversarial binned DGP)
- `nrounds`: the number of Monte Carlo replications to run

Data will be generated according to the selected DGP and the bias and RMSE results from each estimator are calculated and stored in `res/`.
For example, to get the results for the first DGP with `n=100,000` and `1000` Monte Carlo replications, one should run:
```
Rscript inesims.R 1e5 1 1000
```
The results will then be stored in `res/ine-100000-1-10000-[as.integer(Sys.time())].rds`


## Plotting Results

The `compile.R` script processes the results from `res/` and produces Figure 3 from the paper.
For instance, one can run:
```
Rscript compile.R
```
Figure 3 from the paper will then be generated and saved to `figures/ine-rmse.pdf`.

## Cluster Scripts

Running the necessary combinations of training data points and data generating processes
to produce the figure from the paper can be time consuming, so `cluster/` contains 
the needed scripts to run the simulations on a cluster using the SLURM scheduler.
For instance, to submit a job to SLURM that runs the first DGP with `n=100,000` using `1000` Monte Carlo replications,
one should run:
```
sbatch cluster/ine-large-1-100.sh
```








