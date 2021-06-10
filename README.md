# Replication Code for identification-not-enough Paper

This repo contains the code to run several simulations from the identification not enough note.

## Instructions

In order to run one run of the simulations, using seed 99, one should run:
```
Rscript code/run_simul.R --s 99
```

In order to submit an array of jobs to the cluster, using the list of seeds in `jobs/params.txt`,
one should use the script:

```
sbatch jobs/submit_sims.sh
```

In order to create the plots after running the simulation, one should run (the resulting plots will be saved
in `code/figures`/):

```
Rscript code/plot_simul.R
```
