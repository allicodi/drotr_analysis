#!/bin/bash

PARTITION=$1
NSEEDS=$2

sbatch --array=1-$NSEEDS \
       --partition=$PARTITION \
       -n 1 \
       --output=/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/scratch/%a_%J.out \
       --job-name=run_seed_%a \
       --wrap "Rscript 02.1_run_simulation_gold_standard.R $SLURM_ARRAY_TASK_ID"
