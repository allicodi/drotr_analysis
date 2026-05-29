#!/bin/bash

PARTITION=$1
NSEEDS=$2
SENS=$3

sbatch --array=1-$NSEEDS \
       --partition=$PARTITION \
       -n 1 \
       --output=/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/scratch/truth_%a_%J.out \
       --job-name=run_data_adaptive_seed_%a \
       --wrap "Rscript sensitivity/13_get_data_adaptive_truth_sensitivity.R $SENS $SLURM_ARRAY_TASK_ID"
