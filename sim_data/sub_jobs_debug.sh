#!/bin/bash

PARTITION=$1
NSEEDS=$2

sbatch --array=1-$NSEEDS \
       --partition=$PARTITION \
       -n 1 \
       --output=/projects/dbenkes/allison/drotr_sim/journal/results_sim_data/scratch/debug_%a_%J.out \
       --job-name=debug_seed_%a \
       --wrap "Rscript debug_gs_script.R $SLURM_ARRAY_TASK_ID"
