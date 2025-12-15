PARTITION=$1
NSEEDS=5

sbatch --array=1-$NSEEDS \
       --partition=$PARTITION \
       -n 1 \
       --output=/home/acodi/drotr_sim/journal/real_data/scratch/%a_%J.out \
       --job-name=run_seed_%a \
       --wrap "Rscript 02_run_analysis.R $SLURM_ARRAY_TASK_ID"
