#!/bin/bash
#SBATCH --nodes=1             
#SBATCH --ntasks=1
#SBATCH --mem=128GB
#SBATCH --time=96:00:00       
#SBATCH --partition=standard
#SBATCH --output=/project/commonappteacherrec/teacher_rec_full_replication/code/logs/04b_topic_model_search_%a.out
#SBATCH --account=commonappteacherrec
#SBATCH --array=1-16

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/4.1.1

export arraycurrentbk=${SLURM_ARRAY_TASK_ID}
export rootset="/project/commonappteacherrec/teacher_rec_full_replication"
export suffixset="21-12-20"

Rscript "/project/commonappteacherrec/teacher_rec_full_replication/code/04b_topic_model_search.R"
