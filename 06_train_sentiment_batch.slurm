#!/bin/bash
#SBATCH --nodes=1             
#SBATCH --ntasks=1
#SBATCH --mem=64GB
#SBATCH --time=72:00:00       
#SBATCH --partition=gpu
#SBATCH --output=/project/commonappteacherrec/teacher_rec_full_replication/code/logs/06_train_sentiment_batch_%a.out
#SBATCH --account=commonappteacherrec
#SBATCH --gres=gpu
#SBATCH --array=1-32%16
#SBATCH -x udc-an28-[1,7]

module purge
module load jupyter_conda/.2020.11-py3.8 cuda/10.2.89 cudatoolkit/11.0.3-py3.8 cudnn/7.6.5.32
module load singularity

export arraymaxbk=32
export arraycurrentbk=${SLURM_ARRAY_TASK_ID}
export filenameset="03_train_texts_sentence_21-12-20.dta"
export suffixset="21-12-20"
export rootset="/project/commonappteacherrec/teacher_rec_full_replication/data/build"
export datagroupset="train"

singularity run --nv /project/commonappteacherrec/teacher_rec_full_replication/data/build/tensorflow_2.4.1-bhk5fs.sif "/project/commonappteacherrec/teacher_rec_full_replication/code/06_sentiment_python.py"

