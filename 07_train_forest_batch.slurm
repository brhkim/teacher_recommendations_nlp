#!/bin/bash
#SBATCH --nodes=1             
#SBATCH --ntasks=1
#SBATCH --mem=256GB
#SBATCH --time=48:00:00       
#SBATCH --partition=standard
#SBATCH --output=/project/commonappteacherrec/teacher_rec_full_replication/code/logs/07_train_forest_out.out
#SBATCH --account=commonappteacherrec

module purge
module load jupyter_conda/.2020.11-py3.8 cuda/10.2.89 cudatoolkit/11.0.3-py3.8 cudnn/7.6.5.32
module load singularity

export filenameset="03_train_texts_sentence_21-12-20.dta"
export suffixset="21-12-20"
export rootset="/project/commonappteacherrec/teacher_rec_full_replication/data/build"
export clusterfirst="1"
export clusterlast="32"
export datagroupset="train"

singularity run --nv /project/commonappteacherrec/teacher_rec_full_replication/data/build/tensorflow_2.4.1-bhk5fs.sif "/project/commonappteacherrec/teacher_rec_full_replication/code/07_forest_python.py"

