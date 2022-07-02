################################################################################
#  
#   Name: 00_teacher_recs_main.R
#   
#   Purpose: Main analytic root file to conduct the teacher recommendation
#   letter analysis by Brian Heseung Kim. This script will set analytic
#   parameters, file hierarchies, and other relevant settings before running
#   each analytic script in the analysis in order.
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

#install.packages("pacman")
pacman::p_load('tidyverse', 'haven', 'pryr', 'lubridate', 'stm', 'tidytext', 'readxl', 'xlsx',
               'furrr', 'ggthemes', 'ggridges', 'glmnet', 'quanteda', 'quanteda.textmodels', 'tm', 
               'beepr', 'fst', 'fixest', 'tictoc', 'tidylog', 'caret', 'kableExtra', 'doMC')

setwd("/project/commonappteacherrec/teacher_rec_full_replication")

project_code <- file.path('code')
project_data <- file.path('data', 'build')
raw_data <- file.path('data', 'raw')
project_output <- file.path('output')

subsample_proportion <- 10

filesuffix <- "_21-12-20"
maxclustertrain <- 32
maxclustertest <- 160

package_log <- writeLines(capture.output(sessionInfo()), file.path(project_output, paste0("00_sessioninfo", filesuffix, ".txt")))


# Set up file logging
#current_time <- paste(format(Sys.time(), "%Y-%m-%d--%H-%M-%S"))
#sink(file.path(project_code, "logs", paste0(current_time, "_", "history_output", filesuffix, ".txt")), append=FALSE, type="output")


################################################################################
#
#   #mainscripts - Call individual scripts to complete and visualize main analysis
#
################################################################################

# Main analysis; scripts are dependent on all prior scripts in order unless
# otherwise specified
set.seed(1234)
source(file.path(project_code, '01_subsample_creation.R'))

# Run descriptives cleaning among training dataset
dataset_group <- "train"
set.seed(5678)
source(file.path(project_code, '02_descriptives_cleaning.R'))

dataset_group <- "test"
set.seed(9012)
source(file.path(project_code, '02_descriptives_cleaning.R'))

dataset_group <- "train"
set.seed(3456)
source(file.path(project_code, '03_letter_cleaning.R'))

dataset_group <- "test"
set.seed(7890)
source(file.path(project_code, '03_letter_cleaning.R'))

set.seed(12345)
source(file.path(project_code, '04_topic_model_prep.R'))

topic_model_search_slurm_call <- "sbatch /project/commonappteacherrec/teacher_rec_full_replication/code/04b_topic_model_search.slurm"
cat(topic_model_search_slurm_call)
system(topic_model_search_slurm_call)

set.seed(1234567)
source(file.path(project_code, '04c_topic_model_selection.R'))

set.seed(67890)
source(file.path(project_code, '05_topic_model_fitnew.R'))

# Run the sentiment algorithms on the training data
train_sentiment_slurm_call <- "sbatch /project/commonappteacherrec/teacher_rec_full_replication/code/06_train_sentiment_batch.slurm"
cat(train_sentiment_slurm_call)
train_sentiment_jobid <- system(train_sentiment_slurm_call, intern=TRUE) %>%
  str_replace(pattern="Submitted batch job ", "")

train_forest_slurm_call <- paste0("sbatch --dependency=afterok:", train_sentiment_jobid, " ", "/project/commonappteacherrec/teacher_rec_full_replication/code/07_train_forest_batch.slurm")
cat(train_forest_slurm_call)
system(train_forest_slurm_call)

# Run the sentiment algorithms on the test data
test_sentiment_slurm_call <- "sbatch /project/commonappteacherrec/teacher_rec_full_replication/code/06_test_sentiment_batch.slurm"
cat(test_sentiment_slurm_call)
test_sentiment_jobid <- system(test_sentiment_slurm_call, intern=TRUE) %>%
  str_replace(pattern="Submitted batch job ", "")

test_forest_slurm_call <- paste0("sbatch --dependency=afterok:", test_sentiment_jobid, " ", "/project/commonappteacherrec/teacher_rec_full_replication/code/07_test_forest_batch.slurm")
cat(test_forest_slurm_call)
system(test_forest_slurm_call)

dataset_group <- "train"
set.seed(123456)
source(file.path(project_code, '08_analytic_dataset.R'))

dataset_group <- "test"
set.seed(789012)
source(file.path(project_code, '08_analytic_dataset.R'))

set.seed(345678)
source(file.path(project_code, '09_descriptives_table.R'))

dataset_group <- "train"
set.seed(901234)
source(file.path(project_code, '10_regression_analysis.R'))

dataset_group <- "test"
set.seed(567890)
source(file.path(project_code, '10_regression_analysis.R'))

dataset_group <- "test"
set.seed(1234)
source(file.path(project_code, '10c_subsample_analysis.R'))

# Close file logging
#sink()
#savehistory(file=file.path(project_code, "logs", paste0(current_time, "_", "history_commands", filesuffix,".Rhistory")))


