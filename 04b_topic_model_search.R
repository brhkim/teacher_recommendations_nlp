################################################################################
#  
#   Name: 04b_topic_model_search.R
#   
#   Purpose: This file takes in the processed STM corpus as created in
#   04_topic_model_prep and searches through a variety of K parameters to
#   eventually be evaluated for the best model. Note that this should be
#   called through the Slurm script, rather than directly.
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

.libPaths(new=c("~r_packages", "/home/bhk5fs/r_packages", "/library", "/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library"))

#install.packages("pacman")
pacman::p_load('tidyverse', 'haven', 'pryr', 'lubridate', 'stm', 'tidytext', 'readxl', 'xlsx',
               'furrr', 'ggthemes', 'ggridges', 'quanteda', 'tm', 'beepr', 'fst',
               'fixest', 'tictoc', 'tidylog', 'caret', 'kableExtra')

#root <- "/project/commonappteacherrec/teacher_rec_full_replication"
root <- as.character(Sys.getenv("rootset"))
setwd(root)

project_code <- file.path('code')
project_data <- file.path('data', 'build')
raw_data <- file.path('data', 'raw')
project_output <- file.path('output')

set.seed(1234)

#filesuffix <- "21-12-20"
filesuffix <- as.character(Sys.getenv("suffixset"))

#currentarray <- 10
currentarray <- as.numeric(Sys.getenv("arraycurrentbk"))

################################################################################
#
#   #specifications - Set up specific file paths and analytic options for the
#     rest of the analysis
#
################################################################################

base::load(file=file.path(project_data, paste0('04_topic_modeling_temp1_', filesuffix, '.RData')))
base::load(file=file.path(project_data, paste0('04_topic_model_k0_', filesuffix, '.RData')))

topic_model_k0_num <- topic_model_k0$settings$dim$K

# test <- prepDocuments(documents=final_stm$documents, vocab=final_stm$vocab, 
#                       meta=final_stm$data, subsample=10000)

rm(custom_dfm, custom_dfm2, final_dfm, topic_model_k0, clean_paras, final_dfm_wordcheck, metadata_stm, tokencount)

seed_array <- c(1234, 5678, 9012, 3456, 
                7890, 12345, 67890, 123456, 
                789012, 345678, 901234, 567890,
                1234567, 8901234, 5789012, 3456789)

K_array <- c(topic_model_k0_num-60, topic_model_k0_num-50, topic_model_k0_num-40, topic_model_k0_num-30, 
             topic_model_k0_num-25, topic_model_k0_num-20, topic_model_k0_num-15, topic_model_k0_num-10, 
             topic_model_k0_num-5, topic_model_k0_num, topic_model_k0_num+5, topic_model_k0_num+10, 
             topic_model_k0_num+15, topic_model_k0_num+20, topic_model_k0_num+30, topic_model_k0_num+40)

K_test <- K_array[currentarray]
seed_test <- seed_array[currentarray]

tic("Start searching through K's")

#many_models <- stm(documents=test$documents, vocab=test$vocab, data=test$meta, 
many_models <- stm(documents=final_stm$documents, vocab=final_stm$vocab, data=final_stm$data,
                   prevalence=~ year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                     race_miss + black + asian + latinx + other_race +
                     firstgen + firstgen_miss + school_private + school_other +
                     senior + multischool + multischool_miss + feewaiver + 
                     quintile_rank + gpa_bucket + toefl_percentile + math_pctile_bucket + 
                     verb_pctile_bucket + total_main_tests_bucket + sat_subjects_taken + avg_sat_subject_bucket +
                     ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + 
                     ap_tests_arts + score_avg_english + score_avg_languages + score_avg_socialstudies + 
                     score_avg_stem + score_avg_arts + any_ib +
                     any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                     leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                     excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                     s(total_leadership) + s(total_excellence) + s(total_mentorship) + s(total_activities) + 
                     total_apps + any_early + early_only + app_selectivity_quintile +
                     letters_lasttwo_group + letters_current_group + subject + gradestaught_other +
                     coach + years_taught, 
                   K = K_test, verbose = TRUE, seed = seed_test)

base::save(many_models, file=file.path(project_data, paste0('04b_many_models_', filesuffix, "_", currentarray, '.RData')))

toc()

