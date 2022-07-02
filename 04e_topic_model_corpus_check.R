################################################################################
#  
#   Name: 04e_topic_model_corpus_check.R
#   
#   Purpose: This file takes in the list of unigrams and bigrams in the training
#   data and examines how different cutoffs for document frequency would change
#   the ensuing corpus. This is just an informal exploration file for scoping.
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

unigrams <- read_fst(file.path(project_data, paste0('04_full_unigram_list', filesuffix, '.fst')))
bigrams <- read_fst(file.path(project_data, paste0('04_full_bigram_list', filesuffix, '.fst')))

final_dfm_wordcheck_combined <- read_fst(file.path(project_data, paste0('05_topic_model_feature_frequencies', filesuffix, '.fst')))

docprop_threshold_bottom <- 0.0005
docprop_threshold_top <- 0.2

# How many unigrams made the cut?
unigrams %>%
  filter(docprop>=docprop_threshold_bottom & docprop<docprop_threshold_top) %>%
  nrow()

# How many unigrams got removed?
unigrams %>%
  filter(docprop<docprop_threshold_bottom) %>%
  nrow()

# What proportion of keywords were the cut unigrams?
unigrams %>%
  filter(docprop<docprop_threshold_bottom) %>%
  .$featfreq %>%
  sum() / 
  sum(unigrams$featfreq)

# How many unigrams made the cut?
bigrams %>%
  filter(docprop>=docprop_threshold_bottom) %>%
  nrow()

# How many unigrams got removed?
bigrams %>%
  filter(docprop<docprop_threshold_bottom) %>%
  nrow()

# What proportion of keywords were the cut unigrams?
bigrams %>%
  filter(docprop<docprop_threshold_bottom) %>%
  .$featfreq %>%
  sum() / 
  sum(bigrams$featfreq)

rm(docprop_threshold, unigrams, bigrams)
