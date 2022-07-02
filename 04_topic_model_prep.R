################################################################################
#  
#   Name: 04_topic_model_prep.R
#   
#   Purpose: Process the training text data to prepare it for the topic model
#   training process
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "04_"

texts_para <- read_fst(file.path(project_data, paste0("03_", 'train_texts_para', filesuffix, '.fst')))

all_crosswalk <- read_fst(file.path(project_data, paste0("01_", 'train_cw_all', filesuffix, '.fst')))

teacher_metadata <- read_fst(file.path(project_data, paste0("01_", "train_teachers", filesuffix, '.fst'))) %>%
  select(invitationid, year_2019, recommenderid, applicantid, letters_lasttwo_group, letters_current_group, subject, gradestaught_other, coach, years_taught)

metadata <- read_fst(file.path(project_data, paste0("02_", 'train_applicants_cleaned', filesuffix, '.fst'))) %>%
  left_join(all_crosswalk, by="applicantid") %>%
  left_join(teacher_metadata, by=c("applicantid", "invitationid", "recommenderid", "year_2019")) %>%
  mutate(letterid=paste0(invitationid, "__", year_2019))

################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################

# First get rid of paragraphs that are mostly trash
texts <- texts_para %>%
  filter(nontrash_total_characters>100)

texts_custom <- texts %>%
  # Replace email addresses
  mutate(paragraph_text=str_replace_all(paragraph_text, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "tokenemail"),
         # Replace URLs
         paragraph_text=str_replace_all(paragraph_text, "(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)", "tokenwebsite"),
         # Replace phone numbers
         paragraph_text=str_replace_all(paragraph_text, "([\\(]?[0-9]{3}[-\\s\\(\\)])?[0-9]{3}[-\\s\\(\\)][0-9]{4}", "tokenphone"),
         # Replace pre-existing token replacements from PII removal
         paragraph_text=str_replace_all(paragraph_text, "<ADDRESS>", "tokenaddress"),
         paragraph_text=str_replace_all(paragraph_text, "<NAME>", "tokenname"),
         paragraph_text=str_replace_all(paragraph_text, "<EMAIL>", "tokenemail"),
         paragraph_text=str_replace_all(paragraph_text, "<PHONE>", "tokenphone"))

# Get a rough wordcount of the paragraphs
texts_custom$rough_wordcount <- sapply(gregexpr("[[:alpha:]]+", texts_custom$paragraph_text), function(x) sum(x > 0))

# Get rid of paragraphs that are too short
texts_custom <- texts_custom %>%
  filter(rough_wordcount>3)

# Get a separate dataset of just the text of paragraphs
clean_paras <- texts_custom %>%
  select(paraid, rough_wordcount, paragraph_text)

# Tokenize and remove stop-words
tidy_texts_custom <- texts_custom %>%
  unnest_tokens(input=paragraph_text, output=word, token="words", drop=TRUE) %>%
  anti_join(stop_words) 

# Remove numbers more generally
nums <- tidy_texts_custom %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
tidy_texts_custom <- tidy_texts_custom %>%
  anti_join(nums)

# Get rid of explicitly gendered nouns
gendered <- data.frame(word=c("ms", "mr", "miss", "mister", "lady", "sir",  "man", "men", "woman", 
                              "women", "boy", "boys", "girl", "girls",
                              "madam", "madame", "gentleman"))

tidy_texts_custom <- tidy_texts_custom %>%
  anti_join(gendered)

# Run an untrimmed DFM just to get a sense of the full vocabulary and its term/doc frequency
# Uncomment to re-run
custom_dfm_tmp <- tidy_texts_custom %>%
  count(paraid, word, sort = TRUE) %>%
  cast_dfm(document=paraid, term=word, value=n) %>%
  dfm_subset(ntoken(.)>0)

full_unigram <- data.frame(word=featnames(custom_dfm_tmp), featfreq=featfreq(custom_dfm_tmp), docfreq=docfreq(custom_dfm_tmp)) %>%
  mutate(featprop=featfreq/sum(featfreq(custom_dfm_tmp)), docprop=docfreq/ndoc(custom_dfm_tmp))

write_fst(full_unigram, file.path(project_data, paste0(fileprefix, 'full_unigram_list', filesuffix, '.fst')))

rm(full_unigram, custom_dfm_tmp)

# Get a unigram dfm and trim to words that occur in at least 1/2000 paragraphs
custom_dfm <- tidy_texts_custom %>%
  count(paraid, word, sort = TRUE) %>%
  cast_dfm(document=paraid, term=word, value=n) %>%
  dfm_trim(min_docfreq=0.0005, docfreq_type="prop") %>%
  dfm_subset(ntoken(.)>0)

# Recombine the data of cleaned tokens to prepare for a bigram dfm
retokenized <- tidy_texts_custom %>%
  select(paraid, word) %>%
  group_by(paraid) %>% 
  mutate(all_text=paste(word, collapse=" ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-word)

# Tokenize the cleaned tokens into bigrams
tidy_texts_custom2 <- retokenized %>%
  unnest_tokens(input=all_text, output=word, token="ngrams", n=2, drop=TRUE)

# Get the full set of bigram tokens just to see what the full distribution looks like
# Uncomment to run again
custom_dfm2_tmp <- tidy_texts_custom2 %>%
  count(paraid, word, sort = TRUE) %>%
  filter(!is.na(word)) %>%
  cast_dfm(document=paraid, term=word, value=n) %>%
  dfm_subset(ntoken(.)>0)

full_bigram <- data.frame(word=featnames(custom_dfm2_tmp), featfreq=featfreq(custom_dfm2_tmp), docfreq=docfreq(custom_dfm2_tmp)) %>%
  mutate(featprop=featfreq/sum(featfreq(custom_dfm2_tmp)), docprop=docfreq/ndoc(custom_dfm2_tmp))

write_fst(full_bigram, file.path(project_data, paste0(fileprefix, 'full_bigram_list', filesuffix, '.fst')))

rm(full_bigram, custom_dfm2_tmp)

# Get a bigram dfm and trim to only the bigrams that exist in at least 1/2000 paragraphs
custom_dfm2 <- tidy_texts_custom2 %>%
  count(paraid, word, sort = TRUE) %>%
  filter(!is.na(word)) %>%
  cast_dfm(document=paraid, term=word, value=n) %>%
  dfm_trim(min_docfreq=0.0005, docfreq_type="prop") %>%
  dfm_subset(ntoken(.)>0)

# Join the unigram and bigram dfms together
final_dfm <- rbind(custom_dfm, custom_dfm2) %>% 
  dfm_compress(margin="documents") 

# Get a list of the features and the feature frequency and document frequency for use later
final_dfm_wordcheck <- data.frame(word=featnames(final_dfm), featfreq=featfreq(final_dfm), docfreq=docfreq(final_dfm)) %>%
  mutate(featprop=featfreq/sum(featfreq(final_dfm)), docprop=docfreq/ndoc(final_dfm))

contextual_stopwords <- final_dfm_wordcheck %>%
  filter(docprop>=0.2) %>%
  .$word

final_dfm <- final_dfm %>%
  dfm_remove(contextual_stopwords) %>%
  dfm_subset(ntoken(.)>0)

# Get a final list of paragraph id's that we will feed into the model
final_docs_list <- data.frame(paraid=docnames(final_dfm))

clean_paras <- clean_paras %>%
  right_join(final_docs_list, by="paraid")

# Put together the metadata for the stm object
metadata_prep <- data.frame(paraid=docnames(final_dfm),
                            letterid=paste0(str_split_fixed(docnames(final_dfm),"__", 3)[,1], "__", str_split_fixed(docnames(final_dfm),"__", 3)[,2]))

metadata_stm <- left_join(metadata_prep, metadata, by=c("letterid"))

# Manually removing erroneous entries from final_dfm and metadata_stm with no metadata
missingdata <- metadata_stm %>%
  filter(is.na(female))

metadata_stm <- metadata_stm %>%
  filter(!is.na(female))
  
final_dfm <- final_dfm %>%
  dfm_subset(!((docnames(.) %in% missingdata$paraid)))
  
# Create the final stm object
final_stm <- asSTMCorpus(final_dfm, data=metadata_stm)

# Get a quick count of the total tokens in each document for use later
tokencount <- data.frame(paraid=docnames(final_dfm), tokens=ntoken(final_dfm))

# Save checkpoint
datafiles1 <- c("clean_paras", "custom_dfm", "custom_dfm2", "final_dfm_wordcheck", "final_stm", "final_dfm", "metadata_stm", "tokencount")
base::save(list=datafiles1, file=file.path(project_data, paste0(fileprefix, 'topic_modeling_temp1', filesuffix, '.RData')))

# Clear out the memory
rm(all_crosswalk, clean_paras, final_dfm_wordcheck, final_docs_list, gendered, metadata,
   metadata_prep, metadata_stm, missingdata, nums, retokenized, teacher_metadata, texts,
   texts_custom, texts_para, tidy_texts_custom, tidy_texts_custom2, custom_dfm, custom_dfm2,
   final_dfm, final_stm, tokencount, datafiles1)

################################################################################
#
#   #model - Start basic topic modeling and diagnostics
#
################################################################################

base::load(file=file.path(project_data, paste0(fileprefix, 'topic_modeling_temp1', filesuffix, '.RData')))
rm(clean_paras, final_dfm_wordcheck, metadata_stm, tokencount, custom_dfm, custom_dfm2, final_dfm)

tic("Single run, Search K=0")

set.seed(83191)

topic_model_k0 <- stm(documents=final_stm$documents,
                      vocab=final_stm$vocab,
                      data=final_stm$data,
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
                      K = 0, verbose = TRUE)

toc()

base::save(topic_model_k0, file=file.path(project_data, paste0(fileprefix, 'topic_model_k0', filesuffix, '.RData')))

rm(final_stm, topic_model_k0)


