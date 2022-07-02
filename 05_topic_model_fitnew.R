################################################################################
#  
#   Name: 05_topic_model_fitnew.R
#   
#   Purpose: Take the trained topic model from 04_topic_model_training that was
#   created using the training dataset and apply it to the cleaned test dataset
#   text files. Requires cleaning the test dataset text files to align with the
#   corpus of the training dataset text first.
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "05_"

# First load in and rename relevant text files from the training data to align test data text corpus
base::load(file=file.path(project_data, paste0('04_topic_modeling_temp1', filesuffix, '.RData')))
  custom_dfm_old <- custom_dfm
  custom_dfm2_old <- custom_dfm2
  final_dfm_old <- final_dfm
  final_stm_old <- final_stm
  final_dfm_wordcheck_old <- final_dfm_wordcheck
  rm(clean_paras, custom_dfm, custom_dfm2, final_dfm_wordcheck, final_stm, final_dfm, metadata_stm, tokencount)

texts_para <- read_fst(file.path(project_data, paste0("03_", 'test_texts_para', filesuffix, '.fst')))

all_crosswalk <- read_fst(file.path(project_data, paste0("01_", 'test_cw_all', filesuffix, '.fst')))

teacher_metadata <- read_fst(file.path(project_data, paste0("01_", "test_teachers", filesuffix, '.fst'))) %>%
  select(invitationid, year_2019, recommenderid, applicantid, letters_lasttwo_group, letters_current_group, subject, gradestaught_other, coach, years_taught)

metadata <- read_fst(file.path(project_data, paste0("02_", 'test_applicants_cleaned', filesuffix, '.fst'))) %>%
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

# Need to split to deal with issue in vctrs
splitparts <- 4
remainder <- nrow(texts_custom) %% splitparts
splitter <- (nrow(texts_custom) - remainder) / splitparts

texts_custom_whole <- texts_custom %>%
  mutate(row_num=row_number(),
         split_group=ceiling(row_num/splitter),
         split_group=case_when(
           split_group>splitparts ~ splitparts,
           TRUE ~ split_group)
         )

for (i in 1:splitparts) {
  texts_custom <- texts_custom_whole %>%
    filter(split_group==i)
  
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
  
  # Get a unigram dfm and trim to words that occur in at least 1/2000 paragraphs
  custom_dfm <- tidy_texts_custom %>%
    count(paraid, word, sort = TRUE) %>%
    cast_dfm(document=paraid, term=word, value=n) %>%
    dfm_match(features=featnames(custom_dfm_old)) %>%
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
  
  # Get a bigram dfm and trim to only the bigrams that exist in at least 1/2000 paragraphs
  custom_dfm2 <- tidy_texts_custom2 %>%
    count(paraid, word, sort = TRUE) %>%
    filter(!is.na(word)) %>% 
    cast_dfm(document=paraid, term=word, value=n) %>%
    dfm_match(features=featnames(custom_dfm2_old)) %>%
    dfm_subset(ntoken(.)>0)
  
  if(i==1) {
    custom_dfm_prep <- custom_dfm
    custom_dfm2_prep <- custom_dfm2
  } else {
    custom_dfm_prep <- rbind(custom_dfm_prep, custom_dfm)
    custom_dfm2_prep <- rbind(custom_dfm2_prep, custom_dfm2)
  }
  
  rm(texts_custom, tidy_texts_custom, nums, gendered, custom_dfm, retokenized, tidy_texts_custom2, custom_dfm2)
}

custom_dfm <- custom_dfm_prep
custom_dfm2 <- custom_dfm2_prep

# Join the unigram and bigram dfms together
final_dfm <- rbind(custom_dfm, custom_dfm2) %>% 
  dfm_compress(margin="documents") %>%
  dfm_match(features=featnames(final_dfm_old)) %>%
  dfm_subset(ntoken(.)>0)

# Get a list of the features and the feature frequency and document frequency for use later
final_dfm_wordcheck <- data.frame(word=featnames(final_dfm), featfreq_fitnew=featfreq(final_dfm), docfreq_fitnew=docfreq(final_dfm)) %>%
  mutate(featprop_fitnew=featfreq_fitnew/sum(featfreq(final_dfm)), docprop_fitnew=docfreq_fitnew/ndoc(final_dfm))

final_dfm_wordcheck_combined <- final_dfm_wordcheck_old %>%
  left_join(final_dfm_wordcheck, by="word")

# Save the combined file for training and test feature frequencies
write_fst(final_dfm_wordcheck_combined, file.path(project_data, paste0(fileprefix, 'topic_model_feature_frequencies', filesuffix, '.fst')))

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
datafiles1 <- c("clean_paras", "custom_dfm", "custom_dfm2", "final_stm", "final_dfm", "metadata_stm", "tokencount")
base::save(list=datafiles1, file=file.path(project_data, paste0(fileprefix, 'topic_modeling_fitnew_temp1', filesuffix, '.RData')))
    
# Clear out the memory
rm(all_crosswalk, clean_paras, final_dfm_wordcheck, final_dfm_wordcheck_combined,
   final_docs_list, metadata, metadata_prep, metadata_stm, missingdata, 
   teacher_metadata, texts, texts_para, custom_dfm, custom_dfm_old, 
   custom_dfm2, custom_dfm2_old, final_dfm_wordcheck_old, final_dfm, final_dfm_old, 
   final_stm, final_stm_old, tokencount, datafiles1, custom_dfm_prep, custom_dfm2_prep,
   splitparts, remainder, splitter, texts_custom_whole, i)


################################################################################
#
#   #model - Use the trained topic model to produce topic proportions for test data
#
################################################################################

base::load(file=file.path(project_data, paste0('04_topic_modeling_temp1', filesuffix, '.RData')))
final_stm_old <- final_stm
rm(clean_paras, custom_dfm, custom_dfm2, final_dfm_wordcheck, final_stm, final_dfm, metadata_stm, tokencount)

base::load(file=file.path(project_data, paste0(fileprefix, 'topic_modeling_fitnew_temp1', filesuffix, '.RData')))
base::load(file=file.path(project_data, paste0("04_topic_model_k0", filesuffix, ".RData")))
topic_model <- topic_model_k0

tic("Testing")

new_docs <- fitNewDocuments(model=topic_model, documents=final_stm$documents, newData=final_stm$data, origData=final_stm_old$data,
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
                              coach + years_taught)

new_thetas <- new_docs$theta %>%
  as.data.frame()

new_thetas$document <- names(final_stm$documents)

toc()

datafiles2 <- c("new_thetas")
base::save(list=datafiles2, file=file.path(project_data, paste0(fileprefix, 'topic_modeling_fitnew_temp2', filesuffix, '.RData')))

rm(new_docs)
rm(new_thetas)
    

################################################################################
#
#   #output - Get the output
#
################################################################################

base::load(file=file.path(project_data, paste0(fileprefix, 'topic_modeling_fitnew_temp1', filesuffix, '.RData')))
base::load(file=file.path(project_data, paste0(fileprefix, 'topic_modeling_fitnew_temp2', filesuffix, '.RData')))

paragraph_counts <- tokencount %>%
  left_join(clean_paras, by="paraid") %>%
  mutate(letterid=paste0(str_split_fixed(paraid,"__", 3)[,1], "__", str_split_fixed(paraid,"__", 3)[,2])) %>%
  select(letterid) %>%
  group_by(letterid) %>%
  summarize(total_paragraphs=n()) %>%
  ungroup() %>%
  mutate(letterid=as.character(letterid))

# meaningful_paragraphs <- tokencount %>%
#   left_join(clean_paras, by="paraid") %>%
#   mutate(differential_count=rough_wordcount-tokens,
#          differential_prop=(rough_wordcount-tokens)/rough_wordcount) %>%
#   mutate(differential_prop=case_when(differential_prop<0 ~ 0,
#                                      TRUE ~ differential_prop)) %>%
#   mutate(
#     meaningful_para=case_when(
#       differential_prop < 0.85 & rough_wordcount>=20 ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   select(docnames, meaningful_para) %>%
#   rename(document=docnames)


output_prep2 <- new_thetas %>%
  left_join(tokencount, by=c("document" = "paraid")) %>%
  mutate(across(starts_with("V"), ~ . * tokens)) %>%
  rename_with(.fn = ~gsub("V", "topic_tokens_", .x, fixed=TRUE), .cols=starts_with("V")) %>%
  mutate(letterid=paste0(str_split_fixed(document,"__", 3)[,1], "__", str_split_fixed(document,"__", 3)[,2]))

output <- output_prep2 %>%
  group_by(letterid) %>%
  summarize(across((!document), sum)) %>%
  ungroup() %>%
  mutate(across(starts_with("topic_tokens"), ~replace_na(.x, 0)))

write_fst(output, file.path(project_data, paste0(fileprefix, 'test_topics_cleaned', filesuffix, '.fst')))

# 
# for(i in letters) {
# base::load(file=file.path(project_data, paste0('99_topic_modeling_temp1', i, filesuffix, '.RData')))
# base::load(file=file.path(project_data, paste0('99_topic_modeling_temp2', i, filesuffix, '.RData')))
# 
# paragraph_counts <- tokencount %>%
#   left_join(clean_paras, by="docnames") %>%
#   select(invitationid) %>%
#   group_by(invitationid) %>%
#   mutate(total_paragraphs=n()) %>%
#   select(invitationid, total_paragraphs) %>%
#   ungroup() %>%
#   mutate(invitationid=as.character(invitationid))
# 
# meaningful_paragraphs <- tokencount %>%
#   left_join(clean_paras, by="docnames") %>%
#   mutate(differential_count=rough_wordcount-tokens,
#          differential_prop=(rough_wordcount-tokens)/rough_wordcount) %>%
#   mutate(differential_prop=case_when(differential_prop<0 ~ 0,
#                                      TRUE ~ differential_prop)) %>%
#   mutate(
#     meaningful_para=case_when(
#       differential_prop < 0.85 & rough_wordcount>=20 ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   select(docnames, meaningful_para) %>%
#   rename(document=docnames)
# 
# td_gamma2 <- new_thetas %>%
#   pivot_longer(cols=(!document), names_prefix="V", names_to="topic", names_transform=list(topic=as.integer), values_to="gamma") 
# 
# output_prep2 <- td_gamma2 %>%
#   left_join(tokencount, by=c("document"="docnames")) %>%
#   mutate(topic_tokens=gamma*tokens) %>%
#   pivot_wider(id_cols=document, names_from=topic, names_prefix="topic_tokens_", values_from=c(topic_tokens)) %>%
#   mutate(invitationid=str_split_fixed(document,"_", 2)[,1]) %>%
#   left_join(tokencount, by=c("document"="docnames"))
# 
# output <- output_prep2 %>%
#   group_by(invitationid) %>%
#   summarize(across((!document), sum)) %>%
#   ungroup() %>%
#   mutate(across(starts_with("topic_tokens"), ~replace_na(.x, 0)))
# 
# write_fst(output, file.path(project_data, paste0('topics_cleaned', filesuffix, i, '.fst')))
# 
# }


# This is how to do it if you want having at least one paragraph primarily about topic X
# td_gamma2 <- new_thetas %>%
#   pivot_longer(cols=(!document), names_prefix="V", names_to="topic", names_transform=list(topic=as.integer), values_to="gamma")
#   
# set.seed(1234)
# 
# output_prep2 <- td_gamma2 %>%
#   group_by(document) %>%
#   arrange(desc(gamma), .by_group=TRUE) %>%
#   mutate(place=row_number()) %>%
#   slice_head(n=2) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=document, names_from=place, values_from=c(topic, gamma)) %>%
#   left_join(meaningful_paragraphs, by="document") %>%
#   mutate(topic_diff=gamma_1-gamma_2,
#          counter=case_when(
#            gamma_1>=0.1 & topic_diff>=0.05 & meaningful_para==1 ~ 1,
#            gamma_1>=0.05 & topic_diff>=0.01 & meaningful_para==1 ~ 1,
#            TRUE ~ 0
#            ),
#          topic_1=factor(topic_1, levels=c(1:80)),
#          invitationid=str_split_fixed(document,"_", 2)[,1]
#          ) %>%
#   filter(counter==1)
# 
# dummy_prep <- output_prep2 %>%
#   select(topic_1) %>%
#   dummyVars(data=., " ~ .")
# dummy <- data.frame(predict(dummy_prep, newdata=output_prep2))
# 
# output <- output_prep2 %>%
#   select(invitationid) %>%
#   bind_cols(dummy) %>%
#   group_by(invitationid) %>%
#   summarize(across(everything(), max)) %>%
#   ungroup() %>%
#   right_join(paragraph_counts, by="invitationid") %>%
#   mutate(across(starts_with("topic_1"), ~replace_na(.x, 0)))


#test <- read_fst(file.path(project_data, paste0('topics_cleaned', filesuffix, '.fst')))
# Check for topic prevalence of gendered terms
td_beta <- tidy(topic_model)
gendered <- data.frame(word=c("miss", "miss tokenname", "ms", "mr", "mister", "ms tokenname", "ms tokenname's", "recommend ms", "lady", 
                              "woman", "mature woman", "tokenname woman", "woman tokenname", "boy", "girl"))

gendered_check <- td_beta %>%
  right_join(gendered, by=c("term"="word"))

suffix <- "_2019_21-10-29"

texts <- read_fst(file.path(project_data, paste0(fileprefix, 'texts_para', suffix, '.fst'))) %>%
  filter(nontrash_total_characters>100) %>%
  mutate(doc_id=paste0(invitationid, "_", paragraph_num))


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

clean_paras <- texts_custom %>%
  mutate(rough_wordcount=str_count(paragraph_text, "\\s")) %>%
  rename(docnames=doc_id) %>%
  select(invitationid, docnames, rough_wordcount, paragraph_text)

supertopics <- read_xlsx(path=file.path(project_data, paste0('topic_codes_bk_21-10-18b.xlsx')), sheet="Supertopics") %>%
  filter(!is.na(description)) %>%
  select(topic, supertopic) %>%
  mutate(topic=as.character(topic))

checker <- clean_paras %>%
  left_join(output_prep2, by=c("docnames" = "document")) %>%
  pivot_longer(cols=starts_with("topic_tokens_"), names_to="topic", names_prefix="topic_tokens_", values_to="topic_tokens")


checker2 <- checker[1:1000000,] %>%
  left_join(supertopics, by="topic") %>%
  group_by(docnames, tokens, paragraph_text, supertopic) %>%
  summarize(topic_tokens=sum(topic_tokens)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c(docnames, tokens, paragraph_text), names_from=supertopic, names_prefix="st_", values_from=topic_tokens)
