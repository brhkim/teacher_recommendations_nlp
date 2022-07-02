################################################################################
#  
#   Name: 08_analytic_dataset.R
#   
#   Purpose: Combine the output of the various NLP algorithms into the same
#   unified dataset for analysis
#
################################################################################

################################################################################
#
#   #import - Load in a datasets
#
################################################################################

fileprefix <- "08_"

# First check integrity of sentiment analysis datasets
if(dataset_group=="train") {
  maxcluster <- maxclustertrain
} else {
  maxcluster <- maxclustertest
}

for (filenum in 1:maxcluster){
  # First check the logs to see if the runs completed
  check <- read_file(file=file.path(project_code, "logs", paste0("06_", dataset_group, "_sentiment_batch_", filenum, ".out"))) %>%
    # Crude check but see how often it spits out the duration for each transformer run
    str_count(pattern="seconds")
  
  check2 <- file.exists(file.path(project_data, paste0("06_", dataset_group, "_sentiment_prep_", filenum, ".csv")))
  
  if (filenum==1) {
    log_array <- check
    data_array <- check2
  } else {
    log_array[filenum] <- check
    data_array[filenum] <- check2
  }
}
log_check <- log_array %>% mean() == 7
data_check <- (data_array %>% all())

sentiment_completion_check <- c(log_check, data_check) %>% all()
sentiment_completion_check

# Now compile data for sentiment analysis data
applicants <- read_fst(file.path(project_data, paste0('02_', dataset_group, '_applicants_cleaned', filesuffix, ".fst")))

teacher_metadata <- read_fst(file.path(project_data, paste0('01_', dataset_group, '_teachers', filesuffix, '.fst'))) %>%
  select(invitationid, recommenderid, applicantid, year_2019, letters_lasttwo_group, letters_current_group, subject, gradestaught_other, coach, years_taught)

metadata <- read_fst(file.path(project_data, paste0('01_', dataset_group, '_cw_all', filesuffix, '.fst'))) %>%
  left_join(applicants, by="applicantid") %>%
  left_join(teacher_metadata, by=c("applicantid", "invitationid", "recommenderid", "year_2019")) %>%
  mutate(letterid=paste0(invitationid, "__", year_2019))

sentiment_sentence <- read_csv(file.path(project_data, paste0('07_', dataset_group, '_forest_output', filesuffix, '.csv'))) %>%
  select(-X1) %>%
  mutate(
    label_predict_forest = as.numeric(str_sub(label_predict_forest, -1)) - 2,
    label_predict_yelp = case_when(
      yelp__LABEL_0==1 ~ -2,
      yelp__LABEL_1==1 ~ -1,
      yelp__LABEL_2==1 ~ 0,
      yelp__LABEL_3==1 ~ 1,
      yelp__LABEL_4==1 ~ 2
    ),
    label_predict_xlnet = case_when(
      xlnet__LABEL_0==1 ~ -1,
      xlnet__LABEL_4==1 ~ 1
    ),
    label_predict_albert = case_when(
      albert__LABEL_0==1 ~ -1,
      albert__LABEL_4==1 ~ 1
    ),
    label_predict_stanza = case_when(
      stanza__LABEL_0==1 ~ -1,
      stanza__LABEL_2==1 ~ 0,
      stanza__LABEL_4==1 ~ 1
    ),
    label_predict_bert = case_when(
      bert__LABEL_0==1 ~ -2,
      bert__LABEL_1==1 ~ -1,
      bert__LABEL_2==1 ~ 0,
      bert__LABEL_3==1 ~ 1,
      bert__LABEL_4==1 ~ 2
    ),
    label_predict_twit = case_when(
      twit__LABEL_0==1 ~ -1,
      twit__LABEL_2==1 ~ 0,
      twit__LABEL_4==1 ~ 1
    ),
    label_predict_imdb = case_when(
      imdb__LABEL_0==1 ~ -1,
      imdb__LABEL_2==1 ~ 0,
      imdb__LABEL_4==1 ~ 1
    )
  ) %>%
  select(-contains("__")) %>%
  left_join(metadata, by="letterid")

# Spot-check sentiment scores for sentences across scores
# set.seed(1234)
# checker <- train_joined %>%
#   group_by(label_predict_forest) %>%
#   slice_sample(n=100)

# Make student-level dataset
sentiment_aggregated <- sentiment_sentence %>%
  select(letterid, label_predict_twit) %>%
  group_by(letterid, label_predict_twit) %>%
  summarize(level_count=n()) %>%
  group_by(letterid) %>%
  mutate(total_count=sum(level_count)) %>%
  ungroup() %>%
  pivot_wider(names_prefix="sentiment_", names_from="label_predict_twit", values_from="level_count", values_fill=0) %>%
  rename(sentiment_neg=`sentiment_-1`,
         sentiment_neu=`sentiment_0`,
         sentiment_pos=`sentiment_1`
  ) %>%
  mutate(sum_sent=(-1 * sentiment_neg) + (0 * sentiment_neu) + (1 * sentiment_pos),
         avg_sent=sum_sent/total_count) %>%
  left_join(metadata, by="letterid")

if(dataset_group=="train") {
  topics_cleaned <- read_fst(path=file.path(project_data, paste0("04d_train_topics_cleaned", filesuffix, ".fst")))
} else {
  topics_cleaned <- read_fst(path=file.path(project_data, paste0("05_test_topics_cleaned", filesuffix, ".fst")))
}

# Load in the supertopic data crosswalk
supertopics <- read_csv(file=file.path(project_data, paste0('04d_topic_codes_bk', filesuffix, ".csv"))) %>%
  filter(!is.na(description)) %>%
  select(topic, supertopic) %>%
  mutate(topic=as.character(topic))

supertopics_cleaned <- topics_cleaned %>%
  pivot_longer(cols=starts_with("topic_tokens_"), names_to="topic", names_prefix="topic_tokens_", values_to="topic_tokens") %>%
  left_join(supertopics, by="topic") %>%
  group_by(letterid, tokens, supertopic) %>%
  summarize(topic_tokens=sum(topic_tokens)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c(letterid, tokens), names_from=supertopic, names_prefix="st_", values_from=topic_tokens) %>%
  rename(st_academic_excellence=`st_Academic Excellence`,
         st_advanced_coursetaking=`st_Advanced Coursetaking`,
         st_character_excellence=`st_Character Excellence`,
         st_community_engagement=`st_Community Engagement`,
         st_extracurriculars=`st_Extracurriculars`,
         st_formalities=`st_Formalities`,
         st_future_success=`st_Future Success Potential`,
         st_humanities=`st_Humanities`,
         st_sports=`st_Sports`,
         st_intellectual=`st_Intellectual Promise`,
         st_leadership=`st_Leadership`,
         st_other=`st_Other`,
         st_stem=`st_STEM`,
         st_time=`st_Time and Life Management`) %>%
  mutate(across(starts_with("st_"), ~
                case_when(
                  . < median(.) ~ 0,
                  . >= median(.) ~ 1
                ), .names="{.col}_50"),
       across(starts_with("st_") & !ends_with("_50"), ~
                case_when(
                  . < quantile(., probs=0.8) ~ 0,
                  . >= quantile(., probs=0.8) ~ 1
                ), .names="{.col}_80"))

analytic_data <- sentiment_aggregated %>%
  inner_join(supertopics_cleaned, by="letterid")

sentiment_sentence <- sentiment_sentence %>%
  inner_join(supertopics_cleaned %>% select(letterid), by="letterid")

recssent <- read_fst(path=file.path(project_data, paste0('02_', dataset_group, '_institution_apps', filesuffix, '.fst'))) %>%
  mutate(invitationid=as.numeric(invitationid)) %>%
  rename(year_2019=year_2019.x) %>%
  select(-season, -year_2019.y) %>%
  select(applicantid, invitationid, recommenderid, year_2019, member_idmasked)

analytic_data_inst <- analytic_data %>%
  left_join(recssent, by=c("applicantid", "invitationid", "recommenderid", "year_2019")) %>%
  select(-starts_with("LABEL_"))


write_fst(sentiment_sentence, path=file.path(project_data, paste0(fileprefix, dataset_group, '_sentiment_analytic_data_sentence', filesuffix, '.fst')))
write_fst(analytic_data, path=file.path(project_data, paste0(fileprefix, dataset_group, '_analytic_data', filesuffix, '.fst')))
write_fst(analytic_data_inst, path=file.path(project_data, paste0(fileprefix, dataset_group, '_analytic_data_inst', filesuffix, '.fst')))


rm(sentiment_sentence, sentiment_aggregated, metadata, teacher_metadata, applicants,
   fileprefix, log_check, data_check, maxcluster, check, check2, log_array, data_array,
   analytic_data, topics_cleaned, supertopics, supertopics_cleaned, recssent, analytic_data_inst)

