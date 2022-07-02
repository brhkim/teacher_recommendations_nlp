################################################################################
#  
#   Name: 06e_validation_set_generator.R
#   
#   Purpose: Pull stratified random sample of cases from sentiment analysis
#   and topic modeling output to conduct human-algorithm validation processes
#
#
################################################################################



################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

set.seed(83191)

supertopics <- read_xlsx(path=file.path(project_data, paste0('topic_codes_bk_21-10-18b.xlsx')), sheet="Supertopics") %>%
  filter(!is.na(description)) %>%
  select(topic, supertopic)

suffixes <- c("_2018_21-10-29", "_2019_21-10-29")
letters <- c("a", "b", "c", "d", "e", "f")

# First get a crosswalk

## Load in the data
for(suffix in suffixes) {
  
  for(i in letters) {

    base::load(file=file.path(project_data, paste0('99_topic_modeling_temp1', i, suffix, '.RData')))
    rm(custom_dfm)
    rm(custom_dfm2)
    rm(final_dfm)
    rm(final_stm)
    
    base::load(file=file.path(project_data, paste0('99_topic_modeling_temp2', i, suffix, '.RData')))

    subsample_covs <- metadata_stm %>%
      filter(female_miss==0 & race_miss==0 & other_race==0 & school_other==0) %>%
      select(docnames, female, white, black, asian, latinx, school_private) %>%
      group_by(female, white, black, asian, latinx, school_private) %>%
      slice_sample(n=5000) %>%
      ungroup()
    
    subsample_list <- subsample_covs %>%
      select(docnames) %>%
      unique()
    
    td_gamma2 <- new_thetas %>%
      right_join(subsample_list, by=c("document" = "docnames")) %>% 
      left_join(tokencount, by=c("document"="docnames")) %>%
      pivot_longer(cols=(!document & !tokens), names_prefix="V", names_to="topic", names_transform=list(topic=as.integer), values_to="gamma") %>%
      left_join(supertopics, by="topic") %>%
      group_by(document, tokens, supertopic) %>%
      summarize(supertopic_prop=sum(gamma)) %>%
      ungroup() %>%
      pivot_wider(id_cols=document, names_from=supertopic, names_prefix="st_", values_from=c(supertopic_prop)) %>%
      left_join(subsample_covs, by=c("document"="docnames")) %>%
      left_join(clean_paras, by=c("document"="docnames")) %>%
      left_join(tokencount, by=c("document"="docnames")) %>%
      select(-rough_wordcount)
    
    if(suffix=="_2018_21-10-29" & i=="a") {
      combined_2018 <- td_gamma2
    } else if (suffix=="_2018_21-10-29") {
      combined_add_2018 <- td_gamma2
      
      combined_2018 <- combined_2018 %>%
        bind_rows(combined_add_2018) %>%
        mutate(year_2019=0)
    }
    
    if(suffix=="_2019_21-10-29" & i=="a") {
      combined_2019 <- td_gamma2
    } else if (suffix=="_2019_21-10-29") {
      combined_add_2019 <- td_gamma2
      
      combined_2019 <- combined_2019 %>%
        bind_rows(combined_add_2019) %>%
        mutate(year_2019=1)
    }
    
    rm(td_gamma2)
    rm(metadata_stm)
    rm(new_thetas)
    rm(clean_paras)
    
    print(paste0("Done with ", suffix, " part ", i))
    
  }
}

combined <- combined_2018 %>%
  bind_rows(combined_2019)

write_fst(combined, path=file.path(project_data, paste0('topic_validation_2019', filesuffix, '.fst')))

reasonable_length <- combined %>%
  filter(tokens<=40 & tokens>=20) 

column_names <- reasonable_length %>%
  select(starts_with("st_") & !st_Miscellaneous) %>%
  names()

counter <- 1
for (i in column_names) {
  temp <- reasonable_length %>%
    mutate(max_st=pmax(!!!rlang::syms(column_names)))
  
  output <- temp[temp[[i]]==temp[["max_st"]],] %>%
    mutate(supertopic=i)
  
  if (counter==1) {
    topic_validation_output <- output
  } else {
      topic_validation_output <- topic_validation_output %>%
        bind_rows(output)
    }
  
  counter <- counter+1
}

set.seed(83191)

topic_validation_output_subsample <- topic_validation_output %>%
  group_by(female, white, black, asian, latinx, school_private, supertopic, year_2019) %>%
  slice_sample(n=1) %>%
  ungroup() %>%
  mutate(random_order=rnorm(nrow(.))) %>%
  arrange(random_order) %>%
  select(-random_order)

write_csv(topic_validation_output_subsample, file.path(project_data, "06e_topic_validation_key.csv"))

topic_set_1 <- topic_validation_output_subsample %>%
  slice_head(n=100) %>%
  select(paragraph_text) %>%
  mutate(most_prevalent="", second_most_prevalent="")

topic_set_2 <- topic_validation_output_subsample %>%
  slice_tail(n=(nrow(topic_validation_output_subsample)-100)) %>%
  select(paragraph_text) %>%
  mutate(most_prevalent="", second_most_prevalent="")

set.seed(83191)

initials <- c("BK", "EA", "DP", "HM", "KO", "JM", "RD", "SM")

for (i in initials) {
  temp_1 <- topic_set_1 %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)

  temp_2 <- topic_set_2 %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)
  
  write.xlsx(temp_1, file=file.path(project_data, paste0("06e_topic_validation_", i, "a.xlsx")))
  write.xlsx(temp_2, file=file.path(project_data, paste0("06e_topic_validation_", i, "b.xlsx")))
}

    
################################################################################
#
#   #sentiment - Get sample of sentiment output
#
################################################################################

crosswalk_2019 <- read_fst(file.path(project_data, paste0(fileprefix, 'cw_all', filesuffix, '.fst')))

applicants_2019 <- read_fst(file.path(project_data, paste0(fileprefix, 'applicants_cleaned', filesuffix, '.fst'))) %>%
  select(applicantid, female, white, black, asian, latinx, school_private, female_miss, race_miss, other_race, school_other)

texts_sentence_2019a <- read_fst(file.path(project_data, paste0(fileprefix, 'texts_sentence', '_2019_21-10-29', '.fst')))

texts_sentence_2019b <- read_fst(file.path(project_data, paste0(fileprefix, 'texts_sentence', '_2019_21-10-29b', '.fst')))

texts_sentence_2019 <- bind_rows(texts_sentence_2019a, texts_sentence_2019b) %>%
  mutate(merge=row_number())

rm(texts_sentence_2019a)
rm(texts_sentence_2019b)

sentiment_2019a <- read_csv(file.path(project_data, paste0('99_forest_output', "_2019a-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2019a2 <- read_csv(file.path(project_data, paste0('99_forest_output', "_2019a2-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2019b <- read_csv(file.path(project_data, paste0('99_forest_output', "_2019b-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2019b2 <- read_csv(file.path(project_data, paste0('99_forest_output', "_2019b2-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2019 <- bind_rows(sentiment_2019a, sentiment_2019a2, sentiment_2019b, sentiment_2019b2) %>%
  mutate(merge=row_number())

rm(sentiment_2019a)
rm(sentiment_2019a2)
rm(sentiment_2019b)
rm(sentiment_2019b2)

joined_2019 <- texts_sentence_2019 %>%
  left_join(sentiment_2019, by="merge") %>%
  mutate(label_predict_forest=as.numeric(str_sub(label_predict_forest, -1)) - 2) %>%
  mutate(year_2019=1) %>%
  select(invitationid, sentence_text, year_2019, label_predict_forest) %>%
  left_join(crosswalk_2019, by="invitationid") %>%
  left_join(applicants_2019, by="applicantid")

set.seed(83191)

sentiment_2019_subsample <- joined_2019 %>%
  filter(female_miss==0 & race_miss==0 & other_race==0 & school_other==0) %>%
  mutate(length=str_length(sentence_text)) %>%
  filter(length>=60 & length<=200) %>%
  group_by(female, white, black, asian, latinx, school_private, label_predict_forest) %>%
  slice_sample(n=5000) %>%
  ungroup() %>%
  select(-female_miss, -race_miss, -other_race, -school_other)

rm(sentiment_2019)
rm(joined_2019)
rm(applicants_2019)
rm(crosswalk_2019)
rm(texts_sentence_2019)

crosswalk_2018 <- read_fst(file.path(project_data, paste0(fileprefix, 'cw_all', filesuffix2, '.fst')))

applicants_2018 <- read_fst(file.path(project_data, paste0(fileprefix, 'applicants_cleaned', filesuffix2, '.fst'))) %>%
  select(applicantid, female, white, black, asian, latinx, school_private, female_miss, race_miss, other_race, school_other)

texts_sentence_2018a <- read_fst(file.path(project_data, paste0(fileprefix, 'texts_sentence', '_2018_21-10-29', '.fst')))

texts_sentence_2018b <- read_fst(file.path(project_data, paste0(fileprefix, 'texts_sentence', '_2018_21-10-29b', '.fst')))

texts_sentence_2018 <- bind_rows(texts_sentence_2018a, texts_sentence_2018b) %>%
  mutate(merge=row_number())

rm(texts_sentence_2018a)
rm(texts_sentence_2018b)

sentiment_2018a <- read_csv(file.path(project_data, paste0('99_forest_output', "_2018a-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2018a2 <- read_csv(file.path(project_data, paste0('99_forest_output', "_2018a2-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2018b <- read_csv(file.path(project_data, paste0('99_forest_output', "_2018b-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2018b2 <- read_csv(file.path(project_data, paste0('99_forest_output', "_2018b2-21-10-29", '.csv'))) %>%
  select(-X1, -index)

sentiment_2018 <- bind_rows(sentiment_2018a, sentiment_2018a2, sentiment_2018b, sentiment_2018b2) %>%
  mutate(merge=row_number())

rm(sentiment_2018a)
rm(sentiment_2018a2)
rm(sentiment_2018b)
rm(sentiment_2018b2)

joined_2018 <- texts_sentence_2018 %>%
  left_join(sentiment_2018, by="merge") %>%
  mutate(label_predict_forest=as.numeric(str_sub(label_predict_forest, -1)) - 2) %>%
  mutate(year_2019=0) %>%
  select(invitationid, sentence_text, year_2019, label_predict_forest) %>%
  left_join(crosswalk_2018, by="invitationid") %>%
  left_join(applicants_2018, by="applicantid")

set.seed(83191)

sentiment_2018_subsample <- joined_2018 %>%
  filter(female_miss==0 & race_miss==0 & other_race==0 & school_other==0) %>%
  mutate(length=str_length(sentence_text)) %>%
  filter(length>=60 & length<=200) %>%
  group_by(female, white, black, asian, latinx, school_private, label_predict_forest) %>%
  slice_sample(n=5000) %>%
  ungroup() %>%
  select(-female_miss, -race_miss, -other_race, -school_other)

rm(sentiment_2018)
rm(joined_2018)
rm(applicants_2018)
rm(crosswalk_2018)
rm(texts_sentence_2018)

set.seed(83191)

sentiment_subsample <- bind_rows(sentiment_2019_subsample, sentiment_2018_subsample) %>%
  group_by(female, white, black, asian, latinx, school_private, label_predict_forest, year_2019) %>%
  slice_sample(n=3) %>%
  ungroup() %>%
  mutate(random_order=rnorm(nrow(.))) %>%
  arrange(random_order) %>%
  select(-random_order)

write_csv(sentiment_subsample, file.path(project_data, "06e_sentiment_validation_key.csv"))

sentiment_set_1 <- sentiment_subsample %>%
  slice_head(n=100) %>%
  select(sentence_text) %>%
  mutate(sentiment_rating="")

sentiment_set_2 <- sentiment_subsample %>%
  slice_tail(n=(nrow(sentiment_subsample)-100)) %>%
  select(sentence_text) %>%
  mutate(sentiment_rating="")

set.seed(83191)

initials <- c("BK", "EA", "DP", "HM", "KO", "JM", "RD", "SM")

for (i in initials) {
  temp_1 <- sentiment_set_1 %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)
  
  temp_2 <- sentiment_set_2 %>%
    mutate(random_order=rnorm(nrow(.))) %>%
    arrange(random_order) %>%
    select(-random_order)
  
  write.xlsx(temp_1, file=file.path(project_data, paste0("06e_sentiment_validation_", i, "a.xlsx")))
  write.xlsx(temp_2, file=file.path(project_data, paste0("06e_sentiment_validation_", i, "b.xlsx")))
}



