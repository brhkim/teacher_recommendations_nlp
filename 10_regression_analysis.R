################################################################################
#  
#   Name: 10_regression_analysis.R
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

fileprefix <- "10_"

sentiment_sentence <- read_fst(path=file.path(project_data, paste0('08_', dataset_group, '_sentiment_analytic_data_sentence', filesuffix, '.fst')))
analytic_data <- read_fst(path=file.path(project_data, paste0('08_', dataset_group, '_analytic_data', filesuffix, '.fst')))
analytic_data_inst <- read_fst(path=file.path(project_data, paste0('08_', dataset_group, '_analytic_data_inst', filesuffix, '.fst')))


################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################

# Run basic descriptives:
subchain <- . %>%
  summarize(group="All", letters=n(), 
            sentences=mean(total_count),
            sentiment_pos=mean(sentiment_pos), sentiment_neu=mean(sentiment_neu),
            sentiment_neg=mean(sentiment_neg), avg_sent=mean(avg_sent))

descriptives <- analytic_data %>%
  subchain()

descriptives[2,] <- analytic_data %>%
  filter(female==0) %>%
  subchain()

descriptives[3,] <- analytic_data %>%
  filter(female==1) %>%
  subchain()

descriptives[4,] <- analytic_data %>%
  filter(white==1) %>%
  subchain()

descriptives[5,] <- analytic_data %>%
  filter(black==1) %>%
  subchain()

descriptives[6,] <- analytic_data %>%
  filter(asian==1) %>%
  subchain()

descriptives[7,] <- analytic_data %>%
  filter(latinx==1) %>%
  subchain()

descriptives[8,] <- analytic_data %>%
  filter(other_race==1) %>%
  subchain()

descriptives[9,] <- analytic_data %>%
  filter(international==0) %>%
  subchain()

descriptives[10,] <- analytic_data %>%
  filter(international==1) %>%
  subchain()

descriptives[11,] <- analytic_data %>%
  filter(firstgen==0) %>%
  subchain()

descriptives[12,] <- analytic_data %>%
  filter(firstgen==1) %>%
  subchain()

descriptives[13,] <- analytic_data %>%
  filter(school_public==1) %>%
  subchain()

descriptives[14,] <- analytic_data %>%
  filter(school_private==1) %>%
  subchain()

descriptives$group <- c("All", "Male", "Female", "White", "Black", "Asian", "Latinx", "Other", 
                        "Domestic", "International", "Non-First-gen", "First-gen", "Public", "Private")

descriptives <- descriptives %>%
  relocate(group, everything()) 

# Print out the descriptives table nicely
kable(descriptives, digits=c(0, 0, 2, 2, 2, 2, 4, 2), col.names = c("Group", "Total Letters", "Total Sentences",
                                                                     "Positive Sentences", "Neutral Sentences",
                                                                     "Negative Sentences", "Average Sentiment"), 
      align = "l", format="html", table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Overall", 1, 1) %>%
  pack_rows("Gender", 2, 3) %>%
  pack_rows("Race/Ethnicity", 4, 8) %>%
  pack_rows("Country", 9, 10) %>%
  pack_rows("First-Gen Status", 11, 12) %>%
  pack_rows("School Type", 13, 14) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_descriptives.jpg")), zoom=3, bs_theme="default")


subchain <- . %>%
  summarize(letters=n(), 
            tokens=mean(tokens),
            across(starts_with("st_") & !ends_with("_50") & !ends_with("_80"), mean),
            across(ends_with("_50"), mean),
            across(ends_with("_80"), mean))

# Run basic descriptives:

descriptives <- analytic_data %>%
  subchain()

descriptives[2,] <- analytic_data %>%
  filter(female==0) %>%
  subchain()

descriptives[3,] <- analytic_data %>%
  filter(female==1) %>%
  subchain()

descriptives[4,] <- analytic_data %>%
  filter(white==1) %>%
  subchain()

descriptives[5,] <- analytic_data %>%
  filter(black==1) %>%
  subchain()

descriptives[6,] <- analytic_data %>%
  filter(asian==1) %>%
  subchain()

descriptives[7,] <- analytic_data %>%
  filter(latinx==1) %>%
  subchain()

descriptives[8,] <- analytic_data %>%
  filter(other_race==1) %>%
  subchain()

descriptives[9,] <- analytic_data %>%
  filter(international==0) %>%
  subchain()

descriptives[10,] <- analytic_data %>%
  filter(international==1) %>%
  subchain()

descriptives[11,] <- analytic_data %>%
  filter(firstgen==0) %>%
  subchain()

descriptives[12,] <- analytic_data %>%
  filter(firstgen==1) %>%
  subchain()

descriptives[13,] <- analytic_data %>%
  filter(school_public==1) %>%
  subchain()

descriptives[14,] <- analytic_data %>%
  filter(school_private==1) %>%
  subchain()

descriptives$group <- c("All", "Male", "Female", "White", "Black", "Asian", "Latinx", "Other", 
                        "Domestic", "International", "Non-First-gen", "First-gen", "Public", "Private")

descriptives <- descriptives %>%
  relocate(group, everything()) %>%
  select(group, letters, tokens, ends_with("_80")) %>%
  rowwise() %>%
  mutate(across(ends_with("_80"), ~ scales::percent(., accuracy=0.1)))

colnames_replace <- data.frame(colnames=names(descriptives)) %>%
  mutate(colnames=str_replace(colnames, "st_", "")) %>%
  mutate(colnames=str_replace(colnames, "_80", "")) %>%
  mutate(colnames=case_when(
    colnames=="group" ~ "Group",
    colnames=="letters" ~ "Letters",
    colnames=="tokens" ~ "Keywords",
    TRUE ~ colnames
  ))

# Print out the descriptives table nicely
kable(descriptives, digits=c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), col.names = colnames_replace$colnames, 
      align = "l", format="html", table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Overall", 1, 1) %>%
  pack_rows("Gender", 2, 3) %>%
  pack_rows("Race/Ethnicity", 4, 8) %>%
  pack_rows("Country", 9, 10) %>%
  pack_rows("First-Gen Status", 11, 12) %>%
  pack_rows("School Type", 13, 14)  %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_descriptives.jpg")), zoom=3)


sentiment_outcomes <- c("sentiment_pos", "sentiment_neu", "sentiment_neg", "avg_sent")
sentiment_averages <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_append <- data.frame(outcome=sentiment_outcome, average= analytic_data[[sentiment_outcome]] %>% mean() )
  sentiment_averages <- bind_rows(sentiment_averages, sentiment_averages_append)
  i <- i + 1
}

write.csv(sentiment_averages, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_averages.csv")))

sentiment_averages_inst <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_inst_append <- data.frame(outcome=sentiment_outcome, average= analytic_data_inst[[sentiment_outcome]] %>% mean() )
  sentiment_averages_inst <- bind_rows(sentiment_averages_inst, sentiment_averages_inst_append)
  i <- i + 1
}

write.csv(sentiment_averages_inst, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_averages_inst.csv")))




# Run regressions!

allout_sentiment0 <- feols(data=analytic_data, fml=c(sentiment_pos, sentiment_neu, sentiment_neg, avg_sent) ~ 
                             
                             year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                             race_miss + black + asian + latinx + other_race +
                             firstgen + firstgen_miss +
                             senior + multischool + multischool_miss + feewaiver + total_count, 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment0, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_demosonly.csv")))


allout_sentiment1 <- feols(data=analytic_data, fml=c(sentiment_pos, sentiment_neu, sentiment_neg, avg_sent) ~ 
                             
                             year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                             race_miss + black + asian + latinx + other_race +
                             firstgen + firstgen_miss +
                             senior + multischool + multischool_miss + feewaiver + total_count +
                             
                             factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                             factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                             ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                             factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                             factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                             
                             any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                             leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                             excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                             total_leadership + total_excellence + total_mentorship +
                             
                             factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                             
                             school_private + school_other +
                             factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                             factor(years_taught) + gradestaught_other + coach, 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment1, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_landscape.csv")))

allout_sentiment2 <- feols(data=analytic_data, fml=c(sentiment_pos, sentiment_neu, sentiment_neg, avg_sent) ~ 
                             
                             year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                             race_miss + black + asian + latinx + other_race +
                             firstgen + firstgen_miss +
                             senior + multischool + multischool_miss + feewaiver + total_count +
                             
                             factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                             factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                             ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                             factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                             factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                             
                             any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                             leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                             excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                             total_leadership + total_excellence + total_mentorship +
                             
                             factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                             
                             school_private + school_other +
                             factor(subject) + 
                             factor(years_taught) + gradestaught_other + coach | 
                             factor(recommenderid), 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_teacher.csv")))


allout_sentiment3 <- feols(data=analytic_data_inst, fml=c(sentiment_pos, sentiment_neu, sentiment_neg, avg_sent) ~ 
                             
                             year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                             race_miss + black + asian + latinx + other_race +
                             firstgen + firstgen_miss +
                             senior + multischool + multischool_miss + feewaiver + total_count +
                             
                             factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                             factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                             ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                             factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                             factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                             
                             any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                             leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                             excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                             total_leadership + total_excellence + total_mentorship +
                             
                             factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                             
                             school_private + school_other +
                             factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                             factor(years_taught) + gradestaught_other + coach | 
                             factor(member_idmasked), 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_institution.csv")))

rm(sentiment_sentence, allout_sentiment0, allout_sentiment1,
   allout_sentiment2, allout_sentiment3, colnames_replace, descriptives)

supertopic_list <- analytic_data %>%
  select(starts_with("st_") & !ends_with("_80") & !ends_with("_50")) %>%
  names()

supertopic_cutpoints <- data.frame()
i <- 1
for (supertopic in supertopic_list) {
  supertopic_80 <- paste0(supertopic, "_80")
  supertopic_cutpoints_append <- data.frame(supertopic=supertopic, cutpoint=analytic_data[[supertopic]][analytic_data[[supertopic_80]]==1] %>% min() )
  supertopic_cutpoints <- bind_rows(supertopic_cutpoints, supertopic_cutpoints_append)
  i <- i + 1
}

write.csv(supertopic_cutpoints, file=file.path(project_output, paste0(fileprefix, dataset_group, "_supertopic_cutpoints.csv")))


# Topic Modeling Regressions
allout_topic0 <- feols(data=analytic_data, fml=c(st_academic_excellence_80, st_advanced_coursetaking_80, st_character_excellence_80,
                                                 st_community_engagement_80, st_extracurriculars_80, st_formalities_80,
                                                 st_future_success_80, st_humanities_80, st_intellectual_80, st_leadership_80,
                                                 st_sports_80, st_stem_80, st_time_80) ~ 
                         
                         year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                         race_miss + black + asian + latinx + other_race +
                         firstgen + firstgen_miss +
                         senior + multischool + multischool_miss + feewaiver + tokens, 
                       cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_topic0, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_demosonly.csv")))


allout_topic1 <- feols(data=analytic_data, fml=c(st_academic_excellence_80, st_advanced_coursetaking_80, st_character_excellence_80,
                                                 st_community_engagement_80, st_extracurriculars_80, st_formalities_80,
                                                 st_future_success_80, st_humanities_80, st_intellectual_80, st_leadership_80,
                                                 st_sports_80, st_stem_80, st_time_80) ~ 
                         
                         year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                         race_miss + black + asian + latinx + other_race +
                         firstgen + firstgen_miss +
                         senior + multischool + multischool_miss + feewaiver + tokens +
                         
                         factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                         factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                         ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                         factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                         factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                         
                         any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                         leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                         excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                         total_leadership + total_excellence + total_mentorship +
                         
                         factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                         
                         school_private + school_other +
                         factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                         factor(years_taught) + gradestaught_other + coach, 
                       cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_topic1, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_landscape.csv")))

allout_topic2 <- feols(data=analytic_data, fml=c(st_academic_excellence_80, st_advanced_coursetaking_80, st_character_excellence_80,
                                                 st_community_engagement_80, st_extracurriculars_80, st_formalities_80,
                                                 st_future_success_80, st_humanities_80, st_intellectual_80, st_leadership_80,
                                                 st_sports_80, st_stem_80, st_time_80) ~ 
                         
                         year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                         race_miss + black + asian + latinx + other_race +
                         firstgen + firstgen_miss +
                         senior + multischool + multischool_miss + feewaiver + tokens +
                         
                         factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                         factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                         ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                         factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                         factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                         
                         any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                         leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                         excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                         total_leadership + total_excellence + total_mentorship +
                         
                         factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                         
                         school_private + school_other +
                         factor(subject) + 
                         factor(years_taught) + gradestaught_other + coach| 
                         factor(recommenderid), 
                       cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_topic2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_teacher.csv")))


allout_topic3a <- feols(data=analytic_data_inst, fml=c(st_academic_excellence_80, st_advanced_coursetaking_80, st_character_excellence_80,
                                                       st_community_engagement_80) ~ 
                          
                          year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                          race_miss + black + asian + latinx + other_race +
                          firstgen + firstgen_miss +
                          senior + multischool + multischool_miss + feewaiver + tokens +
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                          factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                          
                          school_private + school_other +
                          factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                          factor(years_taught) + gradestaught_other + coach | 
                          factor(member_idmasked), 
                        cluster=~factor(recommenderid)) %>%
  etable(digits="r3")


allout_topic3b <- feols(data=analytic_data_inst, fml=c(st_extracurriculars_80, st_formalities_80, st_future_success_80, 
                                                       st_humanities_80) ~ 
                          
                          year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                          race_miss + black + asian + latinx + other_race +
                          firstgen + firstgen_miss +
                          senior + multischool + multischool_miss + feewaiver + tokens +
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                          factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                          
                          school_private + school_other +
                          factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                          factor(years_taught) + gradestaught_other + coach | 
                          factor(member_idmasked), 
                        cluster=~factor(recommenderid)) %>%
  etable(digits="r3")


allout_topic3c <- feols(data=analytic_data_inst, fml=c(st_intellectual_80, st_leadership_80,
                                                       st_sports_80, st_stem_80, st_time_80) ~ 
                          
                          year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                          race_miss + black + asian + latinx + other_race +
                          firstgen + firstgen_miss +
                          senior + multischool + multischool_miss + feewaiver + tokens +
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + factor(math_pctile_bucket) + 
                          factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + factor(app_selectivity_quintile) + 
                          
                          school_private + school_other +
                          factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                          factor(years_taught) + gradestaught_other + coach | 
                          factor(member_idmasked), 
                        cluster=~factor(recommenderid)) %>%
  etable(digits="r3")


allout_topic3 <- left_join(allout_topic3a %>% mutate(rownames=rownames(.)),
                           allout_topic3b %>% mutate(rownames=rownames(.)),
                           by="rownames") %>%
  left_join(allout_topic3c %>% mutate(rownames=rownames(.)), by="rownames")
rownames(allout_topic3) <- allout_topic3$rownames
allout_topic3 <- allout_topic3 %>%
  select(-rownames)

write.csv(allout_topic3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_institution.csv")))

rm(allout_topic0, allout_topic1, allout_topic2, allout_topic3,
   analytic_data, analytic_data_inst,
   subchain, allout_topic3a, allout_topic3b, allout_topic3c)

