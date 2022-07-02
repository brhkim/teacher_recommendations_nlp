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

fileprefix <- "10c_"

analytic_data_all <- read_fst(path=file.path(project_data, paste0('08_', dataset_group, '_analytic_data', filesuffix, '.fst')))
inst_selectivity <- read_fst(path=file.path(project_data, paste0('01_', dataset_group, '_applications', filesuffix, '.fst'))) %>%
  select(member_idmasked, season, SAT_Bin, ACT_Bin) %>%
  group_by(member_idmasked) %>%
  arrange(desc(season)) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(-season) %>%
  mutate(
    test_bin=case_when(
      SAT_Bin=="1501-1600" ~ 1550,
      ACT_Bin=="33-36" ~ 1525,
      ACT_Bin=="31-32" ~ 1415,
      ACT_Bin=="29-30" ~ 1355,
      ACT_Bin=="27-28" ~ 1290,
      ACT_Bin=="26-27" ~ 1260,
      ACT_Bin=="24-25" ~ 1190,
      ACT_Bin=="22-23" ~ 1125,
      ACT_Bin=="20-21" ~ 1060,
      ACT_Bin=="18-19" ~ 990,
      ACT_Bin=="<= 17" ~ 900,
      SAT_Bin=="1401-1500" ~ 1450,
      SAT_Bin=="1301-1400" ~ 1350,
      SAT_Bin=="1201-1300" ~ 1250,
      SAT_Bin=="1101-1200" ~ 1150,
      SAT_Bin=="1001-1100" ~ 1050,
      SAT_Bin=="<=1000" ~ 900),
    test_miss=case_when(
      is.na(test_bin) ~ 1,
      TRUE ~ 0),
    satbin_decile=ntile(test_bin, 10),
    satbin_decile=case_when(
      is.na(satbin_decile) ~ "Missing",
      !is.na(satbin_decile) ~ as.character(satbin_decile)),
    satbin_decile=factor(satbin_decile,
                           levels=c("Missing", 1:10)))

analytic_data_inst_all <- read_fst(path=file.path(project_data, paste0('08_', dataset_group, '_analytic_data_inst', filesuffix, '.fst'))) %>%
  left_join(inst_selectivity, by="member_idmasked")


################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################



vars_of_interest_a <- c("female", "black", "asian",
                        "Observations", "R2", "Adj. R2")
var_labels_a <- c("Female", "Black", "Asian",
                  "Observations", "R^2", "Adj. R^2")
vars_of_interest_b <- c("female", "black", "asian", 
                        "Observations", "R2", "Within R2")
var_labels_b <- c("Female", "Black", "Asian", 
                  "Observations", "R^2", "Within R^2")
sentiment_models <- c("Variable", "Positive",
                      "Neutral", "Negative", "Mean Sent.")
topic_models <- c("Variable", "Academic Excellence", "Advanced Course- taking", "Character Excellence",
                  "Community Engage- ment", "Extra- currics.", "Formalities", "Future Potential",
                  "Humanities", "Intellectual Promise", "Leadership", "Sports", "STEM", "Time and Life Mgmt")

process_sentiment_averages <- . %>%
  .$average %>%
  round(2) %>%
  as.character() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  mutate(`Variable`="Sample Mean") %>%
  relocate(`Variable`)

analytic_data <- analytic_data_all %>%
  filter(app_selectivity_quintile==5)

sentiment_outcomes <- c("sentiment_pos", "sentiment_neu", "sentiment_neg", "avg_sent")
sentiment_averages <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_append <- data.frame(outcome=sentiment_outcome, average= analytic_data[[sentiment_outcome]] %>% mean() )
  sentiment_averages <- bind_rows(sentiment_averages, sentiment_averages_append)
  i <- i + 1
}
sentiment_averages <- sentiment_averages %>%
  process_sentiment_averages()

names(sentiment_averages) <- sentiment_models

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
                             
                             factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                             
                             school_private + school_other +
                             factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                             factor(years_taught) + gradestaught_other + coach, 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment1, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_landscape_studhi.csv")))

process_sentiment_table <- . %>%
  mutate(dependentvar=rownames(.)) %>%
  relocate(dependentvar) %>%
  slice(-(1:2)) %>%
  filter(dependentvar %in% c(vars_of_interest_a, vars_of_interest_b))

sentiment_landscape <- allout_sentiment1 %>%
  process_sentiment_table()
names(sentiment_landscape) <- sentiment_models
for (i in 1:length(vars_of_interest_a)) {
  sentiment_landscape$`Variable`[sentiment_landscape$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
sentiment_landscape <- bind_rows(sentiment_averages, sentiment_landscape)

sentiment_landscape_studhi <- sentiment_landscape

kable(sentiment_landscape_studhi, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_landscape_studhi.jpg")), zoom=3, bs_theme="default")


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
                         
                         factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                         
                         school_private + school_other +
                         factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                         factor(years_taught) + gradestaught_other + coach, 
                       cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_topic1, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_landscape_studhi.csv")))

supertopic_cutpoints <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_supertopic_cutpoints.csv'))) %>%
#supertopic_cutpoints <- read_csv(file=file.path(project_output, paste0('10_', "test", '_supertopic_cutpoints.csv'))) %>%
    filter(supertopic!="st_other") %>%
  .$cutpoint %>%
  round(2) %>%
  as.character() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  mutate(`Variable`="80th Pctile") %>%
  relocate(`Variable`)

names(supertopic_cutpoints) <- topic_models 

process_topic_table <- . %>%
  mutate(dependentvar=rownames(.)) %>%
  relocate(dependentvar) %>%
  slice(-(1:2)) %>%
  filter(dependentvar %in% c(vars_of_interest_a, vars_of_interest_b))


topic_landscape <- allout_topic1  %>%
  process_topic_table
names(topic_landscape) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_landscape$`Variable`[topic_landscape$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_landscape <- bind_rows(supertopic_cutpoints, topic_landscape)

topic_landscape_studhi <- topic_landscape

kable(topic_landscape_studhi, align = c("r", "c", "c", "c", "c",
                                 "c", "c", "c", "c",
                                 "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_landscape_studhi.jpg")), zoom=3, bs_theme="default")






analytic_data <- analytic_data_all %>%
  filter(subject=="Computer Science" | subject=="Math" | subject=="Science")

sentiment_averages <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_append <- data.frame(outcome=sentiment_outcome, average= analytic_data[[sentiment_outcome]] %>% mean() )
  sentiment_averages <- bind_rows(sentiment_averages, sentiment_averages_append)
  i <- i + 1
}
sentiment_averages <- sentiment_averages %>%
  process_sentiment_averages()

names(sentiment_averages) <- sentiment_models

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

write.csv(allout_sentiment2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_teacher_stem.csv")))

sentiment_teacher <- allout_sentiment2 %>%
  process_sentiment_table()
names(sentiment_teacher) <- sentiment_models
for (i in 1:length(vars_of_interest_a)) {
  sentiment_teacher$`Variable`[sentiment_teacher$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
sentiment_teacher <- bind_rows(sentiment_averages, sentiment_teacher)

sentiment_teacher_stem <- sentiment_teacher

kable(sentiment_teacher_stem, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_teacher_stem.jpg")), zoom=3, bs_theme="default")


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

write.csv(allout_topic2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_teacher_stem.csv")))


topic_teacher <- allout_topic2  %>%
  process_topic_table
names(topic_teacher) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_teacher$`Variable`[topic_teacher$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_teacher <- bind_rows(supertopic_cutpoints, topic_teacher)

topic_teacher_stem <- topic_teacher

kable(topic_teacher_stem, align = c("r", "c", "c", "c", "c",
                                        "c", "c", "c", "c",
                                        "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_teacher_stem.jpg")), zoom=3, bs_theme="default")






analytic_data <- analytic_data_all %>%
  filter(subject=="English" | subject=="Social Studies")

sentiment_averages <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_append <- data.frame(outcome=sentiment_outcome, average= analytic_data[[sentiment_outcome]] %>% mean() )
  sentiment_averages <- bind_rows(sentiment_averages, sentiment_averages_append)
  i <- i + 1
}
sentiment_averages <- sentiment_averages %>%
  process_sentiment_averages()

names(sentiment_averages) <- sentiment_models

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

write.csv(allout_sentiment2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_teacher_hum.csv")))

sentiment_teacher <- allout_sentiment2 %>%
  process_sentiment_table()
names(sentiment_teacher) <- sentiment_models
for (i in 1:length(vars_of_interest_a)) {
  sentiment_teacher$`Variable`[sentiment_teacher$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
sentiment_teacher <- bind_rows(sentiment_averages, sentiment_teacher)

sentiment_teacher_hum <- sentiment_teacher

kable(sentiment_teacher_hum, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_teacher_hum.jpg")), zoom=3, bs_theme="default")


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

write.csv(allout_topic2, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_teacher_hum.csv")))


topic_teacher <- allout_topic2  %>%
  process_topic_table
names(topic_teacher) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_teacher$`Variable`[topic_teacher$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_teacher <- bind_rows(supertopic_cutpoints, topic_teacher)

topic_teacher_hum <- topic_teacher

kable(topic_teacher_hum, align = c("r", "c", "c", "c", "c",
                                    "c", "c", "c", "c",
                                    "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_teacher_hum.jpg")), zoom=3, bs_theme="default")








analytic_data_inst <- analytic_data_inst_all %>%
  filter(satbin_decile=="10")

sentiment_averages_inst <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_inst_append <- data.frame(outcome=sentiment_outcome, average= analytic_data_inst[[sentiment_outcome]] %>% mean() )
  sentiment_averages_inst <- bind_rows(sentiment_averages_inst, sentiment_averages_inst_append)
  i <- i + 1
}
sentiment_averages_inst <- sentiment_averages_inst %>%
  process_sentiment_averages()
names(sentiment_averages_inst) <- sentiment_models

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

write.csv(allout_sentiment3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_institution_topdec.csv")))

sentiment_institution <- allout_sentiment3 %>%
  process_sentiment_table()
names(sentiment_institution) <- sentiment_models
for (i in 1:length(vars_of_interest_b)) {
  sentiment_institution$`Variable`[sentiment_institution$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
sentiment_institution <- bind_rows(sentiment_averages_inst, sentiment_institution)

sentiment_institution_topdec <- sentiment_institution

kable(sentiment_institution_topdec, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_institution_topdec.jpg")), zoom=3, bs_theme="default")


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

write.csv(allout_topic3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_institution_topdec.csv")))

topic_institution <- allout_topic3  %>%
  process_topic_table
names(topic_institution) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_institution$`Variable`[topic_institution$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_institution <- bind_rows(supertopic_cutpoints, topic_institution)

topic_institution_topdec <- topic_institution

kable(topic_institution_topdec, align = c("r", "c", "c", "c", "c",
                                    "c", "c", "c", "c",
                                    "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_institution_topdec.jpg")), zoom=3, bs_theme="default")







analytic_data_inst <- analytic_data_inst_all %>%
  filter(satbin_decile=="10" & math_pctile_bucket==">=95" &
           verb_pctile_bucket==">=95" & app_selectivity_quintile=="5")

sentiment_averages_inst <- data.frame()
i <- 1
for (sentiment_outcome in sentiment_outcomes) {
  sentiment_averages_inst_append <- data.frame(outcome=sentiment_outcome, average= analytic_data_inst[[sentiment_outcome]] %>% mean() )
  sentiment_averages_inst <- bind_rows(sentiment_averages_inst, sentiment_averages_inst_append)
  i <- i + 1
}
sentiment_averages_inst <- sentiment_averages_inst %>%
  process_sentiment_averages()
names(sentiment_averages_inst) <- sentiment_models

allout_sentiment3 <- feols(data=analytic_data_inst, fml=c(sentiment_pos, sentiment_neu, sentiment_neg, avg_sent) ~ 
                             
                             year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                             race_miss + black + asian + latinx + other_race +
                             firstgen + firstgen_miss +
                             senior + multischool + multischool_miss + feewaiver + total_count +
                             
                             factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + # factor(math_pctile_bucket) + 
                             #factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                             factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                             ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                             factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                             factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                             
                             any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                             leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                             excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                             total_leadership + total_excellence + total_mentorship +
                             
                             factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                             
                             school_private + school_other +
                             factor(letters_lasttwo_group) + factor(letters_current_group) + factor(subject) + 
                             factor(years_taught) + gradestaught_other + coach | 
                             factor(member_idmasked), 
                           cluster=~factor(recommenderid)) %>%
  etable(digits="r3")

write.csv(allout_sentiment3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_regressions_institution_elite.csv")))

sentiment_institution <- allout_sentiment3 %>%
  process_sentiment_table()
names(sentiment_institution) <- sentiment_models
for (i in 1:length(vars_of_interest_b)) {
  sentiment_institution$`Variable`[sentiment_institution$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
sentiment_institution <- bind_rows(sentiment_averages_inst, sentiment_institution)

sentiment_institution_topdec <- sentiment_institution

kable(sentiment_institution_topdec, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_institution_elite.jpg")), zoom=3, bs_theme="default")


allout_topic3a <- feols(data=analytic_data_inst, fml=c(st_academic_excellence_80, st_advanced_coursetaking_80, st_character_excellence_80,
                                                       st_community_engagement_80) ~ 
                          
                          year_2019 + female + female_miss + age_u17 + age_a17 + international + 
                          race_miss + black + asian + latinx + other_race +
                          firstgen + firstgen_miss +
                          senior + multischool + multischool_miss + feewaiver + tokens +
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + #factor(math_pctile_bucket) + 
                          #factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                          
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
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + #factor(math_pctile_bucket) + 
                          #factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                          
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
                          
                          factor(quintile_rank) + factor(gpa_bucket) + factor(toefl_percentile) + #factor(math_pctile_bucket) + 
                          #factor(verb_pctile_bucket) + factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          factor(total_main_tests_bucket) + sat_subjects_taken + factor(avg_sat_subject_bucket) +
                          ap_tests_english + ap_tests_languages + ap_tests_socialstudies + ap_tests_stem + ap_tests_arts + 
                          factor(score_avg_english) + factor(score_avg_languages) + factor(score_avg_socialstudies) + 
                          factor(score_avg_stem) + factor(score_avg_arts) + any_ib +
                          
                          any_academic + any_career + any_arts + any_other + any_service + any_athletics +
                          leadership_academic + leadership_career + leadership_arts + leadership_other + leadership_service + leadership_athletics +
                          excellence_academic + excellence_career + excellence_arts + excellence_other + excellence_service + excellence_athletics +
                          total_leadership + total_excellence + total_mentorship +
                          
                          factor(total_apps) + any_early + early_only + #factor(app_selectivity_quintile) + 
                          
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

write.csv(allout_topic3, file=file.path(project_output, paste0(fileprefix, dataset_group, "_topic_regressions_institution_elite.csv")))

topic_institution <- allout_topic3  %>%
  process_topic_table
names(topic_institution) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_institution$`Variable`[topic_institution$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_institution <- bind_rows(supertopic_cutpoints, topic_institution)

topic_institution_topdec <- topic_institution

kable(topic_institution_topdec, align = c("r", "c", "c", "c", "c",
                                          "c", "c", "c", "c",
                                          "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_institution_elite.jpg")), zoom=3, bs_theme="default")
