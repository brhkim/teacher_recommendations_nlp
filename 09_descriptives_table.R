################################################################################
#  
#   Name: 09_descriptives_table.R
#   
#   Purpose: Use the analytic dataset to conduct basic descriptive analysis of
#   the training and test samples. Requires both analytic datasets to be
#   completed through this point.
#
################################################################################

################################################################################
#
#   #import - Load in a datasets
#
################################################################################

train_sentiment_sentence %>%
  select(label_predict_forest, sentence_text) %>%
  group_by(label_predict_forest) %>%
  slice_sample(n=10) %>%
  View()
  

fileprefix <- "09_"

train_sentiment_sentence <- read_fst(path=file.path(project_data, paste0('08_train_sentiment_analytic_data_sentence', filesuffix, '.fst')))
train_analytic_data <- read_fst(path=file.path(project_data, paste0('08_train_analytic_data', filesuffix, '.fst')))

test_sentiment_sentence <- read_fst(path=file.path(project_data, paste0('08_test_sentiment_analytic_data_sentence', filesuffix, '.fst')))
test_analytic_data <- read_fst(path=file.path(project_data, paste0('08_test_analytic_data', filesuffix, '.fst')))


################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################

individual_level <- test_analytic_data %>%
  group_by(applicantid) %>%
  mutate(total_letters=n()) %>%
  slice_head(n=1) %>%
  ungroup()

individual_level_train <- train_analytic_data %>%
  group_by(applicantid) %>%
  mutate(total_letters=n()) %>%
  slice_head(n=1) %>%
  ungroup()

individual_level_2018 <- individual_level %>%
  filter(year_2019==0)

individual_level_2019 <- individual_level %>%
  filter(year_2019==1)

sample_descriptives <- . %>%
  summarize(Students=n_distinct(applicantid),
            Letters=sum(total_letters),
            Female=mean(female),
            `First Generation`=mean(firstgen),
            International=mean(international),
            `Fee Waiver Recipient`=mean(feewaiver),
            White=mean(white),
            Black=mean(black),
            Latinx=mean(latinx),
            Asian=mean(asian),
            Other=mean(other_race),
            Missing=round(mean(race_miss), 3),
            `Public School`=mean(school_public),
            `Private School`=mean(school_private),
            `Other School`=mean(school_other),
            
            total_apps_a=length(.$total_apps[.$total_apps=="1-3"])/length(.$applicantid),
            total_apps_b=length(.$total_apps[.$total_apps=="4-7"])/length(.$applicantid),
            total_apps_c=length(.$total_apps[.$total_apps==">=8"])/length(.$applicantid),
            
            gpa_bucket_a=length(.$gpa_bucket[.$gpa_bucket=="Other/Missing"])/length(.$applicantid),
            gpa_bucket_b=length(.$gpa_bucket[.$gpa_bucket=="<0.90"])/length(.$applicantid),
            gpa_bucket_c=length(.$gpa_bucket[.$gpa_bucket=="0.90-0.99"])/length(.$applicantid),
            gpa_bucket_d=length(.$gpa_bucket[.$gpa_bucket=="1.00"])/length(.$applicantid),
            gpa_bucket_e=length(.$gpa_bucket[.$gpa_bucket==">1.00"])/length(.$applicantid),
            
            math_bucket_a=length(.$math_pctile_bucket[.$math_pctile_bucket=="Missing"])/length(.$applicantid),
            math_bucket_b=length(.$math_pctile_bucket[.$math_pctile_bucket=="<75"])/length(.$applicantid),
            math_bucket_c=length(.$math_pctile_bucket[.$math_pctile_bucket=="75-89"])/length(.$applicantid),
            math_bucket_d=length(.$math_pctile_bucket[.$math_pctile_bucket=="90-94"])/length(.$applicantid),
            math_bucket_e=length(.$math_pctile_bucket[.$math_pctile_bucket==">=95"])/length(.$applicantid),
            
            verb_bucket_a=length(.$verb_pctile_bucket[.$verb_pctile_bucket=="Missing"])/length(.$applicantid),
            verb_bucket_b=length(.$verb_pctile_bucket[.$verb_pctile_bucket=="<75"])/length(.$applicantid),
            verb_bucket_c=length(.$verb_pctile_bucket[.$verb_pctile_bucket=="75-89"])/length(.$applicantid),
            verb_bucket_d=length(.$verb_pctile_bucket[.$verb_pctile_bucket=="90-94"])/length(.$applicantid),
            verb_bucket_e=length(.$verb_pctile_bucket[.$verb_pctile_bucket==">=95"])/length(.$applicantid)
            
  ) %>%
  mutate(across(c(Female, `First Generation`, White, Black, Latinx, Asian, Other, International,
                  `Fee Waiver Recipient`, `Public School`, `Private School`, `Other School`), round, 3)) %>%
  mutate(across(starts_with("total_apps"), round, 3)) %>%
  mutate(across(starts_with("gpa_bucket"), round, 3)) %>%
  mutate(across(starts_with("math_bucket"), round, 3)) %>%
  mutate(across(starts_with("verb_bucket"), round, 3)) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  stack() %>%
  relocate(ind, values) %>%
  rename(Variable=ind, Values=values) %>%
  mutate(Variable=case_when(
    Variable=="total_apps_a" ~ "1-3",
    Variable=="total_apps_b" ~ "4-7",
    Variable=="total_apps_c" ~ ">=8",
    Variable=="gpa_bucket_a" ~ "Other/Missing",
    Variable=="gpa_bucket_b" ~ "<0.90",
    Variable=="gpa_bucket_c" ~ "0.90-0.99",
    Variable=="gpa_bucket_d" ~ "1.00",
    Variable=="gpa_bucket_e" ~ ">1.00",
    Variable=="math_bucket_a" ~ "Missing",
    Variable=="math_bucket_b" ~ "<75",
    Variable=="math_bucket_c" ~ "75-89",
    Variable=="math_bucket_d" ~ "90-94",
    Variable=="math_bucket_e" ~ ">=95",
    Variable=="verb_bucket_a" ~ "Missing",
    Variable=="verb_bucket_b" ~ "<75",
    Variable=="verb_bucket_c" ~ "75-89",
    Variable=="verb_bucket_d" ~ "90-94",
    Variable=="verb_bucket_e" ~ ">=95",
    
    TRUE ~ as.character(Variable)
  ))

descriptives_2018_prep <- individual_level_2018 %>%
  sample_descriptives()
  
descriptives_2019_prep <- individual_level_2019 %>%
  sample_descriptives()

descriptives_pooled_prep <- individual_level %>%
  sample_descriptives()

sample_descriptives_table <- bind_cols(descriptives_2018_prep, descriptives_2019_prep, descriptives_pooled_prep) %>%
  select(-Variable...3, -Variable...5) %>%
  rename(Variable=`Variable...1`, `2018`=Values...2, `2019`=Values...4, Pooled=Values...6)

kable(sample_descriptives_table[1:15,], 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'", background="white") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 2) %>%
  pack_rows("Student Demographics", 3, 6) %>%
  pack_rows("Student Race/Ethnicity", 7, 12) %>%
  pack_rows("Student School Sector", 13, 15) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "sample_descriptives_a", filesuffix, ".jpg")), zoom=2, bs_theme="default")

kable(sample_descriptives_table[16:33,], 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Applications Sent", 1, 3) %>%
  pack_rows("Scaled GPA Group", 4, 8) %>%
  pack_rows("Math SAT/ACT Percentile Group", 9, 13) %>%
  pack_rows("Verbal SAT/ACT Percentile Group", 14, 18) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "sample_descriptives_b", filesuffix, ".jpg")), zoom=2, bs_theme="default")



descriptives_pooled_prep_train <- individual_level_train %>%
  sample_descriptives()

sample_descriptives_table_traintest <- bind_cols(descriptives_pooled_prep_train, descriptives_pooled_prep) %>%
  select(-Variable...3) %>%
  rename(Variable=`Variable...1`, `Train Data`=Values...2, `Test Data`=Values...4)

kable(sample_descriptives_table_traintest[1:15,], 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'", background="white") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 2) %>%
  pack_rows("Student Demographics", 3, 6) %>%
  pack_rows("Student Race/Ethnicity", 7, 12) %>%
  pack_rows("Student School Sector", 13, 15) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "traintest_descriptives_a", filesuffix, ".jpg")), zoom=2, bs_theme="default")

kable(sample_descriptives_table_traintest[16:33,], 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Applications Sent", 1, 3) %>%
  pack_rows("Scaled GPA Group", 4, 8) %>%
  pack_rows("Math SAT/ACT Percentile Group", 9, 13) %>%
  pack_rows("Verbal SAT/ACT Percentile Group", 14, 18) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "traintest_descriptives_b", filesuffix, ".jpg")), zoom=2, bs_theme="default")


teacher_level <- test_analytic_data %>%
  select(recommenderid, year_2019, letters_current_group,
         letters_lasttwo_group) %>%
  distinct()

teacher_level_2018 <- teacher_level %>%
  filter(year_2019==0)

teacher_level_2019 <- teacher_level %>%
  filter(year_2019==1)

teacher_descriptives <- . %>%
  summarize(Teachers=n_distinct(recommenderid),
            current_bucket_a=length(.$letters_current_group[.$letters_current_group=="1"])/length(.$recommenderid),
            current_bucket_b=length(.$letters_current_group[.$letters_current_group=="2-5"])/length(.$recommenderid),
            current_bucket_c=length(.$letters_current_group[.$letters_current_group=="6-10"])/length(.$recommenderid),
            current_bucket_d=length(.$letters_current_group[.$letters_current_group=="11-25"])/length(.$recommenderid),
            current_bucket_e=length(.$letters_current_group[.$letters_current_group=="25+"])/length(.$recommenderid),
            
            lasttwo_bucket_a=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="0"])/length(.$recommenderid),
            lasttwo_bucket_b=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="1"])/length(.$recommenderid),
            lasttwo_bucket_c=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="2-5"])/length(.$recommenderid),
            lasttwo_bucket_d=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="6-10"])/length(.$recommenderid),
            lasttwo_bucket_e=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="11-25"])/length(.$recommenderid),
            lasttwo_bucket_f=length(.$letters_lasttwo_group[.$letters_lasttwo_group=="25+"])/length(.$recommenderid)
  ) %>%
  mutate(across(starts_with("lasttwo"), round, 3)) %>%
  mutate(across(starts_with("current"), round, 3)) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  stack() %>%
  relocate(ind, values) %>%
  rename(Variable=ind, Values=values) %>%
  mutate(Variable=case_when(
    Variable=="current_bucket_a" ~ "1",
    Variable=="current_bucket_b" ~ "2-5",
    Variable=="current_bucket_c" ~ "6-10",
    Variable=="current_bucket_d" ~ "11-25",
    Variable=="current_bucket_e" ~ "25+",
    
    Variable=="lasttwo_bucket_a" ~ "0",
    Variable=="lasttwo_bucket_b" ~ "1",
    Variable=="lasttwo_bucket_c" ~ "2-5",
    Variable=="lasttwo_bucket_d" ~ "6-10",
    Variable=="lasttwo_bucket_e" ~ "11-25",
    Variable=="lasttwo_bucket_f" ~ "25+",
    
    TRUE ~ as.character(Variable)
  ))

teacher_level_2018_prep <- teacher_level_2018 %>%
  teacher_descriptives()

teacher_level_2019_prep <- teacher_level_2019 %>%
  teacher_descriptives()

teacher_level_prep <- teacher_level %>%
  teacher_descriptives()

teacher_descriptives_table <- bind_cols(teacher_level_2018_prep, teacher_level_2019_prep, teacher_level_prep) %>%
  select(-Variable...3, -Variable...5) %>%
  rename(Variable=`Variable...1`, `2018`=Values...2, `2019`=Values...4, `Pooled`=Values...6)

kable(teacher_descriptives_table, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 1) %>%
  pack_rows("Letters Written in Current Year", 2, 6) %>%
  pack_rows("Letters Written in Past Two Years", 7, 12) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "teacher_descriptives", filesuffix, ".jpg")), zoom=2, bs_theme="default")

teacher_level_train <- train_analytic_data %>%
  select(recommenderid, year_2019, letters_current_group,
         letters_lasttwo_group) %>%
  distinct()

teacher_level_train_prep <- teacher_level_train %>%
  teacher_descriptives()

teacher_traintest_table <- bind_cols(teacher_level_train_prep, teacher_level_prep) %>%
  select(-Variable...3) %>%
  rename(Variable=`Variable...1`, `Train Data`=Values...2, `Test Data`=Values...4)

kable(teacher_traintest_table, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 1) %>%
  pack_rows("Letters Written in Current Year", 2, 6) %>%
  pack_rows("Letters Written in Past Two Years", 7, 12) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "teacher_traintest_descriptives", filesuffix, ".jpg")), zoom=2, bs_theme="default")


student_teacher <- test_analytic_data %>%
  select(letterid, year_2019, subject, gradestaught_other,
         coach, years_taught) %>%
  distinct()

student_teacher_2018 <- student_teacher %>%
  filter(year_2019==0)

student_teacher_2019 <- student_teacher %>%
  filter(year_2019==1)

student_teacher_descriptives <- . %>%
  summarize(English=length(.$subject[.$subject=="English"])/length(.$letterid),
            `Social Studies`=length(.$subject[.$subject=="Social Studies"])/length(.$letterid),
            Math=length(.$subject[.$subject=="Math"])/length(.$letterid),
            Science=length(.$subject[.$subject=="Science"])/length(.$letterid),
            `World Language`=length(.$subject[.$subject=="World Language"])/length(.$letterid),
            `Computer Science`=length(.$subject[.$subject=="Computer Science"])/length(.$letterid),
            Other=length(.$subject[.$subject=="Other"])/length(.$letterid),

            `Coach`=mean(coach),
            
            `0`=length(.$years_taught[.$years_taught=="0"])/length(.$letterid),
            `1`=length(.$years_taught[.$years_taught=="1"])/length(.$letterid),
            `2`=length(.$years_taught[.$years_taught=="2"])/length(.$letterid),
            `3`=length(.$years_taught[.$years_taught=="3"])/length(.$letterid),
            `4`=length(.$years_taught[.$years_taught=="4"])/length(.$letterid),
            
            `Other Relationship`=mean(gradestaught_other)) %>%
  mutate(across(everything(), round, 3)) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  stack() %>%
  relocate(ind, values) %>%
  rename(Variable=ind, Values=values)

student_teacher_2018_prep <- student_teacher_2018 %>%
  student_teacher_descriptives()

student_teacher_2019_prep <- student_teacher_2019 %>%
  student_teacher_descriptives()

student_teacher_prep <- student_teacher %>%
  student_teacher_descriptives()

student_teacher_table <- bind_cols(student_teacher_2018_prep, student_teacher_2019_prep, student_teacher_prep) %>%
  select(-Variable...3, -Variable...5) %>%
  rename(Variable=`Variable...1`, `2018`=Values...2, `2019`=Values...4, `Pooled`=Values...6)

kable(student_teacher_table, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Subject Area", 1, 7) %>%
  pack_rows("Student-Coach Relationship", 8, 8) %>%
  pack_rows("Years Known", 9, 13) %>%
  pack_rows("Other Relationship", 14, 14) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "student_teacher_descriptives", filesuffix, ".jpg")), zoom=2, bs_theme="default")

student_teacher_train <- train_analytic_data %>%
  select(letterid, year_2019, subject, gradestaught_other,
         coach, years_taught) %>%
  distinct()

student_teacher_train_prep <- student_teacher_train %>%
  student_teacher_descriptives()

student_teacher_traintest_table <- bind_cols(student_teacher_train_prep, student_teacher_prep) %>%
  select(-Variable...3) %>%
  rename(Variable=`Variable...1`, `Train Data`=Values...2, `Test Data`=Values...4)
  
kable(student_teacher_traintest_table, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Subject Area", 1, 7) %>%
  pack_rows("Student-Coach Relationship", 8, 8) %>%
  pack_rows("Years Known", 9, 13) %>%
  pack_rows("Other Relationship", 14, 14) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "student_teacher_traintest", filesuffix, ".jpg")), zoom=2, bs_theme="default")

supertopics_subset <- read_csv(file=file.path(project_data, "04d_topic_codes_bk_21-12-20.csv")) %>%
  filter(!is.na(description)) %>%
  filter(supertopic!="Other") %>%
  arrange(supertopic) %>%
  unite(allwords, 5:9, sep=", ") %>%
  select(supertopic, description, allwords) %>%
  group_by(supertopic) %>%
  slice_head(n=2) %>%
  rename(`Subtopic Interpretation` = description,
         `Supertopic` = supertopic,
         `Top 5 Keywords` = allwords)

kable(supertopics_subset, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "supertopic_word_examples", filesuffix, ".jpg")), zoom=2, bs_theme="default")

supertopics_all <- read_csv(file=file.path(project_data, "04d_topic_codes_bk_21-12-20.csv")) %>%
  filter(!is.na(description)) %>%
  filter(supertopic!="Other") %>%
  arrange(supertopic) %>%
  unite(allwords, 5:24, sep=", ") %>%
  select(supertopic, description, allwords) %>%
  rename(`Subtopic Interpretation` = description,
         `Supertopic` = supertopic,
         `Top 20 Keywords` = allwords)

kable(supertopics_all, 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'") %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "supertopic_word_examples_all", filesuffix, ".jpg")), zoom=2, bs_theme="default")

distplot <- train_analytic_data %>% 
  ggplot(aes(x=st_intellectual)) +
  geom_density()

ggsave(distplot, file=file.path(project_output, paste0(fileprefix, 'character_excellence_distribution', filesuffix, '.png')), width=10, height=6)


rm(student_teacher, student_teacher_2018, student_teacher_2018_prep,
   student_teacher_2019, student_teacher_2019_prep, student_teacher_prep,
   student_teacher_table, student_teacher_train, student_teacher_train_prep,
   student_teacher_traintest_table, teacher_descriptives_table,
   teacher_level, teacher_level_2018, teacher_level_2018_prep, teacher_level_2019,
   teacher_level_2019_prep, teacher_level_prep, teacher_level_train,
   teacher_level_train_prep, teacher_traintest_table, test_analytic_data,
   test_sentiment_sentence, train_analytic_data, train_sentiment_sentence,
   student_teacher_descriptives, teacher_descriptives, supertopics_subset,
   supertopics_all)
