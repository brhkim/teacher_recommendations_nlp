################################################################################
#  
#   Name: 01_subsample_creation.R
#   
#   Purpose: Create a training subsample of the teacher recommendation data using
#   teachers as the selection level, stratified by the number of letters a
#   teacher has written in the 2018-2019 and 2019-2020 years. Create crosswalks 
#   and data subsets accordingly. Produce corresponding testing dataset.
#
################################################################################

# Set a filename prefix for products of this script
fileprefix <- "01_"

# Create a SAT_bin and ACT_bin crosswalk for institutions just to get it out of the way
satbin_crosswalk <- read_csv(file.path(raw_data, 'Application_Table_201618_BrianKim.csv'), guess_max=1000000) %>%
  select(member_idmasked, SAT_Bin, ACT_Bin) %>%
  distinct()

write_fst(satbin_crosswalk, path=file.path(project_data, paste0(fileprefix, 'cw_satbins', filesuffix, '.fst')))

# First load in the application data so we can determine who actually sent in a completed application
applications_2018 <- read_csv(file.path(raw_data, 'Application_Table_201618_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2018) %>%
  mutate(year_2019=0) %>%
  filter(application_submittedflag==1) %>%
  rename(applicantid=applicant_id)

applications_2019 <- read_csv(file.path(raw_data, 'Application_Table_201920_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2019) %>%
  mutate(year_2019=1) %>%
  # Need to join in SAT/ACT Bin data for 2019 data from 2018 data
  left_join(satbin_crosswalk, by="member_idmasked") %>%
  filter(application_submittedflag==1) %>%
  rename(applicantid=applicant_id)

applicants_2018_tmp <- applications_2018 %>%
  select(applicantid, year_2019) %>%
  distinct()

applicants_2019_tmp <- applications_2019 %>%
  select(applicantid, year_2019) %>%
  distinct()

# Create a crosswalk of students who actually submitted an application and take
# only their most recent completed application
most_recent_applicants <- applicants_2018_tmp %>%
  bind_rows(applicants_2019_tmp) %>%
  group_by(applicantid) %>%
  arrange(year_2019) %>%
  slice_tail(n=1) %>%
  ungroup()


# Load in the teachers data
teachers_2018 <- read_csv(file.path(raw_data, 'TeacherRecommendation_Table_201618_BrianKim.csv'))
teachers_2019 <- read_csv(file.path(raw_data, 'TeacherRecommendation_Table_201920_BrianKim.csv'))

teachers_experience_prep <- bind_rows(teachers_2018, teachers_2019) %>%
  mutate(year_2019=case_when(
    season==2018 ~ 0,
    season==2019 ~ 1
  )) %>%
  filter(teacher_reccsubmitstatus=="Yes") %>%
  group_by(recommenderid, season) %>%
  summarize(letters_written=n()) %>%
  ungroup() %>%
  pivot_wider(names_from=season, names_prefix="letters_", values_from=letters_written, values_fill=0)

teachers_experience_2018 <- teachers_experience_prep %>%
  select(recommenderid, letters_2016, letters_2017, letters_2018) %>%
  mutate(letters_lasttwo=letters_2016+letters_2017,
         letters_current=letters_2018,
         year_2019=0) %>%
  select(recommenderid, letters_lasttwo, letters_current, year_2019)

teachers_experience_2019 <- teachers_experience_prep %>%
  select(recommenderid, letters_2017, letters_2018, letters_2019) %>%
  mutate(letters_lasttwo=letters_2017+letters_2018,
         letters_current=letters_2019,
         year_2019=1) %>%
  select(recommenderid, letters_lasttwo, letters_current, year_2019)

teachers_experience <- bind_rows(teachers_experience_2018, teachers_experience_2019)

teachers_subject <- read_csv(file.path(raw_data, '20162020_Recommender_Subject.csv')) %>%
  filter(season==2018 | season==2019) %>%
  mutate(year_2019=case_when(
    season==2018 ~ 0,
    season==2019 ~ 1
  )) %>%
  select(invitationid, recommenderid, applicantid, rate_subjectarea, year_2019)

teachers <- bind_rows(teachers_2018, teachers_2019) %>%
  filter(season==2018 | season==2019) %>%
  mutate(year_2019=case_when(
    season==2018 ~ 0,
    season==2019 ~ 1
  )) %>%
  left_join(teachers_subject, by=c("invitationid", "recommenderid", "applicantid", "year_2019")) %>%
  left_join(teachers_experience, by=c("recommenderid", "year_2019")) %>%
  mutate(subject=case_when(
    rate_subjectarea==0 ~ "Math",
    rate_subjectarea==1 ~ "English",
    rate_subjectarea==2 ~ "Science",
    rate_subjectarea==3 ~ "World Language",
    rate_subjectarea==4 ~ "Social Studies",
    rate_subjectarea==5 ~ "Other",
    rate_subjectarea==6 ~ "Computer Science",
    TRUE ~ "Missing"
  )) %>%
  mutate(subject=factor(subject)) %>%
  mutate(
    gradestaught_other=case_when(
      is.na(text_gradeleveltaughtapplicant) ~ 0,
      !is.na(text_gradeleveltaughtapplicant) ~ as.numeric(str_count(text_gradeleveltaughtapplicant, pattern="4"))),
    years_taught=case_when(
      is.na(text_gradeleveltaughtapplicant) ~ "Missing",
      !is.na(text_gradeleveltaughtapplicant) ~ as.character(
        str_count(text_gradeleveltaughtapplicant, pattern="0") + 
          str_count(text_gradeleveltaughtapplicant, pattern="1") + 
          str_count(text_gradeleveltaughtapplicant, pattern="2") + 
          str_count(text_gradeleveltaughtapplicant, pattern="3")
      )
    )) %>%
  mutate(years_taught=factor(years_taught))

coach_dict <- dictionary(
  list(
    coach = c("coach", "athlete", "coached", "soccer", "track", "varsity", "basketball",
              "football", "coaching", "cross_country", "wrestling", "golf", "running",
              "lacrosse", "polo", "horse", "horseback", "equestrian", "bowling", "field", "volleyball",
              "tennis", "hockey", "badminton", "softball", "baseball", "swim", "swimming", "diving",
              "crew", "rowing", "rowed", "cycling", "bicycle", "bicycling")))

teacher_coach_prep <- teachers %>%
  mutate(letterid = paste0(recommenderid, "__", applicantid, "__", invitationid, "__", year_2019)) %>%
  select(letterid, text_howlongknown) %>%
  corpus(docid_field="letterid", text_field="text_howlongknown") %>%
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE) %>%
  tokens_tolower() %>%
  tokens_ngrams(n=1:2) %>%
  tokens_lookup(dictionary = coach_dict) %>% 
  dfm() %>%
  convert(to="data.frame")
  
teacher_coach_prep2 <- str_split_fixed(teacher_coach_prep$doc_id,"__", n=4) %>%
  as.data.frame()
  
teacher_coach <- data.frame(recommenderid=as.numeric(teacher_coach_prep2$V1), applicantid=as.numeric(teacher_coach_prep2$V2), invitationid=as.numeric(teacher_coach_prep2$V3), year_2019=as.numeric(teacher_coach_prep2$V4), coach=teacher_coach_prep$coach) %>%
  mutate(coach=case_when(
    coach>=1 ~ 1,
    TRUE ~ 0
  ))

teachers <- teachers %>%
  left_join(teacher_coach, by=c("recommenderid", "applicantid", "invitationid", "year_2019"))

# Generate a crosswalk of randomly subsampled teachers for the training pipeline
submitted_teachers <- teachers %>%
  # Generate a variable to signify that the recommendation was submitted
  inner_join(most_recent_applicants, by=c("applicantid", "year_2019")) %>%
  # Filter down only to those submitted recommendations
  filter(teacher_reccsubmitstatus=="Yes") %>%
  # Drill down the dataset
  select(recommenderid, year_2019, letters_current) %>%
  distinct() %>%
  group_by(recommenderid) %>%
  summarize(total_letters=sum(letters_current)) %>%
  ungroup() %>%
  mutate(total_letters_group=case_when(
    total_letters==1 ~ "1",
    total_letters>1 & total_letters <=5 ~ "2-5",
    total_letters>5 & total_letters<=10 ~ "6-10",
    total_letters>10 & total_letters<=25 ~ "11-25",
    total_letters>25 ~ "25+"
  )) %>%
  mutate(total_letters_group=factor(total_letters_group, levels=c("1", "2-5", "6-10", "11-25", "25+")))

# Set the random seed for replicability
set.seed(1234)

# Get a randomly sampled group of teachers for the training set
train_teachers_set <- submitted_teachers %>%
  # Calculate buckets of writing experience for stratification later
  group_by(total_letters_group) %>%
  # Sample within groups of writing experience buckets
  slice_sample(prop=subsample_proportion/100) %>%
  select(recommenderid, total_letters, total_letters_group) %>%
  ungroup()

# Get the complementary set of teachers not in the training group for test group purposes
test_teachers_set <- submitted_teachers %>%
  anti_join(train_teachers_set, by="recommenderid")
  
# Quick stats check to see how many recs were written on average in each group
train_teachers_set$total_letters %>% summary()
test_teachers_set$total_letters %>% summary()

# Check groupings to ensure stratification worked
divide_tmp <- train_teachers_set$total_letters_group %>% summary() 
divide_tmp/length(train_teachers_set$total_letters_group)

divide_tmp <- test_teachers_set$total_letters_group %>% summary() 
divide_tmp/length(test_teachers_set$total_letters_group)

# Count total letters for each group
sum_tmp <- train_teachers_set$total_letters %>% sum()
sum_tmp2 <- test_teachers_set$total_letters %>% sum()

sum_tmp/(sum_tmp+sum_tmp2)
sum_tmp2/(sum_tmp+sum_tmp2)

# Check to make sure the recommenderid's are mutually exclusive across groups
(train_teachers_set$recommenderid %>% unique() %>% length()) + 
  (test_teachers_set$recommenderid %>% unique() %>% length()) == 
  (bind_rows(train_teachers_set, test_teachers_set) %>% .$recommenderid %>% unique() %>% length())

# Save dataset
write_fst(train_teachers_set, path=file.path(project_data, paste0(fileprefix, 'train_cw_teachers', filesuffix, '.fst')))
write_fst(test_teachers_set, path=file.path(project_data, paste0(fileprefix, 'test_cw_teachers', filesuffix, '.fst')))

# Create additional crosswalks to indicate which applicants and recommendation letters are in-sample
# Make crosswalk dataset including all ids together
train_teachers <- teachers %>%
  inner_join(most_recent_applicants, by=c("applicantid", "year_2019")) %>%
  # Filter down only to those submitted recommendations
  filter(teacher_reccsubmitstatus=="Yes") %>%
  right_join(train_teachers_set, by="recommenderid") %>%
  mutate(letters_lasttwo_group=case_when(
    letters_lasttwo==0 ~ "0",
    letters_lasttwo==1 ~ "1",
    letters_lasttwo>1 & letters_lasttwo<=5 ~ "2-5",
    letters_lasttwo>5 & letters_lasttwo<=10 ~ "6-10",
    letters_lasttwo>10 & letters_lasttwo<=25 ~ "11-25",
    letters_lasttwo>25 ~ "25+"
  ),
  letters_current_group=case_when(
    letters_current==1 ~ "1",
    letters_current>1 & letters_current<=5 ~ "2-5",
    letters_current>5 & letters_current<=10 ~ "6-10",
    letters_current>10 & letters_current<=25 ~ "11-25",
    letters_current>25 ~ "25+"
  ),
  letters_lasttwo_group=factor(letters_lasttwo_group, levels=c("0", "1", "2-5", "6-10", "11-25", "25+")),
  letters_current_group=factor(letters_current_group, levels=c("1", "2-5", "6-10", "11-25", "25+"))
  )

train_all_crosswalk <- train_teachers %>%
  select(invitationid, recommenderid, applicantid, year_2019)

test_teachers <- teachers %>%
  inner_join(most_recent_applicants, by=c("applicantid", "year_2019")) %>%
  # Filter down only to those submitted recommendations
  filter(teacher_reccsubmitstatus=="Yes") %>%
  right_join(test_teachers_set, by="recommenderid") %>%
  mutate(letters_lasttwo_group=case_when(
    letters_lasttwo==0 ~ "0",
    letters_lasttwo==1 ~ "1",
    letters_lasttwo>1 & letters_lasttwo<=5 ~ "2-5",
    letters_lasttwo>5 & letters_lasttwo<=10 ~ "6-10",
    letters_lasttwo>10 & letters_lasttwo<=25 ~ "11-25",
    letters_lasttwo>25 ~ "25+"
  ),
  letters_current_group=case_when(
    letters_current==1 ~ "1",
    letters_current>1 & letters_current<=5 ~ "2-5",
    letters_current>5 & letters_current<=10 ~ "6-10",
    letters_current>10 & letters_current<=25 ~ "11-25",
    letters_current>25 ~ "25+"
  ),
  letters_lasttwo_group=factor(letters_lasttwo_group, levels=c("0", "1", "2-5", "6-10", "11-25", "25+")),
  letters_current_group=factor(letters_current_group, levels=c("1", "2-5", "6-10", "11-25", "25+"))
  )

test_all_crosswalk <- test_teachers %>%
  select(invitationid, recommenderid, applicantid, year_2019)

# Note that student populations are *not* mutually exclusive across train/test
teachers %>%
  inner_join(most_recent_applicants, by=c("applicantid", "year_2019")) %>%
  # Filter down only to those submitted recommendations
  filter(teacher_reccsubmitstatus=="Yes") %>%
  .$applicantid %>%
  unique() %>%
  length()

train_all_crosswalk %>%
  .$applicantid %>%
  unique() %>%
  length()

test_all_crosswalk %>%
  .$applicantid %>%
  unique() %>%
  length()

# But the letters must be
train_all_crosswalk %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length() +

test_all_crosswalk %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length() ==

teachers %>%
  inner_join(most_recent_applicants, by=c("applicantid", "year_2019")) %>%
  # Filter down only to those submitted recommendations
  filter(teacher_reccsubmitstatus=="Yes") %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length()

# Save dataset
write_fst(train_teachers, path=file.path(project_data, paste0(fileprefix, 'train_teachers', filesuffix, '.fst')))
write_fst(test_teachers, path=file.path(project_data, paste0(fileprefix, 'test_teachers', filesuffix, '.fst')))

# Save dataset
write_fst(train_all_crosswalk, path=file.path(project_data, paste0(fileprefix, 'train_cw_all', filesuffix, '.fst')))
write_fst(test_all_crosswalk, path=file.path(project_data, paste0(fileprefix, 'test_cw_all', filesuffix, '.fst')))

# Make crosswalk dataset for only invitation ids
train_invitations_crosswalk <- train_all_crosswalk %>%
  select(invitationid, year_2019) %>%
  distinct()

test_invitations_crosswalk <- test_all_crosswalk %>%
  select(invitationid, year_2019) %>%
  distinct()

# Save dataset
write_fst(train_invitations_crosswalk, path=file.path(project_data, paste0(fileprefix, 'train_cw_invitations', filesuffix, '.fst')))
write_fst(test_invitations_crosswalk, path=file.path(project_data, paste0(fileprefix, 'test_cw_invitations', filesuffix, '.fst')))

# Make crosswalk dataset for only applicant ids
train_applicants_crosswalk <- train_all_crosswalk %>%
  select(applicantid, year_2019) %>%
  distinct()

test_applicants_crosswalk <- test_all_crosswalk %>%
  select(applicantid, year_2019) %>%
  distinct()

# Save dataset
write_fst(train_applicants_crosswalk, path=file.path(project_data, paste0(fileprefix, 'train_cw_applicants', filesuffix, '.fst')))
write_fst(test_applicants_crosswalk, path=file.path(project_data, paste0(fileprefix, 'test_cw_applicants', filesuffix, '.fst')))


# Generate a corresponding subsample dataset of the applicant data
applicants_2018 <- read_csv(file.path(raw_data, 'Applicant_Table_201618_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2018) %>%
  mutate(year_2019=0) %>%
  rename(applicantid=applicant_id)

applicants_2019 <- read_csv(file.path(raw_data, 'Applicant_Table_201920_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2019) %>%
  mutate(year_2019=1) %>%
  rename(applicantid=applicant_id)

applicants <- applicants_2018 %>%
  mutate(gpa_scale = as.character(gpa_scale),
         TOEFL_paper_listening_score = as.character(TOEFL_paper_listening_score),
         TOEFL_paper_reading_score = as.character(TOEFL_paper_reading_score),
         TOEFL_paper_wrexpress_score = as.character(TOEFL_paper_wrexpress_score)) %>%
  bind_rows(applicants_2019)

train_applicants <- train_applicants_crosswalk %>%
  left_join(applicants, by=c("applicantid", "year_2019"))

test_applicants <- test_applicants_crosswalk %>%
  left_join(applicants, by=c("applicantid", "year_2019"))

# Save dataset
write_fst(train_applicants, path=file.path(project_data, paste0(fileprefix, 'train_applicants', filesuffix, '.fst')))
write_fst(test_applicants, path=file.path(project_data, paste0(fileprefix, 'test_applicants', filesuffix, '.fst')))

train_applications <- applications_2018 %>%
  bind_rows(applications_2019) %>%
  right_join(train_applicants_crosswalk, by=c("applicantid", "year_2019"))
  
test_applications <- applications_2018 %>%
  bind_rows(applications_2019) %>%
  right_join(test_applicants_crosswalk, by=c("applicantid", "year_2019"))

# Save dataset
write_fst(train_applications, path=file.path(project_data, paste0(fileprefix, 'train_applications', filesuffix, '.fst')))
write_fst(test_applications, path=file.path(project_data, paste0(fileprefix, 'test_applications', filesuffix, '.fst')))

# Generate a corresponding subsample dataset of the extracurriculars data
extracurriculars_2018 <- read_csv(file.path(raw_data, 'Extracurricular_Table_201618_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2018) %>%
  mutate(year_2019=0) %>%
  rename(applicantid=applicant_id)

extracurriculars_2019 <- read_csv(file.path(raw_data, 'Extracurricular_Table_201920_BrianKim.csv'), guess_max=1000000) %>%
  filter(season==2019) %>%
  mutate(year_2019=1) %>%
  rename(applicantid=applicant_id)

train_extracurriculars <- extracurriculars_2018 %>%
  bind_rows(extracurriculars_2019) %>%
  right_join(train_applicants_crosswalk, by=c("applicantid", "year_2019"))

test_extracurriculars <- extracurriculars_2018 %>%
  bind_rows(extracurriculars_2019) %>%
  right_join(test_applicants_crosswalk, by=c("applicantid", "year_2019"))

# Save dataset
write_fst(train_extracurriculars, path=file.path(project_data, paste0(fileprefix, 'train_extracurriculars', filesuffix, '.fst')))
write_fst(test_extracurriculars, path=file.path(project_data, paste0(fileprefix, 'test_extracurriculars', filesuffix, '.fst')))


# Generate a corresponding subsample dataset of the recommendation sending data
recssent_2018 <- read_csv(file.path(raw_data, 'rec_member_submission_2018.csv'), guess_max=1000000) %>%
  mutate(year_2019=0)

recssent_2019 <- read_csv(file.path(raw_data, 'rec_member_submission_2019.csv'), guess_max=1000000) %>%
  mutate(year_2019=1)

train_recssent <- recssent_2018 %>%
  bind_rows(recssent_2019) %>%
  right_join(train_invitations_crosswalk, by=c("invitationid", "year_2019"))

test_recssent <- recssent_2018 %>%
  bind_rows(recssent_2019) %>%
  right_join(test_invitations_crosswalk, by=c("invitationid", "year_2019"))

# Save dataset
write_fst(train_recssent, path=file.path(project_data, paste0(fileprefix, 'train_recssent', filesuffix, '.fst')))
write_fst(test_recssent, path=file.path(project_data, paste0(fileprefix, 'test_recssent', filesuffix, '.fst')))


# Generate a corresponding subsample dataset of the text data
texts_prep_2018 <- read_csv(file.path(raw_data, '2018_Recommender_Text.csv'), na=character()) %>%
  mutate(year_2019=0)
texts_prep_2019 <- read_csv(file.path(raw_data, '2019_Recommender_Text.csv'), na=character()) %>%
  mutate(year_2019=1)

texts <- texts_prep_2018 %>%
  bind_rows(texts_prep_2019) %>%
  rename(datavalscrubbedtext=data.val.scrubbedtext) %>%
  group_by(invitationid, year_2019) %>%
  mutate(index=row_number(),
         maxindex=max(index)) %>%
  ungroup() %>%
  # Filter out clearly erroneous letters over a thousand lines, usually completely duplicated
  filter(maxindex<1000)

train_texts <- texts %>%
  right_join(train_invitations_crosswalk, by=c("invitationid", "year_2019")) %>%
  filter(!is.na(index))

test_texts <- texts %>%
  right_join(test_invitations_crosswalk, by=c("invitationid", "year_2019")) %>%
  filter(!is.na(index))

# Ensure that they are mutually exclusive sets of letters
train_texts %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length() + 
  
test_texts %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length() ==
  
train_texts %>%
  bind_rows(test_texts) %>%
  mutate(letterid=paste0(invitationid, "_", year_2019)) %>%
  .$letterid %>%
  unique() %>%
  length()

# Save dataset
write_fst(train_texts, path=file.path(project_data, paste0(fileprefix, 'train_texts', filesuffix, '.fst')))
write_fst(test_texts, path=file.path(project_data, paste0(fileprefix, 'test_texts', filesuffix, '.fst')))

# Clear out the memory
rm(applicants, applicants_2018, applicants_2018_tmp, applicants_2019, applicants_2019_tmp,
   applications_2018, applications_2019, extracurriculars_2018, extracurriculars_2019,
   most_recent_applicants, recssent_2018, recssent_2019, satbin_crosswalk, submitted_teachers,
   teacher_coach, teacher_coach_prep, coach_dict, divide_tmp, sum_tmp, sum_tmp2, teacher_coach_prep2, teachers, teachers_2018, teachers_2019,
   teachers_experience, teachers_experience_2018, teachers_experience_2019, teachers_experience_prep,
   teachers_subject, test_all_crosswalk, test_applicants, test_applicants_crosswalk,
   test_applications, test_extracurriculars, test_invitations_crosswalk, test_recssent,
   test_teachers, test_teachers_set, test_texts, texts, texts_prep_2018, texts_prep_2019,
   train_all_crosswalk, train_applicants, train_applicants_crosswalk, train_applications,
   train_extracurriculars, train_invitations_crosswalk, train_recssent, train_teachers, 
   train_teachers_set, train_texts)
