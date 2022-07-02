################################################################################
#  
#   Name: 02_descriptives_cleaning.R
#   
#   Purpose: Clean up basic descriptive data on a given train/test dataset
#   to prepare for later text processing and analytic steps
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "02_"

## Load in the datasets
applicants <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_applicants', filesuffix, '.fst')))

applications <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_applications', filesuffix, '.fst')))

teachers <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_teachers', filesuffix, '.fst')))

extracurriculars <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_extracurriculars', filesuffix, '.fst')))

texts <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_texts', filesuffix, '.fst')))

recssent <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_recssent', filesuffix, '.fst')))

## Load in the crosswalks
applicants_crosswalk <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_cw_applicants', filesuffix, '.fst')))

invitations_crosswalk <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_cw_invitations', filesuffix, '.fst')))

teachers_crosswalk <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_cw_teachers', filesuffix, '.fst')))

all_crosswalk <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_cw_all', filesuffix, '.fst')))


################################################################################
#
#   #clean - Clean and organize the datasets
#
################################################################################

# Create a cleaned dataset with transformed variables of interest
demographics <- applicants %>%
  rename(age=Age) %>%
  select(-applicant_essay, -season) %>%
  mutate(
    female = case_when(
      applicant_sex=="Female" ~ 1,
      applicant_sex=="Male" ~ 0,
      is.na(applicant_sex) ~ 0),
    female_miss=case_when(
      is.na(applicant_sex) ~ 1,
      TRUE ~ 0),
    age_17=case_when(
      age==17 ~ 1,
      TRUE ~ 0),
    age_u17=case_when(
      age<17 ~ 1,
      TRUE ~ 0),
    age_a17=case_when(
      age>17 ~ 1,
      TRUE ~ 0),
    international=case_when(
      applicant_country!="United States of America" | applicant_raceethnicity=="Nonresident Alien" ~ 1,
      TRUE ~ 0),
    race_miss=case_when(
      applicant_raceethnicity=="Nonresident Alien" | applicant_raceethnicity=="Unknown" ~ 1,
      TRUE ~ 0),
    white=case_when(
      applicant_raceethnicity=="White" ~ 1,
      TRUE ~ 0),
    black=case_when(
      applicant_raceethnicity=="Black or African American" ~ 1,
      TRUE ~ 0),
    asian=case_when(
      applicant_raceethnicity=="Asian" ~ 1,
      TRUE ~ 0),
    latinx=case_when(
      applicant_raceethnicity=="Latinx" ~ 1,
      TRUE ~ 0),
    other_race=case_when(
      applicant_raceethnicity=="American Indian or Alaska Native" | applicant_raceethnicity=="Native Hawaiian or Other Pacific Islander" | applicant_raceethnicity=="Two or More Races" ~ 1,
      TRUE ~ 0),
    firstgen=case_when(
      applicant_firstgenstatus=="First-generation" ~ 1,
      TRUE ~ 0),
    firstgen_miss=case_when(
      is.na(applicant_firstgenstatus) ~ 1,
      TRUE ~ 0),
    school_public=case_when(
      applicant_highschooltype=="Public" | applicant_highschooltype=="Charter" ~ 1,
      TRUE ~ 0),
    school_private=case_when(
      applicant_highschooltype=="Independent" | applicant_highschooltype=="Religious" ~ 1,
      TRUE ~ 0),
    school_other=case_when(
      applicant_highschooltype=="Home School" | applicant_highschooltype=="Unknown" ~ 1,
      TRUE ~ 0),
    senior=case_when(
      applicant_classyr=="Senior" ~ 1,
      TRUE ~ 0),
    multischool=case_when(
      number_other_high_schools!=0 & !is.na(number_other_high_schools) ~ 1,
      TRUE ~ 0),
    multischool_miss=case_when(
      is.na(number_other_high_schools) ~ 1,
      TRUE ~ 0)
    ) %>%
  select(applicantid, female, female_miss, age_17, age_u17, age_a17, international, 
         race_miss, white, black, asian, latinx, other_race, 
         firstgen, firstgen_miss, school_public, school_private, school_other, 
         senior, multischool, multischool_miss)

feewaiver <- applications %>%
  select(applicantid, application_feewaiver) %>%
  mutate(
    feewaiver=case_when(
      application_feewaiver=="Common App fee waiver" ~ 1,
      application_feewaiver=="Member fee waiver" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  group_by(applicantid) %>%
  summarize(feewaiver=max(feewaiver)) %>%
  ungroup()

demographics <- demographics %>% 
  left_join(feewaiver, by="applicantid")

# Process class ranks and gpa buckets
classrank_gpa <- applicants %>%
  # First convert rank variables into numeric as necessary
  mutate(
    class_rank=as.numeric(class_rank),
    class_size=as.numeric(class_size),
    rank_pctile=class_rank/class_size) %>%
  # Construct binary indicators by quintile, prioritizing calculated percentile first
  mutate(
    quintile_rank_clean=case_when(
      rank_pctile<=0.01 ~ "Top 1%",
      rank_pctile>0.01 & rank_pctile<=0.05 ~ "Top 5%",
      rank_pctile>0.05 & rank_pctile<=0.1 ~ "Top 10%",
      rank_pctile>0.1 & rank_pctile<=0.2 ~ "Top 20%",
      rank_pctile>0.2 & rank_pctile<=0.4 ~ "Top 40%",
      rank_pctile>0.4 & rank_pctile<=0.6 ~ "Top 60%",
      rank_pctile>0.6 & rank_pctile<=0.8 ~ "Top 80%",
      rank_pctile>0.8 & rank_pctile<=1.0 ~ "Top 100%")) %>%
  # Now fill in binary indicators if missing and other rank variables are available
  mutate(
    quintile_rank_clean=case_when(
      !is.na(quintile_rank_clean) ~ quintile_rank_clean,
      is.na(quintile_rank_clean) & (decile_rank=="Top 10%") ~ "Top 10%",
      is.na(quintile_rank_clean) & (decile_rank=="Top 20%" | quintile_rank=="Top 20%" | quartile_rank=="Top 25%") ~ "Top 20%",
      is.na(quintile_rank_clean) & (decile_rank=="Top 30%" | decile_rank=="Top 40%" | quintile_rank=="Top 40%" | quartile_rank=="Top 50%") ~ "Top 40%",
      is.na(quintile_rank_clean) & (decile_rank=="Top 50%" | decile_rank=="Top 60%" | quintile_rank=="Top 60%") ~ "Top 60%",
      is.na(quintile_rank_clean) & (decile_rank=="Top 70%" | decile_rank=="Top 80%" | quintile_rank=="Top 80%" | quartile_rank=="Top 75%") ~ "Top 80%",
      is.na(quintile_rank_clean) & (decile_rank=="Top 90%" | decile_rank=="Top 100%" | quintile_rank=="Top 100%" | quartile_rank=="Top 100%") ~ "Top 100%",
      TRUE ~ "Missing")) %>%
  mutate(
    quintile_rank_clean=case_when(
      quintile_rank_clean=="Top 100%" ~ "Top 40%", 
      quintile_rank_clean=="Top 80%" ~ "Top 40%", 
      quintile_rank_clean=="Top 60%" ~ "Top 40%",
      TRUE ~ quintile_rank_clean)
    ) %>%
  # Create ordered factor variable
  mutate(
    quintile_rank_clean=factor(quintile_rank_clean, levels=c("Missing", "Top 40%", "Top 20%", "Top 10%", "Top 5%", "Top 1%"))) %>%
  # Make cumulative GPA numeric
  mutate(
    cumulative_gpa=as.numeric(cumulative_gpa),
    # Get cumulative GPA into reasonable buckets by score ranges
    gpa_scaled=case_when(
      cumulative_gpa>1 & cumulative_gpa<=6 ~ cumulative_gpa/4,
      cumulative_gpa>7 & cumulative_gpa<=15 ~ cumulative_gpa/10,
      cumulative_gpa>50 & cumulative_gpa<=150 ~ cumulative_gpa/100),
    # Create bins of GPAs
    gpa_bucket=case_when(
      is.na(gpa_scaled) ~ "Other/Missing",
      gpa_scaled==1 ~ "1.00",
      gpa_scaled>=0.9 & gpa_scaled<1 ~ "0.90-0.99",
      gpa_scaled<0.9 ~ "<0.90",
      gpa_scaled>1 ~ ">1.00",
      TRUE ~ "Other/Missing")) %>%
  # Create ordered factor variable
  mutate(
    gpa_bucket=factor(gpa_bucket, levels=c("Other/Missing",  "<0.90", "0.90-0.99", "1.00", ">1.00"))) %>%
  select(applicantid, quintile_rank_clean, gpa_bucket, gpa_scaled) %>%
  rename(quintile_rank=quintile_rank_clean)


# Process TOEFL scores
toefl <- applicants %>%
  # First generate an indicator for taking any TOEFL
  mutate(
    any_toefl=case_when(
      !is.na(TOEFL_iBT_total_score) ~ 1,
      !is.na(TOEFL_paper_total_score) ~ 1,
      TRUE ~ 0)) %>%
  # Now generate a factor variable for TOEFL score; percentiles generated using TOEFL Score Percentiles.pdf in docs
  mutate(
    TOEFL_iBT_total_score=as.numeric(TOEFL_iBT_total_score),
    toefl_percentile=case_when(
      any_toefl==0 ~ "No TOEFL",
      TOEFL_iBT_total_score>102 ~ ">=90",
      TOEFL_iBT_total_score<=102 ~ "<90",
      TOEFL_paper_total_score>620 ~ ">=90",
      TOEFL_paper_total_score<=620 ~ "<90")) %>%
  # Store it as a proper factor variable
  mutate(
    toefl_percentile=factor(toefl_percentile, levels=c("No TOEFL", "<90", ">=90"))) %>%
  # More concise any_toefl
  select(applicantid, toefl_percentile)


# Process SAT/ACT scores
sat_act <- applicants %>%
  # First get rid of any erroneous SAT scores just in case
  mutate(
    preSAT_math=case_when(
      preSAT_math<200 ~ as.numeric(NA),
      TRUE ~ preSAT_math),
    postSAT_math=case_when(
      postSAT_math<200 ~ as.numeric(NA),
      TRUE ~ postSAT_math),
    preSAT_cr=case_when(
      preSAT_cr<200 ~ as.numeric(NA),
      TRUE ~ preSAT_cr),
    postSAT_rw=case_when(
      postSAT_rw<200 ~ as.numeric(NA),
      TRUE ~ postSAT_rw)) %>%
  # First get their most recent SAT score as available (between old SAT and new SAT)
  mutate(
    sat_math=case_when(
      !is.na(preSAT_math) & is.na(postSAT_math) ~ preSAT_math,
      is.na(preSAT_math) & !is.na(postSAT_math) ~ postSAT_math,
      !is.na(preSAT_math) & !is.na(postSAT_math) ~ postSAT_math),
    sat_verb=case_when(
      !is.na(preSAT_cr) & is.na(postSAT_rw) ~ preSAT_cr,
      is.na(preSAT_cr) & !is.na(postSAT_rw) ~ postSAT_rw,
      !is.na(preSAT_cr) & !is.na(postSAT_rw) ~ postSAT_rw)) %>%
  # Now get ahold of their ACT scores and translate into SAT scores. First remove erroneous scores
  mutate(
    ACT_math=case_when(
      ACT_math==0 ~ as.numeric(NA),
      TRUE ~ ACT_math),
    ACT_reading=case_when(
      ACT_reading==0 ~ as.numeric(NA),
      TRUE ~ ACT_reading),
    ACT_english=case_when(
      ACT_english==0 ~ as.numeric(NA),
      TRUE ~ ACT_english)) %>%
  # Then actually translate the scores properly into SAT equivalents
  mutate(
    act_math_equiv=case_when(
      ACT_math== 36 ~ 800,
      ACT_math== 35 ~ 780,
      ACT_math== 34 ~ 760,
      ACT_math== 33 ~ 740,
      ACT_math== 32 ~ 720,
      ACT_math== 31 ~ 710,
      ACT_math== 30 ~ 700,
      ACT_math== 29 ~ 680,
      ACT_math== 28 ~ 660,
      ACT_math== 27 ~ 640,
      ACT_math== 26 ~ 610,
      ACT_math== 25 ~ 590,
      ACT_math== 24 ~ 580,
      ACT_math== 23 ~ 560,
      ACT_math== 22 ~ 540,
      ACT_math== 21 ~ 530,
      ACT_math== 20 ~ 520,
      ACT_math== 19 ~ 510,
      ACT_math== 18 ~ 500,
      ACT_math== 17 ~ 470,
      ACT_math== 16 ~ 430,
      ACT_math== 15 ~ 400,
      ACT_math== 14 ~ 360,
      ACT_math== 13 ~ 330,
      ACT_math== 12 ~ 310,
      ACT_math== 11 ~ 280,
      ACT_math<= 10 ~ 260),
    # Note that this produces NAs if either english or reading value is missing, which I think is correct
    act_verb_equiv=case_when(
      ACT_english+ACT_reading == 72 ~ 790,
      ACT_english+ACT_reading == 71 ~ 770,
      ACT_english+ACT_reading == 70 ~ 750,
      ACT_english+ACT_reading == 69 ~ 740,
      ACT_english+ACT_reading == 68 ~ 730,
      ACT_english+ACT_reading == 67 ~ 720,
      ACT_english+ACT_reading == 66 ~ 710,
      ACT_english+ACT_reading == 65 ~ 700,
      ACT_english+ACT_reading == 64 ~ 700,
      ACT_english+ACT_reading == 63 ~ 690,
      ACT_english+ACT_reading == 62 ~ 680,
      ACT_english+ACT_reading == 61 ~ 680,
      ACT_english+ACT_reading == 60 ~ 670,
      ACT_english+ACT_reading == 59 ~ 660,
      ACT_english+ACT_reading == 58 ~ 660,
      ACT_english+ACT_reading == 57 ~ 650,
      ACT_english+ACT_reading == 56 ~ 640,
      ACT_english+ACT_reading == 55 ~ 640,
      ACT_english+ACT_reading == 54 ~ 630,
      ACT_english+ACT_reading == 53 ~ 630,
      ACT_english+ACT_reading == 52 ~ 620,
      ACT_english+ACT_reading == 51 ~ 610,
      ACT_english+ACT_reading == 50 ~ 610,
      ACT_english+ACT_reading == 49 ~ 600,
      ACT_english+ACT_reading == 48 ~ 590,
      ACT_english+ACT_reading == 47 ~ 580,
      ACT_english+ACT_reading == 46 ~ 580,
      ACT_english+ACT_reading == 45 ~ 570,
      ACT_english+ACT_reading == 44 ~ 560,
      ACT_english+ACT_reading == 43 ~ 550,
      ACT_english+ACT_reading == 42 ~ 540,
      ACT_english+ACT_reading == 41 ~ 540,
      ACT_english+ACT_reading == 40 ~ 520,
      ACT_english+ACT_reading == 39 ~ 520,
      ACT_english+ACT_reading == 38 ~ 510,
      ACT_english+ACT_reading == 37 ~ 500,
      ACT_english+ACT_reading == 36 ~ 500,
      ACT_english+ACT_reading == 35 ~ 490,
      ACT_english+ACT_reading == 34 ~ 480,
      ACT_english+ACT_reading == 33 ~ 470,
      ACT_english+ACT_reading == 32 ~ 460,
      ACT_english+ACT_reading == 31 ~ 450,
      ACT_english+ACT_reading == 30 ~ 440,
      ACT_english+ACT_reading == 29 ~ 430,
      ACT_english+ACT_reading == 28 ~ 420,
      ACT_english+ACT_reading == 27 ~ 410,
      ACT_english+ACT_reading == 26 ~ 400,
      ACT_english+ACT_reading == 25 ~ 390,
      ACT_english+ACT_reading == 24 ~ 380,
      ACT_english+ACT_reading == 23 ~ 370,
      ACT_english+ACT_reading == 22 ~ 360,
      ACT_english+ACT_reading == 21 ~ 350,
      ACT_english+ACT_reading == 20 ~ 340,
      ACT_english+ACT_reading == 19 ~ 330,
      ACT_english+ACT_reading == 18 ~ 320,
      ACT_english+ACT_reading == 17 ~ 310,
      ACT_english+ACT_reading == 16 ~ 300,
      ACT_english+ACT_reading == 15 ~ 290,
      ACT_english+ACT_reading <= 14 ~ 280)) %>%
  # Now take the higher of their two scores...
  mutate(
    highest_math=case_when(
      !is.na(sat_math) & is.na(act_math_equiv) ~ sat_math,
      is.na(sat_math) & !is.na(act_math_equiv) ~ act_math_equiv,
      !is.na(sat_math) & !is.na(act_math_equiv) & sat_math>act_math_equiv ~ sat_math,
      !is.na(sat_math) & !is.na(act_math_equiv) & sat_math<act_math_equiv ~ act_math_equiv),
    highest_verb=case_when(
      !is.na(sat_verb) & is.na(act_verb_equiv) ~ sat_verb,
      is.na(sat_verb) & !is.na(act_verb_equiv) ~ act_verb_equiv,
      !is.na(sat_verb) & !is.na(act_verb_equiv) & sat_verb>act_verb_equiv ~ sat_verb,
      !is.na(sat_verb) & !is.na(act_verb_equiv) & sat_verb<act_verb_equiv ~ act_verb_equiv)
  ) %>%
  # ALMOST THERE! Now group into percentile buckets. Note that I use the sample of SAT takers rather than nationally representative
  mutate(
    math_pctile_bucket=case_when(
      highest_math>=740 ~ ">=95",
      highest_math<740 & highest_math>=690 ~ "90-94",
      highest_math<690 & highest_math>=600 ~ "75-89",
      highest_math<600 ~ "<75",
      TRUE ~ "Missing"),
    verb_pctile_bucket=case_when(
      highest_verb>=710 ~ ">=95",
      highest_verb<710 & highest_verb>=670 ~ "90-94",
      highest_verb<670 & highest_verb>=610 ~ "75-89",
      highest_verb<610 ~ "<75",
      TRUE ~ "Missing")) %>%
  # Turn them into actual factor variables
  mutate(
    math_pctile_bucket=factor(math_pctile_bucket, levels=c("Missing", "<75", "75-89", "90-94", ">=95")),
    verb_pctile_bucket=factor(verb_pctile_bucket, levels=c("Missing", "<75", "75-89", "90-94", ">=95"))
  ) %>%
  # Generate a variable to indicate how many times they took the college-entrance tests total
  rowwise() %>%
  mutate(
    total_main_tests=sum(number_of_preSAT_taken, number_of_postSAT_taken, number_of_ACT_taken, na.rm=TRUE)
  ) %>% 
  ungroup() %>%
  # Fix cases where they obviously took at least one test because they reported scores
  mutate(
    total_main_tests=case_when(
      (math_pctile_bucket!="Missing" | verb_pctile_bucket!="Missing") & total_main_tests==0 ~ 1,
      (math_pctile_bucket=="Missing" & verb_pctile_bucket=="Missing") ~ 0,
      TRUE ~ total_main_tests)) %>%
  # Turn it into a set of buckets
  mutate(
    total_main_tests_bucket=case_when(
      total_main_tests==0 ~ "0",
      total_main_tests==1 ~ "1",
      total_main_tests==2 ~ "2",
      total_main_tests>2 ~ ">2")) %>%
  # Turn it into an actual factor variable
  mutate(
    total_main_tests_bucket=factor(total_main_tests_bucket, levels=c("0", "1", "2", ">2"))) %>%
  # Phew, get the output
  select(applicantid, math_pctile_bucket, verb_pctile_bucket, total_main_tests_bucket)

# Process SAT subject tests. First set a list of the variables
sat_subjects <- applicants %>%
  # Get a bucketed count of subject tests taken
  mutate(
    sat_subjects_taken=case_when(
      is.na(number_SAT_subs) | number_SAT_subs==0 ~ 0,
      number_SAT_subs>=1 ~ 1)) %>%
  # Turn it into a factor
  # Turn the scores into an index score for concision -- not sure what other signal we can parsimoniously pull here
  rowwise() %>%
  mutate(
    avg_sat_subject_score=mean(c(SAT_subject1_score, SAT_subject2_score, SAT_subject3_score, SAT_subject4_score, SAT_subject5_score,
                              SAT_subject6_score, SAT_subject7_score, SAT_subject8_score, SAT_subject9_score, SAT_subject10_score),
                              na.rm=TRUE)) %>%
  ungroup() %>%
  # Turn it into a set of buckets
  mutate(
    avg_sat_subject_bucket=case_when(
      avg_sat_subject_score>=750 ~ ">=750",
      avg_sat_subject_score<750 ~ "<750",
      TRUE ~ "Missing")) %>%
  # Turn it into an actual factor variables
  mutate(
    avg_sat_subject_bucket=factor(avg_sat_subject_bucket, levels=c("Missing", "<750", ">=750"))
  ) %>%
  select(applicantid, sat_subjects_taken, avg_sat_subject_bucket)
  
# Process AP scores -- first get in long format
ap_long_1 <- applicants %>%
  select(applicantid, contains("AP_subject") & contains("score")) %>%
  pivot_longer(cols=contains("score"), names_to="testnum", names_pattern="AP_subject(.+)_score", values_to="score")

ap_long_2 <- applicants %>%
  select(applicantid, contains("AP_subject") & ends_with("subject")) %>%
  pivot_longer(cols=ends_with("subject"), names_to="testnum", names_pattern="AP_subject(.+)_subject", values_to="subject")

# Make student crosswalk to incorporate students without AP tests
applicants_submit_ids <- applicants %>%
  select(applicantid) %>%
  mutate(subject_group="arts")

ap_long <- ap_long_1 %>%
  left_join(ap_long_2, by=c("applicantid", "testnum")) %>%
  filter(!is.na(score) & !is.na(subject)) %>%
  # Recode the tests into a few buckets
  mutate(subject_group=case_when(
    subject == "Art: Studio Art-2-D Design" ~ "arts",
    subject == "Art: Studio Art-3-D Design" ~ "arts",
    subject == "Art: Studio Art-drawing" ~ "arts",
    subject == "Biology" ~ "stem",
    subject == "Calculus AB" ~ "stem",
    subject == "Calculus BC" ~ "stem",
    subject == "Calculus BC - AB Subscore Grade" ~ "stem",
    subject == "Chemistry" ~ "stem",
    subject == "Chinese Language & Culture" ~ "languages",
    subject == "Computer Science A" ~ "stem",
    subject == "Computer Science Principles" ~ "stem",
    subject == "Economics: Macroeconomics" ~ "stem",
    subject == "Economics: Microeconomics" ~ "stem",
    subject == "English Language & Composition" ~ "english",
    subject == "English Literature & Composition" ~ "english",
    subject == "Environmental Science" ~ "stem",
    subject == "European History" ~ "socialstudies",
    subject == "French Language" ~ "languages",
    subject == "German Language" ~ "languages",
    subject == "Government & Politics: Comparative" ~ "socialstudies",
    subject == "Government & Politics: United States" ~ "socialstudies",
    subject == "History of Art" ~ "arts",
    subject == "Human Geography" ~ "socialstudies",
    subject == "Italian Language & Culture" ~ "languages",
    subject == "Japanese Language & Culture" ~ "languages",
    subject == "Latin" ~ "languages",
    subject == "Latin: Literature" ~ "languages",
    subject == "Latin: Vergil" ~ "languages",
    subject == "Music Theory" ~ "arts",
    subject == "Music Theory - Aural Subscore" ~ "arts",
    subject == "Music Theory - Nonaural Subscore" ~ "arts",
    subject == "Physics 1" ~ "stem",
    subject == "Physics 2" ~ "stem",
    subject == "Physics B" ~ "stem",
    subject == "Physics C - Electricity & Magnetism" ~ "stem",
    subject == "Physics C Mechanics" ~ "stem",
    subject == "Psychology" ~ "socialstudies",
    subject == "Research" ~ "socialstudies",
    subject == "Seminar" ~ "socialstudies",
    subject == "Spanish Language" ~ "languages",
    subject == "Spanish Literature" ~ "languages",
    subject == "Statistics" ~ "stem",
    subject == "United States History" ~ "socialstudies",
    subject == "World History" ~ "socialstudies"
  )) %>%
  group_by(applicantid, subject_group) %>%
  summarize(ap_tests=n(),
            score_avg=mean(score)) %>%
  ungroup() %>%
  # Turn number of tests into ranges
  mutate(ap_tests=case_when(
    is.na(ap_tests) ~ 0,
    ap_tests>=1 ~ 1
  )) %>%
  # Turn score averages into ranges
  mutate(score_avg=case_when(
    score_avg>=4.5 ~ ">=4.5",
    score_avg<4 ~ "<4.5",
    TRUE ~ "Missing")) %>%
  mutate(score_avg=factor(score_avg, levels=c("Missing", "<4.5", ">=4.5"))) %>% 
  # Gotta do some weird trickery to properly indicate values for students with no APs at all
  right_join(applicants_submit_ids, by="applicantid") %>%
  mutate(subject_group=case_when(
    !is.na(subject_group.x) ~ subject_group.x,
    is.na(subject_group.x) ~ subject_group.y),
    ap_tests=case_when(
    !is.na(ap_tests) ~ ap_tests,
    is.na(ap_tests) ~ 0),
  score_avg=case_when(
    !is.na(score_avg) ~ score_avg,
    is.na(score_avg) ~ as.factor("Missing"))
  ) %>%
  # Now pivot wider to get it down to the applicant level
  pivot_wider(id_cols=applicantid, names_from=subject_group, values_from=c(ap_tests, score_avg), values_fill=list("ap_tests" = 0, "score_avg" = "Missing"))

# Process IB scores
ib_long_1 <- applicants %>%
  select(applicantid, contains("IB_subject") & contains("score")) %>%
  pivot_longer(cols=contains("score"), names_to="testnum", names_pattern="IB_subject(.+)_score", values_to="score")

ib_long_2 <- applicants %>%
  select(applicantid, contains("IB_subject") & ends_with("subject")) %>%
  pivot_longer(cols=ends_with("subject"), names_to="testnum", names_pattern="IB_subject(.+)_subject", values_to="subject")

# Make student crosswalk to incorporate students without IB tests
applicants_submit_ids2 <- applicants %>%
  select(applicantid)

ib_long <- ib_long_1 %>%
  left_join(ib_long_2, by=c("applicantid", "testnum")) %>%
  filter(!is.na(score)) %>% 
  group_by(applicantid) %>%
  summarize(ib_tests=n(),
            ib_avg=mean(score)) %>%
  ungroup() %>%
  mutate(
    num_ib_tests=case_when(
      is.na(ib_tests) ~ "0",
      ib_tests==1 ~ "1",
      ib_tests>1 ~ ">1"),
    avg_ib_score=case_when(
      is.na(ib_avg) ~ "Missing",
      ib_avg==7 ~ "7",
      ib_avg<7 & ib_avg>=5 ~ "5-6.99",
      ib_avg<5 ~ "<5"
    )) %>%
  mutate(
    num_ib_tests=factor(num_ib_tests, levels=c("0", "1", ">1")),
    avg_ib_score=factor(avg_ib_score, levels=c("Missing", "7", "5-6.99", "<5"))) %>%
  select(applicantid, num_ib_tests, avg_ib_score) %>%
  right_join(applicants_submit_ids2, by="applicantid") %>%
  mutate(
    num_ib_tests=case_when(
      !is.na(num_ib_tests) ~ num_ib_tests,
      is.na(num_ib_tests) ~ as.factor("0")),
    avg_ib_score=case_when(
      !is.na(avg_ib_score) ~ avg_ib_score,
      is.na(avg_ib_score) ~ as.factor("Missing")),
    any_ib=case_when(
      num_ib_tests!="0" ~ 1,
      TRUE ~ 0
    )) %>%
  select(applicantid, any_ib)


# Process application profiles
application_profiles_tmp <- applications %>%
  filter(application_submittedflag==1) %>%
  # Create a conjoint test score average for the school using concordance tables from ACT: https://www.act.org/content/dam/act/unsecured/documents/ACT-SAT-Concordance-Tables.pdf
  # Because of varying granularity, start with SAT top bin (more granular than 33-36 group for ACT) before moving through ACT (more granular)
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
    early_app=case_when(
      deadline_decisiontype=="Early Action" ~ 1,
      deadline_decisiontype=="Early Action II" ~ 1,
      deadline_decisiontype=="Early Admission (juniors only)" ~ 1,
      deadline_decisiontype=="Early Decision" ~ 1,
      deadline_decisiontype=="Early Decision II" ~ 1,
      deadline_decisiontype=="Restrictive Early Action" ~ 1,
      TRUE ~ 0)) %>%
  # Get back down to the applicant-level characteristics
  group_by(applicantid) %>%
  summarize(
    total_apps=n(),
    total_missing_satbin=sum(test_miss),
    prop_miss=total_missing_satbin/total_apps,
    total_nonmiss=total_apps-total_missing_satbin,
    satbin=mean(test_bin, na.rm=TRUE),
    # Note that we don't want to give an satbin when most of the colleges a student's applying to is missing data
    satbin=case_when(
      prop_miss>0.5 & total_nonmiss<2 ~ as.numeric(NA),
      !is.na(satbin) ~ satbin),
    early_ratio=sum(early_app)/total_apps,
    any_early=case_when(
      early_ratio>0 ~ 1,
      TRUE ~ 0),
    early_only=case_when(
      early_ratio==1 ~ 1,
      TRUE ~ 0)) %>%
  ungroup() %>%
  select(applicantid, total_apps, satbin, any_early, early_only) %>%
  # Get the satbin variable into quintiles
  mutate(
    satbin_quintile=ntile(satbin, 5),
    satbin_quintile=case_when(
      is.na(satbin_quintile) ~ "Missing",
      !is.na(satbin_quintile) ~ as.character(satbin_quintile)),
    satbin_quintile=factor(satbin_quintile,
                         levels=c("Missing", "1", "2", "3", "4", "5")),
    total_apps=case_when(
      total_apps>=1 & total_apps<=3 ~ "1-3",
      total_apps>=4 & total_apps<=7 ~ "4-7",
      total_apps>=8 ~ ">=8"
    )
  ) %>% 
  mutate(total_apps=factor(total_apps, levels=c("1-3", "4-7", ">=8"))) %>%
  # Rename it to not get confusing with their actual SAT score quintiles
  rename(app_selectivity_quintile=satbin_quintile) %>%
  select(-satbin)


# Handle missing data in cases where applicants didn't submit any applications
application_profiles <- applicants %>%
  select(applicantid) %>%
  left_join(application_profiles_tmp, by="applicantid") %>%
  mutate(
    any_early=case_when(
      is.na(any_early) ~ as.double(0),
      TRUE ~ any_early),
    early_only=case_when(
      is.na(early_only) ~ as.double(0),
      TRUE ~ early_only))
application_profiles$app_selectivity_quintile[is.na(application_profiles$app_selectivity_quintile)] <- "Missing"

# Process extracurriculars
extracurriculars_long <- extracurriculars %>%
  # Error here is just due to extra separator for continue_college
  pivot_longer(cols=starts_with("activity_"), names_to=c("activitynum", ".value"), names_sep="_", names_prefix="activity_") %>%
  filter(!is.na(type)) %>%
  mutate(
    hours=as.numeric(hours),
    week=as.numeric(week),
    years=str_count(grade, ",")+1,
    total_hours=hours*week*years)

dict <- dictionary(
  list(
    leadership = c("captain", "president", "leader", "vice", "founder", "council", "leadership", "secretary", "manager", 
                   "editor", "treasurer", "head", "ambassador", "founder", "representative", "lead", "director", "officer",
                   "committee", "board", "chair", "coordinator", "organizer", "chief", "executive", "delegate", "vp",
                   "founding", "owner", "management", "senator", "supervisor", "captian", "ceo", "leading", "cabinet",
                   "facilitator", "planner", "senate", "producer", "chairperson", "chairman", "creator",
                   "co-president", "co-founder", "co-captain", "co-chair", "co-editor-in-chief", "editor-in-chief", "capt"),
    excellence = c("captain", "president", "editor", "director", "lead", "chair", "eagle", "award", "scholarship",
                   "scholar", "top", "producer", "solo", "soloist", "winner", "recipient", "champion", "champions", "championship",
                   "champs", "winning", "1st", "first", "olympic", "olympics",
                   "co-president", "co-founder", "co-captain", "co-chair", "co-editor-in-chief", "editor-in-chief", "capt"),
    mentorship = c("coach", "teacher", "mentor", "tutor", "counselor", "peer", "instructor", "tutoring", "babysitter", "babysitting",
                   "prefect", "trainer", "advisor", "mentorship", "teachers", "proctor", "mentors")))

extracurriculars_analyzed <- extracurriculars_long %>%
  select(applicantid, activitynum, honors) %>%
  mutate(
    doc_id=paste0(applicantid, "__", activitynum)
  ) %>%
  corpus(docid_field="doc_id", text_field="honors", unique_docnames=TRUE) %>%
  tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE) %>%
  tokens_tolower() %>%
  tokens_lookup(dictionary = dict) %>% 
  dfm() %>%
  convert(to="data.frame") %>%
  mutate(
    applicantid=as.numeric(str_split_fixed(doc_id,"__", 2)[,1]),
    activitynum=str_split_fixed(doc_id,"__", 2)[,2]) %>%
  right_join(extracurriculars_long, by=c("applicantid", "activitynum")) %>%
  mutate(
    leadership=case_when(
      leadership>1 ~ 1,
      is.na(leadership) ~ 0,
      TRUE ~ leadership
    ),
    excellence=case_when(
      excellence>1 ~ 1,
      is.na(excellence) ~ 0,
      TRUE ~ excellence
    ),
    mentorship=case_when(
      mentorship>1 ~ 1,
      is.na(mentorship) ~ 0,
      TRUE ~ mentorship
    )
  ) %>%
  select(-doc_id) %>%
  # Catch some very specific edge-cases of false positives
  mutate(
    leadership=case_when(
      TRUE ~ leadership
    ),
    excellence=case_when(
      str_detect(str_to_lower(honors), "base") & str_detect(str_to_lower(honors), "1st") ~ 0,
      str_detect(str_to_lower(honors), "base") & str_detect(str_to_lower(honors), "first") ~ 0,
      str_detect(str_to_lower(honors), "grade") & str_detect(str_to_lower(honors), "1st") ~ 0,
      str_detect(str_to_lower(honors), "grade") & str_detect(str_to_lower(honors), "first") ~ 0,
      str_detect(str_to_lower(honors), "eagle") & str_detect(str_to_lower(honors), "american") ~ 0,
      str_detect(str_to_lower(honors), "olympic") & str_detect(str_to_lower(honors), "volunteer") ~ 0,
      TRUE ~ excellence
    ),
    mentorship=case_when(
      str_detect(str_to_lower(honors), "child") & str_detect(str_to_lower(honors), "care") ~ 1,
      str_detect(str_to_lower(honors), "teach") & str_detect(str_to_lower(honors), "assistant") ~ 1,
      TRUE ~ mentorship
    )
  ) %>%
  mutate(
    type_group=case_when(
      type == "Academic" ~ "Academic",
      type == "Career Oriented" ~ "Career",
      type == "Dance" ~ "Arts",
      type == "Foreign Exchange" ~ "Other",
      type == "Junior R.O.T.C." ~ "Career",
      type == "Other Club/Activity" ~ "Other",
      type == "School Spirit" ~ "Community Service",
      type == "Theater/Drama" ~ "Arts",
      type == "Art" ~ "Arts",
      type == "Community Service (Volunteer)" ~ "Community Service",
      type == "Debate/Speech" ~ "Academic",
      type == "Foreign Language" ~ "Other",
      type == "LGBT" ~ "Other",
      type == "Religious" ~ "Other",
      type == "Science/Math" ~ "Academic",
      type == "Work (Paid)" ~ "Career",
      type == "Athletics: Club" ~ "Athletics",
      type == "Computer/Technology" ~ "Academic",
      type == "Environmental" ~ "Other",
      type == "Internship" ~ "Career",
      type == "Music: Instrumental" ~ "Arts",
      type == "Research" ~ "Academic",
      type == "Social Justice" ~ "Other",
      type == "Athletics: JV/Varsity" ~ "Athletics",
      type == "Cultural" ~ "Cultural",
      type == "Family Responsibilities" ~ "Other",
      type == "Journalism/Publication" ~ "Academic",
      type == "Music: Vocal" ~ "Arts",
      type == "Robotics" ~ "Academic",
      type == "Student Govt./Politics" ~ "Other"
    ))

type_groups <- c("Academic", "Career", "Arts", "Other", "Community Service", "Athletics")
type_names <- c("academic", "career", "arts", "other", "service", "athletics")

extra_dummies <- function(df, characteristicname, typegroupname) {
  anyvarname <- paste0("any_", typegroupname)
  df <- mutate(df, "{characteristicname}_{typegroupname}" := !!as.symbol(characteristicname) * !!as.symbol(anyvarname))
}

for (i in 1:length(type_groups)) {
  any_varname <- paste0("any_", type_names[i])
  extracurriculars_analyzed[[any_varname]][extracurriculars_analyzed$type_group==type_groups[i]] <- 1
  extracurriculars_analyzed[[any_varname]][extracurriculars_analyzed$type_group!=type_groups[i]] <- 0
  
  extracurriculars_analyzed <- extra_dummies(extracurriculars_analyzed, "leadership", type_names[i])
  extracurriculars_analyzed <- extra_dummies(extracurriculars_analyzed, "excellence", type_names[i])
  extracurriculars_analyzed <- extra_dummies(extracurriculars_analyzed, "mentorship", type_names[i])
}

extracurriculars_output <- extracurriculars_analyzed %>%
  group_by(applicantid) %>%
  summarize(across(starts_with(c("any_", "leadership_", "excellence_", "mentorship_")), max),
            total_leadership=sum(leadership),
            total_excellence=sum(excellence),
            total_mentorship=sum(mentorship),
            total_activities=n()) %>%
  ungroup() %>%
  right_join(applicants_submit_ids2, by="applicantid") %>%
  mutate(across(.cols = !applicantid, ~ case_when(
    is.na(.) ~ as.double(0),
    TRUE ~ as.double(.),
  ))) %>%
  # Get rid of mentorship-activity groupings since too sparse
  select(-starts_with("mentorship_"))

# Merge all the separated covariate datasets into a single output dataset
output <- applicants %>%
  select(applicantid) %>%
  left_join(demographics, by="applicantid") %>%
  left_join(classrank_gpa, by="applicantid") %>%
  left_join(toefl, by="applicantid") %>%
  left_join(application_profiles, by="applicantid") %>%
  left_join(sat_act, by="applicantid") %>%
  left_join(sat_subjects, by="applicantid") %>%
  left_join(ap_long, by="applicantid") %>%
  left_join(ib_long, by="applicantid") %>%
  left_join(extracurriculars_output, by="applicantid")

# Save the dataset
write_fst(output, path=file.path(project_data, paste0(fileprefix, dataset_group, '_applicants_cleaned', filesuffix, '.fst')))

# Process recommendations sending data
recssent_merge <- recssent %>%
  mutate(recsent=1)

institution_apps <- applications %>%
  filter(application_submittedflag==1) %>%
  left_join(recssent_merge, by=c("applicantid", "season", "member_idmasked")) %>%
  filter(recsent==1)

write_fst(institution_apps, path=file.path(project_data, paste0(fileprefix, dataset_group, '_institution_apps', filesuffix, '.fst')))

# Clear out the memory
rm(all_crosswalk, ap_long, ap_long_1, ap_long_2, applicants, applicants_crosswalk,
   applicants_submit_ids, applicants_submit_ids2, application_profiles,
   application_profiles_tmp, applications, classrank_gpa, demographics, extracurriculars,
   extracurriculars_analyzed, extracurriculars_long, extracurriculars_output, feewaiver,
   ib_long, ib_long_1, ib_long_2, institution_apps, invitations_crosswalk, output, recssent,
   recssent_merge, sat_act, sat_subjects, teachers, teachers_crosswalk, texts, toefl,
   type_groups, type_names, extra_dummies, dict, any_varname)
