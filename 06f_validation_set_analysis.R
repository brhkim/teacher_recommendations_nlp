################################################################################
#  
#   Name: 99_validation_set_generator.R
#   
#   Purpose: Pull stratified random sample of cases from sentiment analysis
#   and topic modeling output to conduct human-algorithm validation processes
#
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

fileprefix <- "99_"

set.seed(83191)

################################################################################
#
#   #import - Load in a datasets and start taking a peek around
#
################################################################################

initials <- c("EA", "HM", "JM", "KO", "RD", "SM")

# First deal with sentiment analysis datasets
oldnames <- c("sentence_text", "sentiment_rating")

for (i in initials) {
  newnames <- c("text", paste0("score_", i))
  
  tmp1 <- read_excel(file.path(project_data, "nlp_validation", "completed_templates", paste0("06e_sentiment_validation_", i, "a.xlsx"))) %>%
    select(sentence_text, sentiment_rating) %>%
    rename_with(.fn= ~ newnames, .cols= all_of(oldnames))
  
  tmp2 <- read_excel(file.path(project_data, "nlp_validation", "completed_templates", paste0("06e_sentiment_validation_", i, "b.xlsx"))) %>%
    select(sentence_text, sentiment_rating) %>%
    rename_with(.fn = ~ newnames, .cols=all_of(oldnames))
  
  output <- tmp1 %>%
    bind_rows(tmp2)
  
  assign(value=output, x=paste0("sentiment_", i))
}

sentiment_key <- read_csv(file.path(project_data, "nlp_validation", "06e_sentiment_validation_key.csv")) %>%
  rename(text=sentence_text)


# # Now deal with topic modeling
# oldnames <- c("text", "most_prevalent", "second_most_prevalent")
# 
# for (i in initials) {
#   newnames <- c("text", paste0(i, "1"), paste0(i, "2"))
#   
#   tmp1 <- read_xlsx(file.path(validation_data, paste0("06e_topic_validation_", i, "a.xlsx"))) %>%
#     select(text, most_prevalent, second_most_prevalent) %>%
#     rename_with(~ newnames, all_of(oldnames))
#   
#   tmp2 <- read_xlsx(file.path(validation_data, paste0("06e_topic_validation_", i, "b.xlsx"))) %>%
#     select(text, most_prevalent, second_most_prevalent) %>%
#     rename_with(~ newnames, all_of(oldnames))
#   
#   output <- tmp1 %>%
#     bind_rows(tmp2)
#   
#   assign(value=output, x=paste0("topic_", i))
# }
# 
# topic_key <- read_csv(file.path(project_data, "06e_topic_validation_key.csv"),  locale = readr::locale(encoding = "windows-1252")) %>%
#   mutate(
#     highest_prop=case_when(
#       highest_prop=="crse" ~ "Course Selection and Enrollment",
#       highest_prop=="fina" ~ "Finances",
#       highest_prop=="meet" ~ "Advising Logistics",
#       highest_prop=="plan" ~ "Academic Planning",
#       highest_prop=="reso" ~ "Academic Resources"
#     ))

train_sentiment_sentence <- read_fst(path=file.path(project_data, paste0('08_train_sentiment_analytic_data_sentence', filesuffix, '.fst')))
test_sentiment_sentence <- read_fst(path=file.path(project_data, paste0('08_test_sentiment_analytic_data_sentence', filesuffix, '.fst')))


################################################################################
#
#   #merge - Merge datasets together and begin analysis
#
################################################################################


# Sentiment was stratified by: system, race, male, advisor_message (all custom only; 100 chars or more)

# Topic was stratified by: "system", "race", "male", "highest_prop" (only 10 tokens or more)

sentiment_merged <- sentiment_key %>%
  left_join(sentiment_EA, by="text") %>%
  left_join(sentiment_HM, by="text") %>%
  left_join(sentiment_JM, by="text") %>%
  left_join(sentiment_KO, by="text") %>%
  left_join(sentiment_RD, by="text") %>%
  left_join(sentiment_SM, by="text")

# 5 level: Forest, Yelp, bert
# 2 level: xlnet, albert
# 3 level: stanza, twit

train_sentiment_clean <- train_sentiment_sentence %>%
  select(sentence_text, label_predict_forest, label_predict_yelp, label_predict_xlnet,
         label_predict_albert, label_predict_stanza, label_predict_bert, label_predict_twit,
         label_predict_imdb) %>%
  distinct()

test_sentiment_clean <- test_sentiment_sentence %>% 
  select(sentence_text, label_predict_forest, label_predict_yelp, label_predict_xlnet,
         label_predict_albert, label_predict_stanza, label_predict_bert, label_predict_twit,
         label_predict_imdb) %>%
  distinct()

all_sentiment_clean <- train_sentiment_clean %>%
  bind_rows(test_sentiment_clean) %>%
  distinct()

test <- sentiment_merged %>%
  select(-label_predict_forest) %>%
  left_join(all_sentiment_clean, by=c("text"="sentence_text")) 

test_all <- test 

# algorithms_5 <- c("label_predict_forest", "label_predict_yelp", "label_predict_bert")
# 
# matrix_output <- matrix(, nrow=length(initials), ncol=length(algorithms_5))
# rownames(matrix_output) <- initials
# colnames(matrix_output) <- algorithms_5
# 
# for(i in 1:length(algorithms_5)) {
#   for(j in 1:length(initials)) {
#     algorithm <- algorithms_5[i]
#     initial <- initials[j]
#     
#     coder_var <- paste0("score_", initial)
#     
#     combined <- bind_cols(algo_scores=test[[algorithm]], coder_scores=test[[coder_var]]) %>%
#       mutate(correct=case_when(
#         algo_scores==coder_scores ~ 1,
#         TRUE ~ 0
#       ))
#     
#     output_accuracy <- combined$correct %>%
#       mean()
#     
#     matrix_output[j, i] <- output_accuracy
#   }
# }

algorithms_3 <- c("label_predict_forest", "label_predict_yelp", "label_predict_bert", "label_predict_stanza", "label_predict_twit")

disparate_accuracy_check <- function(df, algorithmname) {
  df <- df %>%
    as.data.frame()
  
  matrix_output_3 <- matrix(, nrow=length(initials), ncol=1)
  rownames(matrix_output_3) <- initials

    for(j in 1:length(initials)) {
      initial <- initials[j]
      
      coder_var <- paste0("score_", initial)
      
      combined <- bind_cols(algo_scores=df[[algorithmname]], coder_scores=df[[coder_var]]) %>%
        mutate(algo_scores=case_when(
          algo_scores==2 ~ 1,
          algo_scores==-2 ~ -1,
          TRUE ~ algo_scores
        )) %>%
        mutate(coder_scores=case_when(
          coder_scores==2 ~ 1,
          coder_scores==-2 ~ -1,
          TRUE ~ coder_scores
        )) %>%
        mutate(correct=case_when(
          algo_scores==coder_scores ~ 1,
          TRUE ~ 0
        ))
      
      output_accuracy <- combined$correct %>%
        mean()
      
      matrix_output_3[j, 1] <- output_accuracy
    }
  
  matrix_output_3 %>% mean()
}

female_0 <- test_all %>%
  filter(female==0)

female_1 <- test_all %>%
  filter(female==1)

white_1 <- test_all %>%
  filter(white==1)

black_1 <- test_all %>%
  filter(black==1)

asian_1 <- test_all %>%
  filter(asian==1)

latinx_1 <- test_all %>%
  filter(latinx==1)

datachecks <- list(test_all, female_0, female_1, white_1, black_1, asian_1, latinx_1)

labels <- c("Overall", "Female", "Male", "White", "Black", "Asian", "Latinx")
algorithm_labels <- c("Random Forest", "BERT Yelp", "BERT Products", "Stanza", "RoBERTa")

r <- 1
output <- matrix(data=NA, nrow=length(labels), ncol=length(algorithms_3))

for(dataframe in datachecks) {
  c <- 1

  for(algorithm in algorithms_3) {
    tmp <- disparate_accuracy_check(df=dataframe, algorithmname=algorithm)
    
    output[r, c] <- round(tmp, 3)
    
    c <- c + 1
  }
  
  r <- r + 1
}

observations <- c(nrow(test_all), nrow(female_0), nrow(female_1),
                  nrow(white_1), nrow(black_1), nrow(asian_1), nrow(latinx_1))

colnames(output) <- algorithm_labels
output <- output %>%
  as.data.frame() %>%
  mutate(Subset=labels, `Obs.`=observations) %>%
  relocate(Subset, `Obs.`)

kable(output, align = c("r", "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2, width="6em", border_right=T) %>%
  column_spec(1:7, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(3, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "algorithm_comparison.jpg")), zoom=3, bs_theme="default")



