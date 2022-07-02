################################################################################
#  
#   Name: 10b_regression_tables.R
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

fileprefix <- "10b_"
dataset_group <- "test"

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

sentiment_averages <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_sentiment_averages.csv'))) %>%
  process_sentiment_averages()
names(sentiment_averages) <- sentiment_models

sentiment_averages_inst <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_sentiment_averages_inst.csv'))) %>%
  process_sentiment_averages()
names(sentiment_averages_inst) <- sentiment_models

process_sentiment_table <- . %>%
  filter(`Dependent Var.:` %in% c(vars_of_interest_a, vars_of_interest_b))

sentiment_landscape <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_sentiment_regressions_landscape.csv')), skip=1) %>%
  process_sentiment_table()
names(sentiment_landscape) <- sentiment_models
for (i in 1:length(vars_of_interest_a)) {
  sentiment_landscape$`Variable`[sentiment_landscape$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
sentiment_landscape <- bind_rows(sentiment_averages, sentiment_landscape)


sentiment_teacher <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_sentiment_regressions_teacher.csv')), skip=1) %>%
  process_sentiment_table()
names(sentiment_teacher) <- sentiment_models
for (i in 1:length(vars_of_interest_b)) {
  sentiment_teacher$`Variable`[sentiment_teacher$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
sentiment_teacher <- bind_rows(sentiment_averages, sentiment_teacher)


sentiment_institution <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_sentiment_regressions_institution.csv')), skip=1) %>%
  process_sentiment_table()
names(sentiment_institution) <- sentiment_models
for (i in 1:length(vars_of_interest_b)) {
  sentiment_institution$`Variable`[sentiment_institution$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
sentiment_institution <- bind_rows(sentiment_averages_inst, sentiment_institution)


# Print out the descriptives table nicely
kable(sentiment_landscape, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_landscape.jpg")), zoom=3, bs_theme="default")

kable(sentiment_teacher, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_teacher.jpg")), zoom=3, bs_theme="default")

kable(sentiment_institution, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(4, border_right=T) %>%
  column_spec(2:5, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_sentiment_institution.jpg")), zoom=3, bs_theme="default")



supertopic_cutpoints <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_supertopic_cutpoints.csv'))) %>%
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
  filter(`Dependent Var.:` %in% c(vars_of_interest_a, vars_of_interest_b))


topic_landscape <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_topic_regressions_landscape.csv')), skip=1)  %>%
  process_topic_table
names(topic_landscape) <- topic_models
for (i in 1:length(vars_of_interest_a)) {
  topic_landscape$`Variable`[topic_landscape$`Variable`==vars_of_interest_a[i]] <- var_labels_a[i]
}
topic_landscape <- bind_rows(supertopic_cutpoints, topic_landscape)

topic_teacher <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_topic_regressions_teacher.csv')), skip=1)  %>%
  process_topic_table
names(topic_teacher) <- topic_models
for (i in 1:length(vars_of_interest_b)) {
  topic_teacher$`Variable`[topic_teacher$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
topic_teacher <- bind_rows(supertopic_cutpoints, topic_teacher)

topic_institution <- read_csv(file=file.path(project_output, paste0('10_', dataset_group, '_topic_regressions_institution.csv')), skip=1)  %>%
  process_topic_table
names(topic_institution) <- topic_models
for (i in 1:length(vars_of_interest_b)) {
  topic_institution$`Variable`[topic_institution$`Variable`==vars_of_interest_b[i]] <- var_labels_b[i]
}
topic_institution <- bind_rows(supertopic_cutpoints, topic_institution)


kable(topic_landscape, align = c("r", "c", "c", "c", "c",
                                       "c", "c", "c", "c",
                                 "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_landscape.jpg")), zoom=3, bs_theme="default")

kable(topic_teacher, align = c("r", "c", "c", "c", "c",
                                 "c", "c", "c", "c",
                                 "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_teacher.jpg")), zoom=3, bs_theme="default")


kable(topic_institution, align = c("r", "c", "c", "c", "c",
                                 "c", "c", "c", "c",
                                 "c", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'") %>%
  column_spec(1, width="6em", bold = T, border_right=T) %>%
  column_spec(2:14, width="2em") %>%
  row_spec(1, extra_css="border-bottom: 1px solid") %>%
  row_spec(4, extra_css="border-bottom: 1px solid") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  save_kable(file.path(project_output, paste0(fileprefix, dataset_group, "_topic_institution.jpg")), zoom=3, bs_theme="default")

