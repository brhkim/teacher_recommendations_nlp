################################################################################
#  
#   Name: 11_word_frequency.R
#   
#   Purpose: Analyze the DFM using penalized logistic regression to find
#   words that best predict student race
#
################################################################################

################################################################################
#
#   #import - Load in a datasets
#
################################################################################

fileprefix <- "11_"

base::load(file=file.path(project_data, paste0("04_topic_modeling_temp1", filesuffix, ".RData")))

################################################################################
#
#   #clean - Clean and organize the dataset for topic modeling
#
################################################################################

registerDoMC(cores=8)

final_dfm <- final_dfm %>%
  dfm_trim(docfreq_type="rank", min_docfreq = 1000)

predictive_words <- function(var) {
  model <- textmodel_lr(x=final_dfm, y=metadata_stm[[var]], trace.it=TRUE, parallel=TRUE)
  coefs <- coef(model, s="lambda.min")
  output_prep <- data.frame(variable=paste0(var), word = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x)
  output_prep
}

set.seed(1237)
female_predict <- predictive_words("female")
write_fst(female_predict, path=file.path(project_data, paste0(fileprefix, 'female_predict', filesuffix, '.fst')))

set.seed(1234)
white_predict <- predictive_words("white")
write_fst(white_predict, path=file.path(project_data, paste0(fileprefix, 'white_predict', filesuffix, '.fst')))

set.seed(1235)
black_predict <- predictive_words("black")
write_fst(black_predict, path=file.path(project_data, paste0(fileprefix, 'black_predict', filesuffix, '.fst')))

set.seed(1236)
asian_predict <- predictive_words("asian")
write_fst(asian_predict, path=file.path(project_data, paste0(fileprefix, 'asian_predict', filesuffix, '.fst')))

# set.seed(1238)
# latinx_predict <- predictive_words("latinx")
# write_fst(latinx_predict, path=file.path(project_data, paste0(fileprefix, 'latinx_predict', filesuffix, '.fst')))

# set.seed(1239)
# firstgen_predict <- predictive_words("firstgen")
# write_fst(firstgen_predict, path=file.path(project_data, paste0(fileprefix, 'firstgen_predict', filesuffix, '.fst')))
# 
# set.seed(1240)
# feewaiver_predict <- predictive_words("feewaiver")
# write_fst(feewaiver_predict, path=file.path(project_data, paste0(fileprefix, 'feewaiver_predict', filesuffix, '.fst')))

# set.seed(1241)
# international_predict <- predictive_words("international")
# write_fst(international_predict, path=file.path(project_data, paste0(fileprefix, 'international_predict', filesuffix, '.fst')))

docfreq <- data.frame(word=featnames(final_dfm), featfreq=featfreq(final_dfm), docfreq=docfreq(final_dfm)) %>%
  mutate(featprop=featfreq/sum(featfreq(final_dfm)), docprop=docfreq/ndoc(final_dfm))

head_tail <- function(df) {
  head <- df %>%
    filter(word!="(Intercept)") %>%
    arrange(desc(coefficient)) %>%
    head(20) %>%
    mutate(place=row_number(),
           type="top") %>%
    left_join(docfreq, by="word") %>%
    select(word, coefficient, place) %>%
    relocate(place, word, coefficient) %>%
    rename(top_word=word, top_coefficient=coefficient)
  
  tail <- df %>%
    filter(word!="(Intercept)") %>%
    arrange(coefficient)%>%
    head(20) %>%
    mutate(place=row_number(),
           type="bottom") %>%
    left_join(docfreq, by="word") %>%
    select(word, coefficient, place) %>%
    relocate(place, word, coefficient) %>%
    rename(bottom_word=word, bottom_coefficient=coefficient)
  
  output <- head %>%
    left_join(tail, by="place") %>%
    mutate(top_coefficient=round(top_coefficient, 3),
           bottom_coefficient=round(bottom_coefficient, 3))
}

female_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'female_predict', filesuffix, '.fst')))
white_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'white_predict', filesuffix, '.fst')))
black_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'black_predict', filesuffix, '.fst')))
asian_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'asian_predict', filesuffix, '.fst')))
latinx_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'latinx_predict', filesuffix, '.fst')))
firstgen_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'firstgen_predict', filesuffix, '.fst')))
feewaiver_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'feewaiver_predict', filesuffix, '.fst')))
international_predict <- read_fst(path=file.path(project_data, paste0(fileprefix, 'international_predict', filesuffix, '.fst')))


female_arranged <- head_tail(female_predict)
white_arranged <- head_tail(white_predict)
black_arranged <- head_tail(black_predict)
asian_arranged <- head_tail(asian_predict)
# latinx_arranged <- head_tail(latinx_predict)
# firstgen_arranged <- head_tail(firstgen_predict)
# feewaiver_arranged <- head_tail(feewaiver_predict)
# international_arranged <- head_tail(international_predict)


kable(female_arranged, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'",
      col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
  column_spec(1, width="1em", bold = T, border_right=T) %>%
  column_spec(3, border_right=T) %>%
  column_spec(2:5, width="3em") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_female.jpg")), zoom=3, bs_theme="default")

kable(white_arranged, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'",
      col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
  column_spec(1, width="1em", bold = T, border_right=T) %>%
  column_spec(3, border_right=T) %>%
  column_spec(2:5, width="3em") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_white.jpg")), zoom=3, bs_theme="default")

kable(black_arranged, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'",
      col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
  column_spec(1, width="1em", bold = T, border_right=T) %>%
  column_spec(3, border_right=T) %>%
  column_spec(2:5, width="3em") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_black.jpg")), zoom=3, bs_theme="default")

kable(asian_arranged, align = c("r", "c", "c", "c", "c"), 
      format="html", row.names=FALSE, table.attr = "style='width:10%;'",
      col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
  column_spec(1, width="1em", bold = T, border_right=T) %>%
  column_spec(3, border_right=T) %>%
  column_spec(2:5, width="3em") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
  save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_asian.jpg")), zoom=3, bs_theme="default")

# kable(latinx_arranged, align = c("r", "c", "c", "c", "c"), 
#       format="html", row.names=FALSE, table.attr = "style='width:10%;'",
#       col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
#   column_spec(1, width="1em", bold = T, border_right=T) %>%
#   column_spec(3, border_right=T) %>%
#   column_spec(2:5, width="3em") %>%
#   kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
#   add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
#   save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_latinx.jpg")), zoom=3, bs_theme="default")
# 
# kable(firstgen_arranged, align = c("r", "c", "c", "c", "c"), 
#       format="html", row.names=FALSE, table.attr = "style='width:10%;'",
#       col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
#   column_spec(1, width="1em", bold = T, border_right=T) %>%
#   column_spec(3, border_right=T) %>%
#   column_spec(2:5, width="3em") %>%
#   kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
#   add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
#   save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_firstgen.jpg")), zoom=3, bs_theme="default")
# 
# kable(feewaiver_arranged, align = c("r", "c", "c", "c", "c"), 
#       format="html", row.names=FALSE, table.attr = "style='width:10%;'",
#       col.names=c("Rank", "Keyword", "Coefficient", "Keyword", "Coefficient")) %>%
#   column_spec(1, width="1em", bold = T, border_right=T) %>%
#   column_spec(3, border_right=T) %>%
#   column_spec(2:5, width="3em") %>%
#   kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
#   add_header_above(c(" " = 1, "Positively Predictive" = 2, "Negatively Predictive" = 2)) %>%
#   save_kable(file.path(project_output, paste0(fileprefix, "wordfreq_feewaiver.jpg")), zoom=3, bs_theme="default")
# 


checker <- clean_paras %>%
  filter(str_detect(paragraph_text, "Hosa"))
