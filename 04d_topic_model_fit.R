################################################################################
#  
#   Name: 04d_topic_model_fit.R
#   
#   Purpose: Take the selected topic model and apply it to the training data
#   for preliminary analysis purposes
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "04d_"

base::load(file=file.path(project_data, paste0("04_topic_modeling_temp1", filesuffix, ".RData")))
base::load(file=file.path(project_data, paste0("04_topic_model_k0", filesuffix, ".RData")))

################################################################################
#
#   #modelselection - Pick a model and prep the output for interpretation
#
################################################################################

topic_model <- topic_model_k0

colnames_reset <- c(1:20, "topic", "metric")
labeledtopics <- labelTopics(topic_model, n=20)
labeloutput_prob <- as.data.frame(labeledtopics[1]) %>%
  mutate(topic=rownames(.),
         metric="prob")
colnames(labeloutput_prob) <- colnames_reset

labeloutput_frex <- as.data.frame(labeledtopics[2]) %>%
  mutate(topic=rownames(.),
         metric="frex")
colnames(labeloutput_frex) <- colnames_reset

labeloutput <- bind_rows(labeloutput_prob, labeloutput_frex) %>%
  mutate(description="",
         supertopic="",
         topic=as.numeric(topic)) %>%
  arrange(topic) %>%
  relocate(topic, description, supertopic, metric)

write_csv(labeloutput, file.path(project_data, paste0(fileprefix, 'topic_codes_empty', filesuffix, '.csv')))

td_beta <- tidy(topic_model)

words_by_topic <- td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
ggsave(filename=paste0(fileprefix, "words_by_topic", filesuffix, ".png"), plot=words_by_topic, path=file.path(project_output), width=30, height=18)

td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = names(final_stm$documents)) %>%
  left_join(tokencount, by=c("document"="paraid"))

# Set up output to show word frequency by topics
top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols=c(terms))

gamma_terms <- td_gamma %>%
  mutate(words=gamma*tokens) %>%
  group_by(topic) %>%
  summarise(words=sum(words)) %>%
  arrange(desc(words)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, words))

topic_prevalence <- gamma_terms %>%
  ggplot(aes(topic, words, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 100, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 2500000)) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = "Number of Word Occurrences",
       title = "Topics by Frequency of Word Occurrences in the Teacher Recommendation Data",
       subtitle = "Annotated with each topic's highest probability words")
ggsave(filename=paste0(fileprefix, "topic_prevalence", filesuffix, ".png"), plot=topic_prevalence, path=file.path(project_output), width=30, height=18)

# Set up graphs to show word frequency by supertopics
# gamma_terms2 <- td_gamma %>%
#   mutate(supertopic=case_when(topic %in% c(7, 19, 20, 24) ~ "Scheduling Meetings",
#                             topic %in% c(4, 11, 23) ~ "Course Registration",
#                             topic %in% c(10, 17) ~ "Financial Aid",
#                             topic %in% c(2, 3, 6, 13, 14, 16, 18, 22) ~ "General Academic \nSupport and Planning",
#                             TRUE ~ "Other")) %>%
#   mutate(words=gamma*tokens) %>%
#   group_by(topic) %>%
#   summarise(words=sum(words),
#             supertopic=first(supertopic)) %>%
#   arrange(desc(words)) %>%
#   left_join(top_terms, by = "topic") %>%
#   mutate(topic = paste0("Topic ", topic),
#          topic = reorder(topic, words))
# 
# gamma_terms2 %>%
#   ggplot(aes(topic, words, label = terms, fill = supertopic)) +
#   geom_col() +
#   geom_text(hjust = 0, nudge_y = 100, size = 3) +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0, 23000)) +
#   scale_fill_colorblind() +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 16),
#         plot.subtitle = element_text(size = 13)) +
#   labs(x = NULL, y = "Number of Word Occurrences", fill="Supertopic",
#        title = "Topics and Supertopics by Frequency of Word Occurrences in the N2FL Data",
#        subtitle = "Annotated with each topic's highest probability words")
# ggsave(file.path(project_output, paste0('topic_model_supertopics_prevalence', filesuffix, '.png')), width=10, height=6)

corroutput <- topicCorr(topic_model)

datafiles2 <- c("corroutput", "topic_model", "td_beta", "td_gamma")
base::save(list=datafiles2, file=file.path(project_data, paste0(fileprefix, 'topic_modeling_temp2', filesuffix, '.RData')))


################################################################################
#
#   #output - Get the output
#
################################################################################

# base::load(file=file.path(project_data, paste0('04_topic_modeling_temp1', filesuffix, '.RData')))
# base::load(file=file.path(project_data, paste0(fileprefix, 'topic_modeling_temp2', filesuffix, '.RData')))

paragraph_counts <- tokencount %>%
  left_join(clean_paras, by="paraid") %>%
  mutate(letterid=paste0(str_split_fixed(paraid,"__", 3)[,1], "__", str_split_fixed(paraid,"__", 3)[,2])) %>%
  select(letterid) %>%
  group_by(letterid) %>%
  summarize(total_paragraphs=n()) %>%
  ungroup() %>%
  mutate(letterid=as.character(letterid))

# meaningful_paragraphs <- tokencount %>%
#   left_join(clean_paras, by=c("docnames"="paraid")) %>%
#   mutate(differential_count=rough_wordcount-tokens,
#          differential_prop=(rough_wordcount-tokens)/rough_wordcount) %>%
#   mutate(differential_prop=case_when(differential_prop<0 ~ 0,
#                                      TRUE ~ differential_prop)) %>%
#   mutate(
#     meaningful_para=case_when(
#       differential_prop < 0.85 & rough_wordcount>=20 ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   select(docnames, meaningful_para) %>%
#   rename(document=docnames)

output_prep <- td_gamma %>%
  #filter(document!="__") %>%
  mutate(topic_tokens=gamma*tokens) %>%
  pivot_wider(id_cols=document, names_from=topic, names_prefix="topic_tokens_", values_from=topic_tokens) %>%
  mutate(letterid=paste0(str_split_fixed(document,"__", 3)[,1], "__", str_split_fixed(document,"__", 3)[,2]))

output <- output_prep %>%
  left_join(tokencount, by=c("document" = "paraid")) %>%
  group_by(letterid) %>%
  summarize(across((!document), sum)) %>%
  ungroup() %>%
  mutate(across(starts_with("topic_tokens"), ~replace_na(.x, 0)))

# This is how to do it if you want having at least one paragraph primarily about topic X

# set.seed(1234)

# output_prep <- td_gamma %>%
#   group_by(document) %>%
#   arrange(desc(gamma), .by_group=TRUE) %>%
#   mutate(place=row_number()) %>%
#   slice_head(n=2) %>%
#   ungroup() %>%
#   pivot_wider(id_cols=document, names_from=place, values_from=c(topic, gamma)) %>%
#   left_join(meaningful_paragraphs, by="document") %>%
#   mutate(topic_diff=gamma_1-gamma_2,
#          counter=case_when(
#            gamma_1>=0.1 & topic_diff>=0.05 & meaningful_para==1 ~ 1,
#            gamma_1>=0.05 & topic_diff>=0.01 & meaningful_para==1 ~ 1,
#            TRUE ~ 0
#          ),
#          topic_1=factor(topic_1, levels=c(1:80)),
#          invitationid=str_split_fixed(document,"_", 2)[,1]
#   ) %>%
#   filter(counter==1)
# 
# dummy_prep <- output_prep %>%
#   select(topic_1) %>%
#   dummyVars(data=., " ~ .")
# dummy <- data.frame(predict(dummy_prep, newdata=output_prep))
# 
# output <- output_prep %>%
#   select(invitationid) %>%
#   bind_cols(dummy) %>%
#   group_by(invitationid) %>%
#   summarize(across(everything(), max)) %>%
#   ungroup() %>%
#   right_join(paragraph_counts, by="invitationid") %>%
#   mutate(across(starts_with("topic_1"), ~replace_na(.x, 0)))

write_fst(output, file.path(project_data, paste0(fileprefix, 'train_topics_cleaned', filesuffix, '.fst')))

rm(clean_paras, final_dfm_wordcheck, labeloutput, labeloutput_frex, labeloutput_prob, metadata_stm,
   td_beta, td_gamma, tokencount, top_terms, colnames_reset, corroutput, custom_dfm, custom_dfm2,
   final_dfm, final_stm, labeledtopics, topic_model, topic_model_k0, topic_prevalence, words_by_topic,
   output_prep, output, gamma_terms, paragraph_counts)

