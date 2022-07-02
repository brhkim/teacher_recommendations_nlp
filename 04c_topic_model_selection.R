################################################################################
#  
#   Name: 04c_topic_model_selection.R
#   
#   Purpose: Take the various topic models as searched through in 04b_topic_model_search
#   and plot their various performance metrics, eventually taking the optimal model.
#   Note that this final step of the process does require manual analyst intervention.
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "04c_"

base::load(file=file.path(project_data, paste0('04_topic_modeling_temp1', filesuffix, '.RData')))

rm(custom_dfm, custom_dfm2, clean_paras, final_dfm_wordcheck, metadata_stm, tokencount)

# heldout <- make.heldout(documents=final_stm$documents, vocab=final_stm$vocab, seed=1234567)
# base::save(heldout, file=file.path(project_data, paste0(fileprefix, 'heldout', filesuffix, '.RData')))

base::load(file=file.path(project_data, paste0(fileprefix, 'heldout', filesuffix, '.RData')))

done <- c(11, 12, 14)

for (i in done) {
  base::load(file=file.path(project_data, paste0('04b_many_models', filesuffix, "_", i, '.RData')))

  K_test <- many_models$settings$dim$K
  
	loader <- data.frame(K=K_test) %>%
		mutate(topic_model=list(many_models))

	k_result_tmp <- loader %>%
	  mutate(exclusivity = map(topic_model, exclusivity),
	         semantic_coherence = map(topic_model, semanticCoherence, final_dfm),
	         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
	         residual = map(topic_model, checkResiduals, final_dfm),
	         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
	         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
	         lbound = bound + lfact,
	         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
	  select(-topic_model) %>%
	  transmute(K,
	            `Lower bound` = lbound,
	            Residuals = map_dbl(residual, "dispersion"),
	            `Semantic coherence` = map_dbl(semantic_coherence, mean),
	            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
	  gather(Metric, Value, -K) 
	
  if (i==1) {
    k_result <- k_result_tmp
  } else {
    k_result <- k_result %>%
      bind_rows(k_result_tmp)
  }
}

base::save(k_result, file=file.path(project_data, paste0(fileprefix, 'k_results', filesuffix, '.RData')))

#base::load(file=file.path(project_data, paste0(fileprefix, 'k_results', filesuffix, '.RData')))

topic_fit_metrics <- k_result %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")
ggsave(topic_fit_metrics, file=file.path(project_output, paste0(fileprefix, 'topic_model_fit_metrics', filesuffix, '.png')), width=10, height=6)

# exclusivity <- k_result %>%
#   select(K, exclusivity, semantic_coherence) %>%
#   filter(K %in% Ktest) %>%
#   unnest(cols = c(exclusivity, semantic_coherence)) %>%
#   mutate(K = as.factor(K)) %>%
#   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(x = "Semantic coherence",
#        y = "Exclusivity",
#        title = "Comparing exclusivity and semantic coherence",
#        subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
# ggsave(exclusivity, file=file.path(project_output, paste0(fileprefix, 'topic_model_exclusivity_coherence', filesuffix, '.png')), width=10, height=6)
# 
