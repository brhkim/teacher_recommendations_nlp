################################################################################
#  
#   Name: 03_letter_cleaning.R
#   
#   Purpose: Run logic checks on text data line-by-line to remove extraneous
#   text data, such as headers, letterhead text, and so on, and the produce
#   cleaned datasets at each level (letter, paragraph, sentence) for varying
#   NLP analyses
#
################################################################################

################################################################################
#
#   #import - Load in the relevant datasets
#
################################################################################

fileprefix <- "03_"

texts <- read_fst(file.path(project_data, paste0("01_", dataset_group, '_texts', filesuffix, '.fst'))) %>%
  mutate(letterid=paste0(invitationid, "__", year_2019))


################################################################################
#
#   #clean - Clean and organize the text data
#
################################################################################

texts_clean <- texts %>%
  # First generate some basic indicator variables to assess contents of each line
  mutate(commas=str_count(datavalscrubbedtext, ","),
         periods=str_count(datavalscrubbedtext, "\\."),
         length=str_length(datavalscrubbedtext),
         upper_length=str_count(datavalscrubbedtext, "[:upper:]"),
         upper_prop=upper_length/length,
         last_char=str_sub(datavalscrubbedtext, start=-1),
         address_token=str_detect(datavalscrubbedtext, "<ADDRESS>"),
         name_token=str_detect(datavalscrubbedtext, "<NAME>"),
         phone_token=str_detect(datavalscrubbedtext, "<PHONE>"),
         email_token=str_detect(datavalscrubbedtext, "<EMAIL>"),
         index_prop=index/maxindex,
         numbers=str_count(datavalscrubbedtext, "[:digit:]"),
         number_prop=numbers/length) %>%
  # Now check for any completely duplicated lines
  group_by(letterid, datavalscrubbedtext) %>%
  mutate(dupes=n()) %>%
  ungroup() %>%
  # Now check for formal salutation-esque lines and closing-esque lines ONLY among non-repeated
  # lines in the front- or back-half of each letter by line count
  mutate(salutation_test=case_when(dupes==1 & index_prop<0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 5), "dear ") & (last_char==":" | last_char==","| last_char==";") & commas<=1 & periods<1 ~ 1,
                                   dupes==1 & index_prop<0.5 & str_detect(str_sub(datavalscrubbedtext, 1, 10), "To ") & (last_char==":" | last_char=="," | last_char==";") & commas<=1 & periods<1 ~ 1,
                                   dupes==1 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "to whom it may") ~ 1,
                                   dupes==1 & index_prop<0.5 & (str_detect(datavalscrubbedtext, "Recommendation") | str_detect(datavalscrubbedtext, "RECOMMENDATION") | str_detect(datavalscrubbedtext, "Evaluation") | 
                                                                  str_detect(datavalscrubbedtext, "EVALUATION") | str_detect(datavalscrubbedtext, "Reference") | str_detect(datavalscrubbedtext, "REFERENCE")) & length<80 & (commas+periods<=1) ~ 1,
                                   dupes==1 & index_prop<0.5 & (str_detect(str_sub(datavalscrubbedtext, 1, 3), "To:") | str_detect(str_sub(datavalscrubbedtext, 1, 4), "For:") | str_detect(str_sub(datavalscrubbedtext, 1, 3), "TO:") | 
                                                                  str_detect(str_sub(datavalscrubbedtext, 1, 4), "FOR:") | str_detect(str_sub(datavalscrubbedtext, 1, 3), "RE:") | str_detect(str_sub(datavalscrubbedtext, 1, 3), "re:") | 
                                                                  str_detect(str_sub(datavalscrubbedtext, 1, 3), "Re:") | str_detect(str_sub(datavalscrubbedtext, 1, 9), "Subject:") | str_detect(str_sub(datavalscrubbedtext, 1, 9), "SUBJECT:")) ~ 1,
                                   TRUE ~ 0),
         closing_test=case_when(dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "yours truly") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "yours") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<10 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "sincerely") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "sincerely yours") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "yours sincerely") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "most sincerely") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 10), "thanks")  & length<10 &  commas<=1 & periods<=1 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "thank you")  & commas<=1 & periods<=1 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "thank you again") & commas<=1 & periods<=1 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "all the best") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "all my best") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "best") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<10 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "best wishes") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "my best") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "take care") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "respectfully") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "yours respectfully") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "respectfully yours") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "most respectfully") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "faithfully") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "yours faithfully") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "most faithfully") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 20), "faithfully yours") & commas<=1 & periods<1 & length<20 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "regards") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "best regards") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "kind regards") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "warmly") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                dupes==1 & index_prop>0.5 & str_detect(str_sub(str_to_lower(datavalscrubbedtext), 1, 15), "kindly") & (last_char=="," | last_char==";" | last_char==":") & commas<=1 & periods<1 & length<15 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(letterid) %>%
  # Get letter-level characteristics
  mutate(salutation_any=max(salutation_test),
         closing_any=max(closing_test),
         total_characters=sum(length),
         salutation_last_tmp=ifelse(salutation_test==1, index, NA),
         closing_last_tmp=ifelse(closing_test==1, index, NA),
         salutation_last=max(salutation_last_tmp, na.rm=TRUE),
         closing_last=max(closing_last_tmp, na.rm=TRUE)) %>%
  fill(salutation_last, .direction="downup") %>%
  fill(closing_last, .direction="downup") %>%
  # In the case when there is no salutation detected, fill in 0
  # In the case when there is no closing detected, fill in max line number plus one (to work out with the later logic)
  mutate(salutation_last=case_when(salutation_last!=-Inf ~ salutation_last,
                                   salutation_last==-Inf ~ 0),
         closing_last=case_when(closing_last!=-Inf ~ closing_last,
                                closing_last==-Inf ~ maxindex+1)) %>%
  # Catch weird instances where letter order seems reversed or flipped somehow
  mutate(flipped=case_when(salutation_last>=closing_last | (salutation_last/maxindex>0.8 & maxindex>=10) | (closing_last/maxindex<0.1 & maxindex>=10) ~ 1,
                           TRUE ~ 0),
         salutation_last=case_when(flipped==0 ~ salutation_last,
                                   flipped==1 ~ 0),
         closing_last=case_when(flipped==0 ~ closing_last,
                                flipped==1 ~ maxindex+1)) %>%
  ungroup() %>%
  # Finally, generate trash values for each line
  mutate(trash=case_when(dupes!=1 ~ 1, # Trash if line is perfectly duplicated in letter
                         index<=salutation_last ~ 1, # Trash if earlier than the last-detected salutation
                         index>=closing_last ~ 1, # Trash if later than the first-detected closing
                         number_prop>=0.2 ~ 1, # Trash if over 20% of the line is numbers
                         address_token==1 & length<12 ~ 1, # Trash if most of the line is the address token
                         name_token==1 & length<9 ~ 1, # Trash if most of the line is the name token
                         phone_token==1 & length<10 ~ 1, # Trash if most of the line is the phone token
                         email_token==1 & length<10 ~ 1, # Trash if most of the line is the email token
                         length==0 ~ 1, # Trash if empty line
                         upper_prop>0.3 & upper_length!=0 & length>10 ~ 1, # Trash if most of the line is upper-case (for only longer lines)
                         upper_prop>0.8 & upper_length!=0 & length>3 ~ 1,
                         TRUE ~ 0)) %>% # Not trash if none of the above apply
  mutate(nontrash_length=case_when(trash==0 ~ length,
                                   trash==1 ~ 0L)) %>%
  group_by(letterid) %>%
  mutate(nontrash_total_characters=sum(nontrash_length),
         nontrash_lines=sum(1-trash),
         nontrash_prop=nontrash_lines/maxindex) %>%
  ungroup()

write_fst(texts_clean, path=file.path(project_data, paste0(fileprefix, dataset_group, '_texts_clean', filesuffix, '.fst')))

#texts_clean %>% filter(trash==0) %>% View()
#texts_clean %>% filter(trash==1) %>% View()

texts_para <- texts_clean %>%
  select(-commas, -periods, -upper_length, -upper_prop, -address_token, -name_token, -phone_token, -email_token, 
         -index_prop, -numbers, -number_prop, -dupes, -salutation_test, -closing_test, -salutation_any, 
         -closing_any, -salutation_last_tmp, -closing_last_tmp, -nontrash_length) %>%
  filter(trash==0) %>%
  select(-trash, -salutation_last, -closing_last) %>%
  group_by(letterid) %>%
  mutate(last2_char=str_sub(datavalscrubbedtext, start=-2),
         paragraph_end=case_when(last_char=="." | last_char=="!" | last_char=="?" | last_char=="?\"" | last2_char==".\"" | last2_char=="!\"" | last2_char==".\'" | last2_char=="!\'" | last_char=="?\'"  ~ 1,
                                 TRUE ~ 0),
         paragraph_start=lag(paragraph_end, n=1, default=0),
         paragraph_num=cumsum(paragraph_start)+1,
         total_paragraphs=max(paragraph_num)) %>%
  ungroup() %>%
  group_by(letterid, paragraph_num) %>%
  mutate(paragraph_text = paste(datavalscrubbedtext, collapse=" ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-datavalscrubbedtext, -index, -length, -last_char, -last2_char, -paragraph_end, -paragraph_start)

# Run a check for letters that are dominated by replacement tokens, erroneously
trash_checkpoint <- texts_para %>%
  mutate(address_token=str_count(paragraph_text, "<ADDRESS>"),
         name_token=str_count(paragraph_text, "<NAME>"),
         phone_token=str_count(paragraph_text, "<PHONE>"),
         email_token=str_count(paragraph_text, "<EMAIL>"),
         replacement_tokens=address_token+name_token+phone_token+email_token) %>%
  group_by(letterid) %>%
  mutate(letter_text = paste(paragraph_text, collapse=" "),
         total_tokens= sum(replacement_tokens)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(check=(nontrash_total_characters - (total_tokens * 7))/nontrash_total_characters,
         dropper=case_when(check<0.85 ~ 1,
                           TRUE ~ 0)
  ) %>%
  filter(dropper==1) %>%
  select(letterid)


texts_para <- texts_para %>%
  anti_join(trash_checkpoint, by="letterid") %>%
  mutate(paraid=paste0(letterid, "__", paragraph_num))

write_fst(texts_para, path=file.path(project_data, paste0(fileprefix, dataset_group, '_texts_para', filesuffix, '.fst')))

texts_letter <- texts_para %>%
  group_by(letterid) %>%
  mutate(letter_text = paste(paragraph_text, collapse=" ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-paragraph_text, -paragraph_num) %>%
  mutate(characters_per_paragraph=nontrash_total_characters/total_paragraphs)

write_fst(texts_letter, path=file.path(project_data, paste0(fileprefix, dataset_group, '_texts_letter', filesuffix, '.fst')))

# Create sentence-level dataset for sentiment analysis
texts_sentence <- texts_letter %>%
  select(letterid, letter_text) %>%
  unnest_tokens(input=letter_text, output=sentence_text, token="sentences", drop=TRUE, to_lower=FALSE)

rm(texts_letter)

texts_sentence <- texts_sentence %>%
  mutate(length=str_length(sentence_text),
         last_char=str_sub(sentence_text, start=-1),
         last2_char=str_sub(sentence_text, start=-2),
         last3_char=str_sub(sentence_text, start=-3),
         last4_char=str_sub(sentence_text, start=-4),
         # Catch cases when titles like Mr. Ms. etc. break sentences, or degrees (M.Ed.), or A.P.
         starter=case_when(last3_char=="Mr." | last3_char=="Ms." | last3_char=="Dr." | last4_char=="Mrs." |
                             last3_char=="Jr." | last3_char=="Sr." |
                             last3_char=="Ph." | last3_char=="Ed." |
                             last3_char=="St." |
                             last4_char=="A.P." | last4_char=="I.B." | last4_char=="U.S." | last4_char=="C.P." |
                             last4_char=="M.S." | last4_char=="M.A." | last4_char=="B.A." | last4_char=="A.B." |
                             last4_char=="B.S." | last4_char=="H.S." | last4_char=="P.S." | last4_char=="p.s." ~ 1,
                           TRUE ~ 0)) %>%
  group_by(letterid) %>%
  mutate(next_sentence=lead(sentence_text, default=""),
         next2_sentence=lead(sentence_text, n=2, default=""),
         prev_starter=lag(starter, default=0),
         prev2_starter=lag(starter, n=2, default=0),
         next_starter=lead(starter, default=0),
         next2_starter=lead(starter, n=2, default=0),
         sentence_text=case_when(starter==1 & next_starter==0 ~ paste(sentence_text, next_sentence, sep=" "),
                                 starter==1 & next_starter==1 & next2_starter==0 ~ paste(sentence_text, next_sentence, next2_sentence, sep=" "),
                                 TRUE ~ sentence_text)) %>%
  ungroup() %>%
  filter(!(prev_starter==1 & starter==0) & !(prev_starter==1 & prev2_starter==1 & starter==0)) %>%
  select(letterid, sentence_text) %>%
  mutate(length=str_length(sentence_text)) %>%
  filter(!(length<5 | length>500)) %>%
  select(-length) %>%
  # Get rid of the tokens and replace with neutral, natural-language substitutes
  # <ADDRESS> <EMAIL> <NAME> <PHONE>
  mutate(sentence_text=str_replace_all(sentence_text, pattern="\\<ADDRESS\\>", replacement="this place"),
         sentence_text=str_replace_all(sentence_text, pattern="\\<EMAIL\\>", replacement="this email"),
         sentence_text=str_replace_all(sentence_text, pattern="\\<NAME\\>", replacement="this student"),
         sentence_text=str_replace_all(sentence_text, pattern="\\<PHONE\\>", replacement="this number"))



# The following code just checks for unique tokens in the text that remain; keeping for future reference just in case
# test <- texts_sentence2 %>%
#   mutate(token_test=str_extract_all(sentence_text, "\\<{1}[\\w]*\\>{1}", simplify=TRUE))
# 
# test2 <- test$token_test %>%
#   as.data.frame() %>%
#   pivot_longer(cols=everything())
# 
# test2 %>% count(value)

write_dta(texts_sentence, path=file.path(project_data, paste0(fileprefix, dataset_group, '_texts_sentence', filesuffix, '.dta')))
write_fst(texts_sentence, path=file.path(project_data, paste0(fileprefix, dataset_group, '_texts_sentence', filesuffix, '.fst')))


rm(texts, texts_clean, texts_para, texts_sentence, trash_checkpoint)
# CHECK FOR TOTAL DUPES IN TEXTS_LETTERS


# texts_sentence$length[texts_sentence$length>500 | texts_sentence$length<10] %>% length()
# texts_sentence$invitationid[texts_sentence$length>500 | texts_sentence$length<5] %>% unique() %>% length()
# 
# texts_sentence$length[texts_sentence$length<10] %>% length()



# Clean out overtly way too short letters and match back?
# Count sentences per letter?


# 
# # Analyze letter-level dataset to examine distribution of letter characteristics
# ggplot(texts_letter, aes(x=characters_per_paragraph)) +
#   geom_histogram()
# 
# 
# 
# # View only trash and non-trash lines separately to do spot-checks
# View(texts_clean %>% filter(trash==1))
# View(texts_clean %>% filter(trash==0))
# 
# # How many letters are overwhelmingly trash lines?
# texts_clean$invitationid[texts_clean$nontrash_prop<=0.1] %>% unique() %>% length()
# texts_clean$invitationid[texts_clean$nontrash_prop==0] %>% unique() %>% length()
# View(texts_clean %>% filter(nontrash_prop<=0.1))
# 
# texts_clean$invitationid[texts_clean$nontrash_lines>100] %>% unique() %>% length()
# View(texts_clean %>% filter(nontrash_lines>100))
# 
# texts_clean$invitationid[texts_clean$nontrash_total_characters>8000] %>% unique() %>% length()
# View(texts_clean %>% filter(nontrash_total_characters>8000))

