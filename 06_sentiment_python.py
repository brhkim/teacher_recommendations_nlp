#!/usr/bin/env python
# coding: utf-8

# # Basic Setup
# 

# ### Import libraries

# In[1]:


import os
import pandas as pd
import numpy as np
import time
import seaborn as sns
import matplotlib.pyplot as plt
import tensorflow as tf

from sklearn import model_selection
from sklearn import ensemble
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import confusion_matrix 
from sklearn.metrics import accuracy_score 
from sklearn.metrics import classification_report 

from sklearn.model_selection import cross_val_score

from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline, AutoModelForSeq2SeqLM

import stanza
# stanza.download('en')


# ### Check that GPU loaded correctly

# In[2]:


device_name = tf.test.gpu_device_name()
if device_name != '/device:GPU:0':
    raise SystemError('GPU device not found')
print('Found GPU at: {}'.format(device_name))


# ### Grab environment variables

# In[3]:


totalarrays=int(os.getenv('arraymaxbk'))
currentarray=int(os.getenv('arraycurrentbk'))
filenameset=str(os.getenv('filenameset'))
suffixset=str(os.getenv('suffixset'))
rootset=str(os.getenv('rootset'))
datagroupset=str(os.getenv('datagroupset'))


# In[3]:


#totalarrays=160
#currentarray=1
#filenameset="03_test_texts_sentence_21-12-20.dta"
#suffixset="21-12-20"
#rootset="/project/commonappteacherrec/teacher_rec_full_replication/data/build"
#datagroupset="test"


# ### Load in and organize the teacher recommendation text data

# In[4]:


filename = rootset + "/" + filenameset


# In[5]:


teacherrec = pd.read_stata(filename)
teacherrec["index"]=teacherrec.index
teacherrec.shape


# ### Subset data into partitions for array job

# In[6]:


datalength = len(teacherrec.index)


# In[7]:


#partitionsize = np.ceil(len(teacherrec.index)/totalarrays).astype(np.int64)
partitionsize = round(datalength/totalarrays)
partitionsize


# In[8]:


bottombound = (partitionsize * (currentarray-1))
bottombound


# In[9]:


if currentarray==totalarrays:
    topbound = datalength
else:
    topbound = (partitionsize * currentarray)
topbound


# In[10]:


subset = teacherrec[bottombound:topbound]
#subset = teacherrec[1:100]


# In[12]:


del teacherrec


# # Begin creating NLP sentiment analysis pipeline

# ### Define various sentiment analysis models

# In[5]:


# https://huggingface.co/gilf/english-yelp-sentiment
yelp_all = pipeline(task="sentiment-analysis", model = AutoModelForSequenceClassification.from_pretrained("gilf/english-yelp-sentiment", revision="5dede457b5f58f45edcd71ccfe10953371c630ef"), tokenizer = AutoTokenizer.from_pretrained("gilf/english-yelp-sentiment", revision="5dede457b5f58f45edcd71ccfe10953371c630ef"), device = 0, return_all_scores=True)


# In[6]:


# https://huggingface.co/nlptown/bert-base-multilingual-uncased-sentiment
bert_all = pipeline(task="sentiment-analysis", model = AutoModelForSequenceClassification.from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment", revision="f4067398d9230016de89fc62c43e4ba42c349c72"), tokenizer = AutoTokenizer.from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment", revision="f4067398d9230016de89fc62c43e4ba42c349c72"), device = 0, return_all_scores=True)


# In[7]:


# https://huggingface.co/cardiffnlp/twitter-roberta-base-sentiment
twit_all = pipeline(task="sentiment-analysis", model = AutoModelForSequenceClassification.from_pretrained("cardiffnlp/twitter-roberta-base-sentiment", revision="c8c5458081108134d5b2e5fc2ab4215b677ed0b4"), tokenizer = AutoTokenizer.from_pretrained("cardiffnlp/twitter-roberta-base-sentiment", revision="c8c5458081108134d5b2e5fc2ab4215b677ed0b4"), device = 0, return_all_scores=True)


# In[8]:


# https://huggingface.co/textattack/xlnet-large-cased-SST-2
xlnet_all = pipeline(task="sentiment-analysis", model = AutoModelForSequenceClassification.from_pretrained("textattack/xlnet-base-cased-SST-2", revision="9ceeb077dcd5cf5ae790572b2bd6aec755a263be"), tokenizer = AutoTokenizer.from_pretrained("textattack/xlnet-base-cased-SST-2", revision="9ceeb077dcd5cf5ae790572b2bd6aec755a263be"), device = 0, return_all_scores=True)


# In[9]:


# https://huggingface.co/mrm8488/t5-base-finetuned-imdb-sentiment
imdb_all = pipeline(task="text2text-generation", model = AutoModelForSeq2SeqLM.from_pretrained("mrm8488/t5-base-finetuned-imdb-sentiment", revision="d9d412418ff1a359b7783eeebd5b318791f00765"), tokenizer = AutoTokenizer.from_pretrained("mrm8488/t5-base-finetuned-imdb-sentiment", revision="d9d412418ff1a359b7783eeebd5b318791f00765"), device = 0)


# In[10]:


# https://huggingface.co/textattack/albert-base-v2-SST-2
albert_all = pipeline(task="sentiment-analysis", model = AutoModelForSequenceClassification.from_pretrained("textattack/albert-base-v2-SST-2", revision="96d7dedb92b3679c4f1ae69e7e77440d058d8602"), tokenizer = AutoTokenizer.from_pretrained("textattack/albert-base-v2-SST-2", use_fast=False, revision="96d7dedb92b3679c4f1ae69e7e77440d058d8602"), device = 0, return_all_scores=True)


# In[12]:


stanza_top = stanza.Pipeline(lang='en', processors='tokenize,sentiment')


# ### Define pipeline for running sentiment analysis and charting output

# In[13]:


# Function to run the sentiment analysis algorithm on a specific string of text
# and input the results into a tidy series
#
# ARGS:
# text - String of text to be analyzed
# model - one of "yelp", "bert", "twit", "xlnet", "albert", or "stanza"

def save_sentiment(text, model):
    # Run analyses and save results
    if model=="yelp":
        temp_all = yelp_all(text)

        # Output results into a dataframe
        d = pd.Series({'LABEL_0':temp_all[0][0]["score"], 'LABEL_1': temp_all[0][1]["score"], 'LABEL_2': temp_all[0][2]["score"], 'LABEL_3': temp_all[0][3]["score"], 'LABEL_4': temp_all[0][4]["score"]})
        d2 = pd.Series({'label_predict': d.idxmax(), 'label_prob': d.max()})
        d = d.append(d2)
    
    elif model=="bert":
        temp_all = bert_all(text)

        # Output results into a dataframe
        d = pd.Series({'LABEL_0':temp_all[0][0]["score"], 'LABEL_1': temp_all[0][1]["score"], 'LABEL_2': temp_all[0][2]["score"], 'LABEL_3': temp_all[0][3]["score"], 'LABEL_4': temp_all[0][4]["score"]})
        d2 = pd.Series({'label_predict': d.idxmax(), 'label_prob': d.max()})
        d = d.append(d2)

    elif model=="twit":
        temp_all = twit_all(text)

        # Output results into a dataframe
        d = pd.Series({'LABEL_0':temp_all[0][0]["score"], 'LABEL_2': temp_all[0][1]["score"], 'LABEL_4': temp_all[0][2]["score"]})
        d2 = pd.Series({'label_predict': d.idxmax(), 'label_prob': d.max()})
        d = d.append(d2)

    elif model=="xlnet":
        temp_all = xlnet_all(text)

        # Output results into a dataframe
        d = pd.Series({'LABEL_0':temp_all[0][0]["score"], 'LABEL_4': temp_all[0][1]["score"]})
        d2 = pd.Series({'label_predict': d.idxmax(), 'label_prob': d.max()})
        d = d.append(d2)

    elif model=="imdb":
        temp_all = imdb_all(text)

        # Output results into a dataframe
        if temp_all[0]["generated_text"]=="negative":
            d = pd.Series({'label_predict': "LABEL_0"})
        elif temp_all[0]["generated_text"]=="positive":
            d = pd.Series({'label_predict': "LABEL_4"})
        else:
            d = pd.Series({'label_predict': "LABEL_2"})

    elif model=="albert":
        temp_all = albert_all(text)

        # Output results into a dataframe
        d = pd.Series({'LABEL_0':temp_all[0][0]["score"], 'LABEL_4': temp_all[0][1]["score"]})
        d2 = pd.Series({'label_predict': d.idxmax(), 'label_prob': d.max()})
        d = d.append(d2)
    
    elif model=="stanza":
        temp = stanza_top(text)
        for i, sentence in enumerate(temp.sentences):
            if sentence.sentiment==0:
                d = pd.Series({'label_predict': "LABEL_0"})
            elif sentence.sentiment==1:
                d = pd.Series({'label_predict': "LABEL_2"})
            elif sentence.sentiment==2:
                d = pd.Series({'label_predict': "LABEL_4"})

    # Print it
    return d
  


# In[14]:


def get_sentiment(dataset, text_column, model):
    start_time = time.time()

    temp = dataset[text_column].apply(save_sentiment, model=model)
    print("--- %s seconds ---" % (time.time() - start_time))
    temp = pd.concat([dataset, temp], axis=1)
    return temp


# In[15]:


# Function to quickly and visually display the accuracy metrics of the sentiment
# analysis algorithm in a confusion matrix and standard measures
#
# ARGS:
# truevals - Dataframe column of true values
# predictvals - Dataframe column of predicted values

def prediction_test(truevals, predictvals):
    sns.set(font_scale=1.2, rc={'figure.figsize':(11.7,8.27)})
    cm = confusion_matrix(truevals, predictvals)
    ax = plt.subplot()
    sns.heatmap(cm, annot=True, ax = ax, cmap='Greens', fmt='g')
    ax.set_xlabel('Predicted labels');ax.set_ylabel('True labels')
    ax.set_title('Confusion Matrix')

    print(classification_report(truevals, predictvals))


# # Apply to the teacher recommendation dataset

# ### Apply the Sentiment Analysis algorithms to masked N2FL text data

# In[16]:


teacherrec_yelp = get_sentiment(subset, "sentence_text", "yelp").add_suffix("_yelp")


# In[17]:


teacherrec_albert = get_sentiment(subset, "sentence_text", "albert").add_suffix("_albert")


# In[18]:


teacherrec_xlnet = get_sentiment(subset, "sentence_text", "xlnet").add_suffix("_xlnet")


# In[19]:


# Minor cleaning steps because Stanza can't take empty rows
stanza_prep = subset.copy()
stanza_prep["str_len"] = stanza_prep["sentence_text"].str.len()
stanza_prep = stanza_prep[stanza_prep['str_len'] >= 1].drop(columns=["str_len"])


# In[20]:


teacherrec_stanza = get_sentiment(stanza_prep, "sentence_text", "stanza").add_suffix("_stanza")


# In[21]:


teacherrec_bert = get_sentiment(subset, "sentence_text", "bert").add_suffix("_bert")


# In[22]:


teacherrec_twit = get_sentiment(subset, "sentence_text", "twit").add_suffix("_twit")


# In[23]:


# Minor cleaning steps because imdb can't take non-ascii characters apparently
imdb_prep = subset.copy()
imdb_prep["sentence_text"] = imdb_prep["sentence_text"].str.encode("ascii", "ignore").str.decode("ascii")


# In[24]:


teacherrec_imdb = get_sentiment(imdb_prep, "sentence_text", "imdb").add_suffix("_imdb")


# ### Join sentiment scores back into the main dataset

# In[25]:


teacherrec_analyze = pd.merge(subset, teacherrec_yelp, how="inner", left_on="index", right_on="index_yelp", validate="1:1").drop(columns=["sentence_text_yelp", "index_yelp", "letterid_yelp"])


# In[26]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_xlnet, how="inner", left_on="index", right_on="index_xlnet", validate="1:1").drop(columns=["sentence_text_xlnet", "index_xlnet", "letterid_xlnet"])


# In[27]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_albert, how="inner", left_on="index", right_on="index_albert", validate="1:1").drop(columns=["sentence_text_albert", "index_albert", "letterid_albert"])


# In[28]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_stanza, how="inner", left_on="index", right_on="index_stanza", validate="1:1").drop(columns=["sentence_text_stanza", "index_stanza", "letterid_stanza"])


# In[29]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_bert, how="inner", left_on="index", right_on="index_bert", validate="1:1").drop(columns=["sentence_text_bert", "index_bert", "letterid_bert"])


# In[30]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_twit, how="inner", left_on="index", right_on="index_twit", validate="1:1").drop(columns=["sentence_text_twit", "index_twit", "letterid_twit"])


# In[31]:


teacherrec_analyze = pd.merge(teacherrec_analyze, teacherrec_imdb, how="inner", left_on="index", right_on="index_imdb", validate="1:1").drop(columns=["sentence_text_imdb", "index_imdb", "letterid_imdb"])


# In[32]:


teacherrec_analyze.shape


# In[33]:


teacherrec_analyze.sample(10, random_state=1234)


# ### Output the dataset

# In[9]:


#filepath = rootset + '/output_%s' % suffixset


# In[11]:


#folderchecker = os.path.exists(filepath)
#if folderchecker==False:
#    os.mkdir(filepath)


# In[3]:


#filepath = filepath + '/06_sentiment_prep_%s.csv' % currentarray
#filepath = rootset + '/06_sentiment_prep_%s.csv' % currentarray


# In[4]:


filepath = rootset + '/06_%s_' % datagroupset
filepath = filepath + 'sentiment_prep_%s.csv' % currentarray


# In[47]:


teacherrec_analyze.to_csv(filepath)


# In[ ]:




