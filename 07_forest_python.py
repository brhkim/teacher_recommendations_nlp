#!/usr/bin/env python
# coding: utf-8

# # Basic Setup

# In[1]:


import os
import pandas as pd
import numpy as np
import time
import seaborn as sns
import matplotlib.pyplot as plt
import tensorflow as tf
import pickle

#import importlib.util
#spec = importlib.util.spec_from_file_location("sklearn", "/home/bhk5fs/.local/lib/python3.7/site-packages/sklearn/__init__.py")
#sklearn = importlib.util.module_from_spec(spec)
#spec.loader.exec_module(sklearn)

import sklearn

from sklearn import model_selection
from sklearn import ensemble
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import confusion_matrix 
from sklearn.metrics import accuracy_score 
from sklearn.metrics import classification_report 

from sklearn.model_selection import cross_val_score


# In[2]:


print(sklearn.__version__)


# # Load in Data

# In[1]:


# Get environment variables
filenameset=str(os.getenv('filenameset'))
suffixset=str(os.getenv('suffixset'))
rootset=str(os.getenv('rootset'))
clusterfirst=str(os.getenv('clusterfirst'))
clusterlast=str(os.getenv('clusterlast'))
datagroupset=str(os.getenv('datagroupset'))


# In[33]:


#filenameset=""
#suffixset="21-12-20"
#rootset="/project/commonappteacherrec/teacher_rec_full_replication/data/build"
#clusterfirst=str(1)
#clusterlast=str(2)
#datagroupset="train"


# In[34]:


rfcpath = rootset + "/rfc.sav"
#rfcpath = "/home/bhk5fs/Common App Teacher Recommendations/rfc.sav"


# In[35]:


rfc = pickle.load(open(rfcpath, "rb"))


# In[36]:


filepath = rootset


# In[72]:


filename1 = filepath + '/06_%s_' % datagroupset
filename1 = filename1 + 'sentiment_prep_%s.csv' % clusterfirst


# In[73]:


filename2 = filepath + '/06_%s_' % datagroupset
filename2 = filename2 + 'sentiment_prep_'


# In[74]:


data = pd.read_csv(filename1, sep=",", encoding='utf-8').drop(columns=["Unnamed: 0"])


# In[75]:


for x in range(int(clusterfirst)+1, int(clusterlast)+1):
    filename3 = filename2 + str(x) + ".csv"
    tmp = pd.read_csv(filename3, sep=",", encoding='utf-8').drop(columns=["Unnamed: 0"])
    data = data.append(tmp)
    print(x)


# In[76]:


data.head()


# In[77]:


data.shape


# In[78]:


data.columns


# # Clean Data for Algorithm

# ### Make dummy variables from predictions

# In[79]:


data_x = pd.get_dummies(data, prefix=['yelp_', 'xlnet_', 'albert_', 'stanza_', 'bert_', 'twit_', 'imdb_'], columns=['label_predict_yelp', 'label_predict_xlnet', 'label_predict_albert', 'label_predict_stanza', 'label_predict_bert', 'label_predict_twit', 'label_predict_imdb'])


# ### Add additional predictors

# In[80]:


data_x.head()


# In[81]:


data_x.columns


# In[82]:


if 'imdb__LABEL_2' not in data_x.columns:
    data_x[['imdb__LABEL_2']] = 0
    data_x = data_x[['letterid',
 'sentence_text',
 'index',
 'LABEL_0_yelp',
 'LABEL_1_yelp',
 'LABEL_2_yelp',
 'LABEL_3_yelp',
 'LABEL_4_yelp',
 'label_prob_yelp',
 'LABEL_0_xlnet',
 'LABEL_4_xlnet',
 'label_prob_xlnet',
 'LABEL_0_albert',
 'LABEL_4_albert',
 'label_prob_albert',
 'LABEL_0_bert',
 'LABEL_1_bert',
 'LABEL_2_bert',
 'LABEL_3_bert',
 'LABEL_4_bert',
 'label_prob_bert',
 'LABEL_0_twit',
 'LABEL_2_twit',
 'LABEL_4_twit',
 'label_prob_twit',
 'yelp__LABEL_0',
 'yelp__LABEL_1',
 'yelp__LABEL_2',
 'yelp__LABEL_3',
 'yelp__LABEL_4',
 'xlnet__LABEL_0',
 'xlnet__LABEL_4',
 'albert__LABEL_0',
 'albert__LABEL_4',
 'stanza__LABEL_0',
 'stanza__LABEL_2',
 'stanza__LABEL_4',
 'bert__LABEL_0',
 'bert__LABEL_1',
 'bert__LABEL_2',
 'bert__LABEL_3',
 'bert__LABEL_4',
 'twit__LABEL_0',
 'twit__LABEL_2',
 'twit__LABEL_4',
 'imdb__LABEL_0',
 'imdb__LABEL_2',
 'imdb__LABEL_4']]


# In[83]:


def add_predictors(df):
    start_time = time.time()
    
    tmp = pd.DataFrame(np.sort(df[["LABEL_0_yelp", "LABEL_1_yelp", "LABEL_2_yelp", "LABEL_3_yelp", "LABEL_4_yelp"]].values))
    df["label_prob2_yelp"] = tmp.iloc[:,-2].values
    df["label_prob_diff_yelp"] = df["label_prob_yelp"] - df["label_prob2_yelp"]

    df["label_prob_diff_xlnet"] = (df["LABEL_0_xlnet"] - df["LABEL_4_xlnet"]).abs()

    df["label_prob_diff_albert"] = (df["LABEL_0_albert"] - df["LABEL_4_albert"]).abs()

    tmp = pd.DataFrame(np.sort(df[["LABEL_0_bert", "LABEL_1_bert", "LABEL_2_bert", "LABEL_3_bert", "LABEL_4_bert"]].values))
    df["label_prob2_bert"] = tmp.iloc[:,-2].values
    df["label_prob_diff_bert"] = df["label_prob_bert"] - df["label_prob2_bert"]

    tmp = pd.DataFrame(np.sort(df[["LABEL_0_twit", "LABEL_2_twit", "LABEL_4_twit"]].values))
    df["label_prob2_twit"] = tmp.iloc[:,-2].values
    df["label_prob_diff_twit"] = df["label_prob_twit"] - df["label_prob2_twit"]
    
    print("--- %s seconds ---" % (time.time() - start_time))

    return df


# In[84]:


data_x = add_predictors(data_x)


# In[85]:


data_x.head()


# In[86]:


column_check_actual = data_x.columns
column_check_actual


# In[87]:


column_check_expected = ['letterid',  'sentence_text', 'index', 'LABEL_0_yelp', 'LABEL_1_yelp', 'LABEL_2_yelp', 'LABEL_3_yelp',
       'LABEL_4_yelp', 'label_prob_yelp', 'LABEL_0_xlnet', 'LABEL_4_xlnet',
       'label_prob_xlnet', 'LABEL_0_albert', 'LABEL_4_albert',
       'label_prob_albert', 'LABEL_0_bert', 'LABEL_1_bert', 'LABEL_2_bert',
       'LABEL_3_bert', 'LABEL_4_bert', 'label_prob_bert', 'LABEL_0_twit',
       'LABEL_2_twit', 'LABEL_4_twit', 'label_prob_twit', 'yelp__LABEL_0',
       'yelp__LABEL_1', 'yelp__LABEL_2', 'yelp__LABEL_3', 'yelp__LABEL_4',
       'xlnet__LABEL_0', 'xlnet__LABEL_4', 'albert__LABEL_0',
       'albert__LABEL_4', 'stanza__LABEL_0', 'stanza__LABEL_2',
       'stanza__LABEL_4', 'bert__LABEL_0', 'bert__LABEL_1', 'bert__LABEL_2',
       'bert__LABEL_3', 'bert__LABEL_4', 'twit__LABEL_0', 'twit__LABEL_2',
       'twit__LABEL_4', 'imdb__LABEL_0', 'imdb__LABEL_2', 'imdb__LABEL_4',
       'label_prob2_yelp', 'label_prob_diff_yelp', 'label_prob_diff_xlnet',
       'label_prob_diff_albert', 'label_prob2_bert', 'label_prob_diff_bert',
       'label_prob2_twit', 'label_prob_diff_twit']
column_check_expected


# In[88]:


if not (np.array_equal(column_check_actual, column_check_expected)):
    raise ValueError("Columns are not as expected in the order the random forest model requires")


# # Apply to teacher recommendation data

# In[89]:


start_time = time.time()
rfc_predict_data = rfc.predict(data_x.drop(columns=['letterid', 'sentence_text', 'index']))
rfc_predict_data = pd.Series(rfc_predict_data)
print("--- %s seconds ---" % (time.time() - start_time))


# In[90]:


start_time = time.time()
probabilities = rfc.predict_proba(data_x.drop(columns=['letterid', 'sentence_text', 'index']))
print("--- %s seconds ---" % (time.time() - start_time))


# In[92]:


predictions = {'index': rfc_predict_data.index,'label_predict_forest': rfc_predict_data, 'LABEL_0_forest': probabilities[:,0], 'LABEL_1_forest': probabilities[:,1], 'LABEL_2_forest': probabilities[:,2], 'LABEL_3_forest': probabilities[:,3], 'LABEL_4_forest': probabilities[:,4]}


# In[93]:


predictions = pd.DataFrame(predictions)


# In[94]:


#data_x['merge'] = np.arange(len(data_x))


# In[95]:


#data_x['merge'] = data_x.index


# In[96]:


data_x.head()


# In[97]:


output_prep = pd.merge(data_x, predictions, how="left", on="index", validate="1:1")


# In[101]:


output_prep.head(20)


# In[100]:


outputname = rootset + "/07_%s_" % datagroupset
outputname = outputname + "forest_output_%s.csv" %suffixset


# In[41]:


output_prep.to_csv(outputname)


# In[ ]:




