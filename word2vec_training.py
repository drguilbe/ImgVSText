#!/usr/bin/env python
# coding: utf-8

# ## Train word2vec model with same specs as google news model

# In[1]:


from gensim.models import Word2Vec


# In[5]:


import pandas as pd


# In[3]:


import os


# ## Loading data and cleaning it
# 

# In[11]:


from gensim.parsing.preprocessing import preprocess_string, strip_tags, strip_punctuation


# In[12]:


CUSTOM_FILTERS = [lambda x: x.lower(), strip_tags, strip_punctuation]


# In[14]:


cleaned_documents = []


# In[16]:


directory = os.fsencode("news_dataset/")
for file in os.listdir(directory):
    filename = os.fsdecode(directory + file)
    file_content = pd.read_csv(filename)
    all_sentences = list(file_content.description.values)
    for sentence in all_sentences:
        cleaned_documents.append(preprocess_string(sentence, CUSTOM_FILTERS))


# In[20]:


cleaned_documents[0]


# ## Word2Vec training
# 
# 300 dimensional size, skip-gram training, with a window size of 10, similar to the google2news model.
# 
# Documentation for the Word2Vec model if you want to use multiple cores:
# 
# https://radimrehurek.com/gensim/models/word2vec.html

# In[22]:


model = Word2Vec(sentences=cleaned_documents, vector_size=300, sg=1, window=10)


# In[24]:


model.wv.most_similar("engineer")


# In[ ]:




