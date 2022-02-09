#!/usr/bin/env python
# coding: utf-8

# In[1]:


import h5py
import numpy as np
import pandas as pd
from pandas import HDFStore


# In[2]:


# get input direction inputdir = urbansim/data
# get the file to process, you would need to know the year
# if it is the base year, filename = custom_mpo_06197001_model_data.h5
# if other years it is called model_data_[year]

data = pd.HDFStore('custom_mpo_06197001_model_data.h5')
# data = h5py.File('custom_mpo_06197001_model_data.h5', 'r')
list(data.keys())


# In[4]:


households = data['/households']


# In[8]:


households.dtypes


# In[9]:


households.to_csv('households.csv')


# In[10]:


blocks = data['/blocks']


# In[12]:


blocks.head()


# In[13]:


blocks.to_csv('blocks.csv')


# In[14]:


persons = data['/persons']


# In[15]:


persons.to_csv('persons.csv')


# In[5]:


residential_units= data['/residential_units']


# In[6]:


residential_units.head()


# In[7]:


residential_units.to_csv('residential.csv')


# In[3]:


jobs = data['/jobs']


# In[4]:


jobs.to_csv('jobs.csv')

# put the csv files to atlas_input direction pilates/atlas/atlas_input/year2016


