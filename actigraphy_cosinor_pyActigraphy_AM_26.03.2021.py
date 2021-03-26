#!/usr/bin/env python
# coding: utf-8

# # Actigraphy analysis using pyActigraphy 
# ### A.Michalak - 25.03.2021
# https://ghammad.github.io/pyActigraphy/tutorials.html

# In[1]:


import sys,os
import pyActigraphy
from pyActigraphy.analysis import Cosinor # library is a collection of scripts
import plotly.graph_objects as go
import pandas as pd
sys.executable


# In[2]:


cosinor = Cosinor() # object containing various functions; everything is accessed by '.', e.g., access a function such as cosinor.fit


# In[3]:


# Read the file
rawMTN = pyActigraphy.io.read_raw_mtn('C:/Users/adria/Desktop/data/B_Actigraphy_sessions/data_actigraphy_RAW/APO070.mtn') # period="14D"
rawMTN.IS()


# In[4]:


# Plot RAW data
layout = go.Layout(title="Actigraphy data - RAW", xaxis=dict(title="Date time"), yaxis=dict(title="Counts/period"), showlegend=False)
go.Figure(data=go.Scatter(x=rawMTN.data.index.astype(str), y=rawMTN.data), layout=layout)


# # Cleaning data

# ## *Mask inactive data*

# In[5]:


rawMTN.frequency


# In[6]:


rawMTN.create_inactivity_mask(duration='2h00min')


# In[7]:


rawMTN.inactivity_length # in epochs - 480 epochs * 15 sec each = 120 min


# In[8]:


layout = go.Layout(title="Data mask", xaxis=dict(title="Date time"), yaxis=dict(title="Mask"), showlegend=False)
go.Figure(data=go.Scatter(x=rawMTN.mask.index.astype(str),y=rawMTN.mask),layout=layout)


# In[9]:


rawMTN.inactivity_length = '2h'


# In[10]:


rawMTN.mask_inactivity = True
rawMTN.IS()


# In[11]:


# to check how much applying a mask is changing
#rawMTN.mask_inactivity = False
#rawMTN.IS()


# ## *Discarding invalid sequences at the beginning and/or the end of the recordings*
# remove first/last day of recording

# In[12]:


# Check the start time of the actigraphy recording
Timestamp = rawMTN.start_time
Timestamp


# In[13]:


# Check the duration of the recording
Timedelta = rawMTN.duration()
Timedelta


# In[14]:


layout = go.Layout(title="Actigraphy data", xaxis=dict(title="Date time"), yaxis=dict(title="Counts/period"), showlegend=False)
go.Figure(data=[go.Scatter(x=rawMTN.data.index, y=rawMTN.data)], layout=layout)


# In[15]:


rawMTN_cropped = pyActigraphy.io.read_raw_mtn('C:/Users/adria/Desktop/data/B_Actigraphy_sessions/data_actigraphy_RAW/APO070.mtn', start_time=Timestamp,
                                      period="14 D") # Timestamp


# In[16]:


layout = go.Layout(title="Actigraphy data", xaxis=dict(title="Date time"), yaxis=dict(title="Counts/period"), showlegend=False)
go.Figure(data=[go.Scatter(x=rawMTN_cropped.data.index, y=rawMTN_cropped.data)], layout=layout)


# In[17]:


# IS before cropping the data and addressing time of inactivity
rawMTN.IS()


# In[18]:


rawMTN_cropped.IS()


# In[19]:


# Check the duration of the recording - AFTER cropping
Timedelta_cropped = rawMTN_cropped.duration()
Timedelta_cropped


# # Non-parametric analysis
# Non-parametric variables such as:
# 
# - Interdaily stability (IS)
# - Intradaily variability (IV)
# - Relative amplitude (RA)

# In[20]:


# IS before cropping the data and addressing time of inactivity
rawMTN.IS(binarize=False)


# ##### Intra-Daily Variability (IV): 
# quantifies the degree of
# fragmentation of activity-rest periods. Typical healthy
# subjects will show a single prolonged activity period and a
# single prolonged rest period per 24 hour cycle. The
# variable has a theoretical range of 0 to 2 with higher values
# indicating higher fragmentation. Typical values for healthy
# subjects will be below 1.

# In[21]:


# Intradaily variability (IV) 
Intradaily_variability=rawMTN_cropped.IV(binarize=False)
Intradaily_variability


# ##### Interdaily Stability(IS): 
# quantifies the degree of regularity
# in the Activity-Rest pattern with a range of 0 to 1 where a
# value of 0 indicates a total lack of rhythm and a value of 1
# indicates a perfectly stable rhythm.

# In[22]:


# Interdaily stability (IS)
Interdaily_stability=rawMTN_cropped.IS(binarize=False)
Interdaily_stability


# ##### Relative Amplitude(RA): 
# the variable has a theoretical range of 0 to 1 with higher values
# indicating a rhythm with higher amplitude.

# In[23]:


# Relative amplitude (RA)
Relative_amplitude=rawMTN_cropped.RA(binarize=False)
Relative_amplitude


# # Parametric analysis

# ## *Cosinor analysis* - Single participnat analysis 

# In[29]:


cosinor.fit_initial_params['Period'].value = 1440*2 # 1440 for 1 minute, therefore 1440*4 for 15 sec epochs


# In[30]:


cosinor.fit_initial_params.pretty_print()


# In[31]:


cosinor.fit_initial_params['Period'].vary = False


# In[32]:


go.Figure(go.Scatter(x=rawMTN_cropped.data.index.astype(str), y=rawMTN_cropped.data))


# In[33]:


results=cosinor.fit(rawMTN_cropped, verbose=True) # Set verbose to True to print the fit output


# In[34]:


RedChiSq=results.redchi
RedChiSq


# In[35]:


amplitude=results.params['Amplitude'].value
amplitude


# In[36]:


acrophase=results.params['Acrophase'].value
acrophase


# In[37]:


period=results.params['Period'].value
period


# In[38]:


mesor=results.params['Mesor'].value
mesor


# In[39]:


results.params.valuesdict()


# In[40]:


results.aic # Akaike information criterium


# In[41]:


results.redchi # Reduced Chi^2


# In[42]:


best_fit = cosinor.best_fit(rawMTN_cropped, results.params)


# In[43]:


go.Figure(
    data=[
        go.Scatter(x=rawMTN_cropped.data.index.astype(str),y=rawMTN_cropped.data,name='Raw data'),
        go.Scatter(x=best_fit.index.astype(str),y=best_fit,name='Best fit')
    ]
)


# ## *Cosinor analysis* - Multiple participants analysis

# In[ ]:


# readers = pyActigraphy.io.read_raw('C:/Users/adria/Desktop/actigraphy/APO*.mtn', reader_type='MTN', verbose=1)


# In[ ]:


# len(readers.readers)


# In[ ]:


# readers.read_sst_log('C:/Users/adria/Desktop/actigraphy/sst_log.csv')


# In[ ]:


# readers.apply_sst(verbose=True)


# In[ ]:


# results_batch = cosinor.fit_reader(readers, n_jobs=3, prefer='threads')


# In[ ]:


# results_batch


# In[ ]:


# results_batch.to_csv('C:/Users/adria/Desktop/actigraphy/myfile.csv')


# # *Sleep fragmentation* - via state transition probability
# Quantify the Rest-to-activity transition probability, based on Lim et al.,2011 https://doi.org/10.5665/sleep.1400

# In[ ]:


# create objects for layout and traces
layout = go.Layout(title="",xaxis=dict(title=""), showlegend=False)


# In[ ]:


# Rest to activity (kRA)
pRA, pRA_weights = rawMTN_cropped.pRA(0, start='00:00:00', period='8H')


# In[ ]:


layout.update(title="Rest to Activity transition probability",xaxis=dict(title="Time [min]"), showlegend=False);


# In[ ]:


go.Figure(data=go.Scatter(x=pRA.index, y=pRA, name='', mode = 'markers'), layout=layout)


# #### Rest-to-activity transition probability
# The values kAR and kRA are metrics of the transition probabilities once sustained activity or rest have been attained and therefore represent measures of the tendency to fragment sustained runs of activity and rest, respectively. The higher the kAR, the more poorly sustained are runs of activity, and the lower the kAR the more consolidated they are. Similarly, the higher the kRA, the more fragmented are runs of rest, and the lower the kRA, the more consolidated they are.

# In[ ]:


# “The values kAR and kRA are metrics of the transition probabilities once sustained activity or rest have been attained”
rawMTN_cropped.kRA(0)


# In[ ]:


kRA=rawMTN_cropped.kRA(0, start='AoffT', freq='15min')
kRA


# ###### Lim et al., 2011
# kRA (probability) 0.027 (0.008) [0.014-0.084]

# In[ ]:


# Activity to rest (kAR) 
rawMTN_cropped.kAR(0)


# ###### Lim et al., 2011
# kAR (probability) 0.064 (0.032) [0.010-0.333] 

# In[ ]:


kAR=rawMTN_cropped.kAR(0, start='AonT', freq='15min')
kAR


# # Save all

# In[ ]:


part_dict = {"ID":["APO070"],"Timedelta_cropped":Timedelta_cropped,"Interdaily_stability":Interdaily_stability,"Intradaily variability":Intradaily_variability,
             "Relative_Amplitude":Relative_amplitude,"Rest_to_activity":kRA, "Activity_to_rest":kAR, "Amplitude":amplitude,
             "Acrophase":acrophase,"Period":period,"Mesor":mesor, "RedChiSq":RedChiSq}#"BIC":BIC
part_dict = pd.DataFrame(part_dict)
# change this one before running new analysis
part_dict.to_csv("C:/Users/adria/Desktop/actigraphy_outcomes_AM_24.03.2021/APO070_actigraphy_analysis.csv", index_label=False, index=False)

