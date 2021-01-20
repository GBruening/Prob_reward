"""
cf_pull_data.py
--------------------------------------------

This script is used to pull the data from the .zip files saved in the
experiment folder. It uses the matlab python engine to pull the data from the
zip files and then uses python to squeeze the data into a numpy compatible
format.

"""

#%%
import matlab.engine as ml
import os
from os import path
import sys
import numpy as np
from scipy.signal import butter, lfilter
import math
import matplotlib.pyplot as plt
# import seaborn as sns
import pickle
import time

from cf_functions import *

plt.style.use('classic')
save_data = 1
save_plots = 0

out = []
cfdata = []
target_data = []

abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname+'\\..\\')

sys.path.append('/KINARMAnalysisScriptsv3')
# exp_name = '4_Target_Data' # Change this to determine which data set to use
# file_name = 'vigor_conf_postsqueeze_4targetpilot.pickle'

exp_name = '4t_180trial_4block' # Change this to determine which data set to use
file_name = 'vigor_conf_postsqueeze_'+exp_name+'.pickle'

# Switch to data folder and see if there's already pulled data. If pulled data
# already load it.
os.chdir(exp_name+'/Data')
already_pulled = []
if path.exists(file_name):
    with open(file_name,'rb') as f:
        temp = pickle.load(f)
    cfdata = temp[0]
    target_data = temp[1]
    del temp
    for s, subj in enumerate(cfdata):
        print('Loaded files: ')
        print(subj['trial_1']['filename'])
        already_pulled.append(subj['trial_1']['filename'])
os.chdir('..')

zip_files = []
n_files = 0
for file in os.listdir('Data'): 
    if file.endswith('.zip') and file not in already_pulled:
        n_files += 1
        zip_files.append(file)

print('Zip Files:')
for k, filename in enumerate(zip_files):
    print(str(k+1)+': '+str(zip_files[k]))

#%%
os.chdir('..')
base_dir = os.getcwd()
os.chdir(exp_name+'/Data')
eng = ml.start_matlab()

# Definte objects to pull from zip files.
obj_need = ['Right_HandX', 'Right_HandY', 'Right_HandXVel', 'Right_HandYVel', 'Right_HandVel',
            'Right_HandXAcc', 'Right_HandYAcc','Right_HandAcc',
            'Right_FS_TimeStamp', 'HAND', 'EVENTS',
            'EVENT_DEFINITIONS', 'TRIAL','TP_TABLE']

# Determine which fiels to pull. Then either append the current file or start fresh.
if len(already_pulled)>0 and n_files>0:
    for s, filename in enumerate(zip_files):
        print('Pulling Vars: '+str(filename))
        out = eng.testing('Data\\'+filename)
        cfdata.append({})
        cfdata[s+len(already_pulled)] = pull_vars(out, cfdata[s+len(already_pulled)], obj_need)
        cfdata[s+len(already_pulled)]['trial_1']['filename'] = filename
        
        target_data.append({})
        target_data[s+len(already_pulled)].update({'BLOCK_TABLE': out['trial_1']['BLOCK_TABLE'],
                               'TP_TABLE': out['trial_1']['TP_TABLE'],
                               'TARGET_TABLE': out['trial_1']['TARGET_TABLE']})
elif n_files == 1:
    out = eng.testing('Data',zip_files[0])
    cfdata.append({})
    cfdata[0] = pull_vars(out, cfdata[0], obj_need)
    cfdata[0]['trial_1']['filename'] = filename
    
    target_data.append({})
    target_data[0].update({'BLOCK_TABLE': out['trial_1']['BLOCK_TABLE'],
                        'TP_TABLE': out['trial_1']['TP_TABLE'],
                        'TARGET_TABLE': out['trial_1']['TARGET_TABLE']})
else:
    for s, filename in enumerate(zip_files):
        print('Pulling Vars: '+str(filename))
        print(os.getcwd())
        out = eng.testing(filename)
        cfdata.append({})
        cfdata[s] = pull_vars(out, cfdata[s], obj_need)
        cfdata[s]['trial_1']['filename'] = filename

        target_data.append({})
        target_data[s].update({'BLOCK_TABLE': out['trial_1']['BLOCK_TABLE'],
                            'TP_TABLE': out['trial_1']['TP_TABLE'],
                            'TARGET_TABLE': out['trial_1']['TARGET_TABLE']})

os.chdir('..')
eng.quit()

#%%
mltype = type(cfdata[0]['trial_1']['Right_HandVel'])

# Definte variables that need to be squeezed. to conver to the numpy format.
squeeze_vars = ['Right_HandX', 'Right_HandY',
                'Right_HandXVel', 'Right_HandYVel','Right_HandVel',
                'Right_HandXAcc', 'Right_HandYAcc', 'Right_FS_TimeStamp']

# Squeeze the data.
if len(already_pulled)>0 and n_files>0:
    for s, subj in enumerate(cfdata[len(already_pulled):]):
        n_call = 0
        print('Squeezin Subject: '+str(s+1))
        t = time.time()
        cfdata[s+len(already_pulled)], n_call = squeezin2(cfdata[s], mltype, n_call)
        print('Number of Calls: ' + str(n_call))
        print('Elapsed Time: ' +str(time.time()-t))
    print('Squeeze Target Data')
    n_call = 0
    target_data, n_call = squeezin2(target_data, mltype, n_call)
elif n_files>0:
    for s, subj in enumerate(cfdata):
        n_call = 0
        print('Squeezin Subject: '+str(s+1))
        t = time.time()
        cfdata[s+len(already_pulled)], n_call = squeezin2(cfdata[s], mltype, n_call)
        print('Number of Calls: ' + str(n_call))
        print('Elapsed Time: ' +str(time.time()-t))
    print('Squeeze Target Data')
    n_call = 0
    target_data, n_call = squeezin2(target_data, mltype, n_call)

# Save the data.
if save_data:
    os.chdir('Data')
    with open(file_name, 'wb') as f:
        print('Post Squeeze data saved as: '+file_name)
        pickle.dump([cfdata, target_data, file_name], f)
    os.chdir('..')

# %%
# This was an attempt to parralize this to speed it up. Didn't work.
# import multiprocessing as mp
# pool = mp.Pool(mp.cpu_count())
# https://www.machinelearningplus.com/python/parallel-processing-python/
# cfdata = [pool.apply(squeezin2, args=(subj, mltype, n_call)) for subj in enumerate(cfdata)]