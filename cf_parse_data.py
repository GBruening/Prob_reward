#%%
import matlab.engine as ml
import os
import sys
import numpy as np
import math
import matplotlib.pyplot as plt
# import seaborn as sns
import pandas as pd
import pickle

from cf_functions import *

# These need to be changed in cf_pull_data.py as well
# exp_name = '4_Target_Data' # Change this to determine which data set to use
# file_name = 'vigor_conf_postsqueeze_4targetpilot.pickle'
# confdata_file_name = 'vigor_conf_4t'

exp_name = '4t_180trial_4block' # Change this to determine which data set to use
file_name = 'vigor_conf_postsqueeze_'+exp_name+'.pickle'
confdata_file_name = 'vigor_conf_4t_180trial_4block'

abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname+'\\..\\')

os.chdir(exp_name+'/Data')
with open(file_name,'rb') as f:
    temp = pickle.load(f)
    cfdata = temp[0]
    target_data = temp[1]
    del temp
    os.chdir('..')


skip_files = ['Pilot_CK_4t_180trial_4block.zip',
              'Pilot_DA_4t_180trial_4block.zip',
              'Pilot_RC_4t_180trial_4block.zip',
              'Pilot_SS_4t_180trial_4block.zip']

skip_ind = []
for x, subj in enumerate(cfdata):
    if subj['trial_1']['filename'] not in skip_files:
        skip_ind.append(x)
skip_ind = [x for x, subj in enumerate(cfdata) if subj['trial_1']['filename'] not in skip_files]
cfdata = [cfdata[s] for s in skip_ind]

plt.style.use('classic')

trial_cutoff = 70
save_data = 1

# %% 
# PUll out return peak velocities
arr_data = {'subj': [], 'trial': [], 'P': [], 'V': [], 'A': []}

for s, subj_data in enumerate(cfdata):
    print('Analyzing subject '+str(s+1))
    for k, trial in enumerate(subj_data):
        # print('Subj: '+str(s))
        # print('Trial: '+trial)
        # cfdata[s][trial] = squeezin(cfdata[s][trial], squeeze_vars)
        # target_data['TARGET_TABLE'] = squeezin(target_data['TARGET_TABLE'],t_table_squeeze)
        subj_data[trial] = shift_x(subj_data[trial], target_data[s])
        subj_data[trial] = t_dist(subj_data[trial], target_data[s])
        subj_data[trial].update({'vigor': get_mvttimes(subj_data[trial],target_data[s])})
        subj_data[trial] = get_v_sign(subj_data[trial])
        subj_data[trial] = calc_errors(subj_data[trial], target_data[s])
        if subj_data[trial]['TRIAL']['TP']>4:
            subj_data[trial]['TRIAL']['TP'] += -4

        # # Used?
        # arr_data['subj'].append(s)
        # arr_data['trial'].append(k)
        # idx_targetshow = subj_data[trial]['vigor']['idx']['target_show']
        # idx_offset200 = subj_data[trial]['vigor']['idx']['offset']+200
        # arr_data['P'].append(subj_data[trial]['P'][idx_targetshow:idx_offset200])
        # arr_data['V'].append(subj_data[trial]['Right_HandVel'][idx_targetshow:idx_offset200])
        # arr_data['A'].append(subj_data[trial]['Right_HandAcc'][idx_targetshow:idx_offset200])

    cfdata[s] = est_p(subj_data)

# %% Remove Variables
for s, subj_data in enumerate(cfdata):
    print('Rm Vars subject '+str(s+1))
    for k, trial in enumerate(subj_data):
        rm_vars = ['Right_HandX',
                   'Right_HandY',
                   'Right_HandXVel',
                   'Right_HandYVel',
                   'Right_HandVel',
                   'Right_HandXAcc',
                   'Right_HandYAcc',
                   'Right_HandAcc']
        for item in rm_vars:
            del subj_data[trial][item]

#%% Save Data
if save_data:
    os.chdir('Data')
    with open(confdata_file_name+'.pickle', 'wb') as f:
        print('Data saved as: '+confdata_file_name+'.pickle')
        pickle.dump([cfdata, target_data], f)
    os.chdir('..')

#%% Put into a dataframe
import pandas as pd
rows = []
for s, subj in enumerate(cfdata):
    for t, trial in enumerate(subj):
        rows.append([s, #1
                     t+1,
                     cfdata[s][trial]['TRIAL']['TARGET'],
                     cfdata[s][trial]['TRIAL']['TP_NUM'],
                     cfdata[s][trial]['vigor']['move_dur'], #5
                     cfdata[s][trial]['vigor']['peak_vel'],
                     cfdata[s][trial]['vigor']['peak_vel_moveback'],
                     cfdata[s][trial]['vigor']['react_time'],
                     np.squeeze(cfdata[s][trial]['v_abs'][cfdata[s][trial]['vigor']['idx']['onset']]),
                     np.squeeze(cfdata[s][trial]['t_diff'][cfdata[s][trial]['vigor']['idx']['onset']]), #10
                     cfdata[s][trial]['est_prob'],
                     cfdata[s][trial]['r_prob'],
                     cfdata[s][trial]['diff_prob'],
                     cfdata[s][trial]['rewarded'],
                     cfdata[s][trial]['t_since_reward'],  #15
                     cfdata[s][trial]['vigor']['error_dist'],
                     cfdata[s][trial]['vigor']['move_back_error'],
                     cfdata[s][trial]['vigor']['error_angle'],
                     np.max(cfdata[s][trial]['P']),
                     cfdata[s][trial]['P'][cfdata[s][trial]['vigor']['idx']['onset']] #20
                     ])
        if cfdata[s][trial]['rewarded']==-1:
            print(str(cfdata[s][trial]['rewarded']))

cfdf = pd.DataFrame(rows)
cfdf.columns = ['subj', #1
                'trial',
                'target',
                'target_num',
                'move_dur', #5
                'peak_vel',
                'peak_vel_moveback',
                'react_time',
                'react_vel',
                'react_vel_target', #10
                'est_prob',
                'r_prob',
                'diff_prob',
                'rewarded',
                't_since_reward', #15
                'error_dist',
                'move_back_error',
                'error_angle',
                'maxex',
                'react_pos' #20
                ]
#%%
targets = ['one','two','three','four']
target_s = []
for val in cfdf['target'].astype('int32'):
    target_s.append(targets[val-1])
cfdf['target_s'] = target_s

if save_data:
    # os.chdir('Data')
    cfdf.to_csv(confdata_file_name+'.csv',index=False)
    print('Saved csv file as: '+confdata_file_name+'.csv')
    os.chdir('..')

#%%
