#%%
import matlab.engine as ml
import os
import sys
import numpy as np
import math
import matplotlib.pyplot as plt
import matplotlib as mpl
import seaborn as sns
import pandas as pd
import pickle


#%% Try to pull data if possible
# if file exists load it
# else run pull data
# try:
#     cfdf = pd.read_csv('')
with open('vigor_conf.pickle','rb') as f:
    temp = pickle.load(f)
    cfdata = temp[0]
    target_data = temp[1]
    del temp

cfdf = pd.read_csv('conf_data.csv')
plt.style.use('classic')

trial_cutoff = 16
save_plots = 1
home = os.getcwd()
plots_dir = str(home)+'\\Plots_pilot\\'

# mpl.rcParams['axes.']

cfdf['error_angle_abs']=abs(cfdf['error_angle'])

#%% Plot Movement Duration
def plot_varbyrprob_tsplit(var, cfdf, trial_cutoff, plots_dir):
    fig, ax = plt.subplots(2,2,figsize=(12,8))
    for k, axs in enumerate(fig.axes):
        sns.regplot(x='r_prob', 
                    y=var,
                    data=cfdf.query('target == '+str(k+1)).query('trial>'+str(trial_cutoff)),ax = axs)
        sns.despine(top=True, right = True)
        axs.title.set_text('Target: '+str(k+1))
        axs.set_xlabel('Probability of Reward')
        axs.set_ylabel('Movement Duration')
        axs.tick_params(direction = 'out', top = 0, right = 0)
        
    plt.tight_layout()
    if save_plots:
        fig.savefig(plots_dir+var+"_rplot_splittarget.pdf", bbox_inches='tight')

plot_varbyrprob_tsplit('move_dur',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob_tsplit('peak_vel',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob_tsplit('react_time',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob_tsplit('react_vel_target',cfdf,trial_cutoff,plots_dir)

#%% Grouped all targets
def plot_varbyrprob(var, cfdf, trial_cutoff, plots_dir):
    fig, ax = plt.subplots()
    sns.scatterplot(x='r_prob',
                    y=var,
                    data=cfdf.query('trial>'+str(trial_cutoff)),
                    hue='target_s',
                    palette='muted',
                    ax=ax)
    sns.regplot(x='r_prob',
                y=var,
                data=cfdf.query('trial>25'),
                scatter=0,
                ax=ax)
    sns.despine(top=True, right = True)
    ax.set_xlabel('Reward Probability')
    ax.set_ylabel(var)
    ax.tick_params(direction = 'out', top = 0, right = 0)

    handles,labels = ax.get_legend_handles_labels()
    labels = ['Target Num','One','Two','Three','Four']
    handles = [handles[0], handles[3], handles[1], handles[2], handles[4]]

    ax.legend(handles,labels,loc=1)

    if save_plots:
        fig.savefig(plots_dir+var+"_rplot_allt.pdf", bbox_inches='tight')

#%%
plot_varbyrprob('move_dur',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob('peak_vel',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob('react_time',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob('react_vel_target',cfdf,trial_cutoff,plots_dir)

plot_varbyrprob('error_dist',cfdf,trial_cutoff,plots_dir)
plot_varbyrprob('error_angle',cfdf,trial_cutoff,plots_dir)

#%% Plotting Trajectories
#test = cfdata[0]['trial_1']
#import matplotlib.pyplot as plt
#
#fig, [ax1, ax2, ax3] = plt.subplots(3)
#temp = cfdata[0]['trial_65']
#ax1.plot(temp['X'])
#ax1.plot(temp['idx']['onset'],temp['X'][temp['idx']['onset']],'x',color='red')
#ax1.plot(temp['idx']['at_target'],temp['X'][temp['idx']['at_target']],'x',color='blue')
#ax1.plot(temp['idx']['offset'],temp['X'][temp['idx']['offset']],'x',color='green')
#ax2.plot(temp['Right_HandXVel'])
#ax2.plot(temp['idx']['onset'],temp['Right_HandXVel'][temp['idx']['onset']],'x',color='red')
#ax2.plot(temp['idx']['at_target'],temp['Right_HandXVel'][temp['idx']['at_target']],'x',color='blue')
#ax2.plot(temp['idx']['offset'],temp['Right_HandXVel'][temp['idx']['offset']],'x',color='green')
#ax3.plot(temp['Right_HandXAcc'])
#ax3.plot(temp['idx']['onset'],temp['Right_HandXAcc'][temp['idx']['onset']],'x',color='red')
#ax3.plot(temp['idx']['at_target'],temp['Right_HandXAcc'][temp['idx']['at_target']],'x',color='blue')
#ax3.plot(temp['idx']['offset'],temp['Right_HandXAcc'][temp['idx']['offset']],'x',color='green')
