#%%
# Loading Data

import pickle
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from scipy.interpolate import interp1d

#%%
# Load the data
# exp_name = '4t_180trial_4block' # Change this to determine which data set to use
exp_name = 'Rand_block'
file_name = 'vigor_conf_postsqueeze_'+exp_name+'.pickle'
confdata_file_name = 'vigor_conf_'+exp_name+'.pickle'

abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname+'\\..\\')

os.chdir(exp_name+'/Data')
pickle_files = []
n_files = 0
cfdata = []
target_data = []
for file in os.listdir():
    # if file.endswith('.pickle'):
    if file.endswith('.pickle') and file[11:13]=='Ra':
        # file_name = 'vigor_conf_'+exp_name+'_'+str(n_files)+'.pickle'
        file_name = file
        with open(file_name,'rb') as f:
            print('Loading Subject: ' + file_name)
            temp = pickle.load(f)
            cfdata.append(temp[0])
            target_data.append(temp[1])
            del temp
        n_files += 1
        # if n_files > 2:
        #     break
        
# file_name = 'vigor_conf_4t_180trial_4block_0.pickle'
# with open(file_name,'rb') as f:
#     print('Loading Subject: ' + file_name)
#     temp = pickle.load(f)
#     peakv_subj_sum = 0
#     for t, trial in enumerate(temp[0].items()):
#         peakv_subj_sum += np.max(trial[1]['rad_v'])
#     peakv_subj_sum = peakv_subj_sum/736
#     if peakv_subj_sum < 105:
#         cfdata.append(temp[0])
#         target_data.append(temp[1])
#     del temp

os.chdir('..')

os.chdir(dname+'\\..\\')
os.chdir(exp_name)
cfdf = pd.read_csv('vigor_conf_'+exp_name+'.csv')

#%%
# Functions
def interp_func(data, onset_to_moveback):
        # Do Some Normalization stuff
        f2 = interp1d(onset_to_moveback,data,kind='cubic')
        xnew = np.linspace(onset_to_moveback[0],onset_to_moveback[-1], num = 1000, endpoint = True)
        return f2(xnew)

# for s in range(0,10):
#     for tar in [1,2,3,4]:
#         r_sum  = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==1'))
#         nr_sum = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==0'))
#         f_sum  = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==-1'))
#         if r_sum != 90:
#             print('Subject '+str(s)+' Target '+str(tar)+': Rewarded '+str(r_sum)+', Not '+str(nr_sum))
#         if r_sum+nr_sum+f_sum != 180:
#             print('Subject '+str(s)+' Target '+str(tar)+': Rewarded '+str(r_sum)+', Not '+str(nr_sum)+', F '+str(f_sum))

# for s in range(0,10):
#     for tar in  [1,2,3,4]:
#         n_tar = len(cfdf.query('subj=='+str(s)).query('target=='+str(tar)))
#         if n_tar != 184:
#             print('Subject '+str(s)+' Target '+str(tar)+': '+str(n_tar))

# onset peakv at_target offset target_show moveback
# onset_or_targetshow = 'onset' #'move_back'
onset_or_targetshow = 'target_show' #'move_back'
offset_or_moveback = 'offset' # 'offset'

min_thin_onset = 100000
min_thin_target_show = 100000
min_target_show = []
min_thin_react = 100000
# min_thin_peakv = 100000
# max_thin_peakv = 100000
for s, subj in enumerate(cfdata):
    for t, trial in enumerate(subj.items()):
        min_thin_onset = min([min_thin_onset,
                             len(trial[1]['P'])-trial[1]['vigor']['idx']['onset']])
        min_target_show.append(len(trial[1]['P'])-trial[1]['vigor']['idx'][onset_or_targetshow])
        min_thin_target_show = min([min_thin_target_show,
                                    len(trial[1]['P'])-trial[1]['vigor']['idx'][onset_or_targetshow]])
        min_thin_react = min([min_thin_onset,
                             trial[1]['vigor']['idx']['onset']])
        # min_thin_peakv = min([min_thin_peakv,
        #                       trial[1]['vigor']['peakv']])
        # max_thin_peakv = min([max_thin_peakv,
        #                       len(trial[1]['P'])-trial[1]['vigor']['peakv']])

#%% 
# Put data into correct format
r_prob = []
est_prob = []
RPE = []
prior_RPE = []
prior_RWD = []
RWD = []
c = []
subject = []

test_var_abs = ['p','rad_v']
# test_var_norm = [item+'_norm' for item in test_var_abs]
alignments = ['target_show','onset','peakv','retpeakv']
test_var_names = ['P','rad_v',]
for item in test_var_abs:
    for aligns in alignments:
        exec(item+'_'+aligns+'align= []')
# for item in test_var_norm:
#     exec(item+'= []')
# for item in test_var_peakv_align:
#     exec(item+'= []')

#%% Loopin
rm_vars = ['Right_FS_TimeStamp',
 'HAND', 'EVENTS', 'EVENT_DEFINITIONS', 
 'TP_TABLE', 'Reward_prob',
 'filename', 'X', 'Y', 't_dist',
 't_diff', 'tx', 'ty', 'px_sign',
 'py_sign', 'vx_sign', 'vy_sign', 'ax_sign',
 'ay_sign', 'v_abs', 'a_abs',
 't_since_reward']
for s, subj in enumerate(cfdata):
    trials = list(subj.keys())
    for t, trial in enumerate(subj.items()):
        for item in rm_vars:
            if item in cfdata[s][trials[t]].keys():
                del cfdata[s][trials[t]][item]

#%% Loopin
for s, subj in enumerate(cfdata):
    trial_count = 0
    c1 = 0
    c2 = 0
    c3 = 0
    c4 = 0
    for t, trial in enumerate(subj.items()):
        # trial[1]['vigor']['idx']['retpeakv'] = np.argmin(trial[1]['rad_v'])
        # check2 = len(trial[1]['P'])-trial[1]['vigor']['idx']['peakv']>500
        # check3 = trial[1]['vigor']['idx']['retpeakv'] > trial[1]['vigor']['idx']['peakv']
        # check4 = len(trial[1]['P'])-trial[1]['vigor']['idx']['retpeakv']>100
        check1 = trial[1]['rewarded']==-1
        check2 = len(trial[1]['P'])-trial[1]['vigor']['idx']['peakv']>500
        if max(trial[1]['P']) > .08 and not check1 and check2:
            block = 0
            trial_in_block = (t-16) % 180
            if trial_in_block == 0:
                c1 = 0
                c2 = 0
                c3 = 0
                c4 = 0
                block += 1

            if t < 16 or trial[1]['rewarded']==-1:
                continue
            prob = trial[1]['r_prob']
            if prob == 0:
                cond = 1
                c1 += 1
                if c1 > 30:
                    continue
            elif prob == 0.33:
                cond = 2
                c2 += 1
                if c2 > 30:
                    continue
            elif prob == 0.66:
                cond = 3
                c3 += 1
                if c3 > 30:
                    continue
            elif prob == 1:
                cond = 4
                c4 += 1
                if c4 > 30:
                    continue
            c.append(cond)

            subject.append(s)
            if trial[1]['TRIAL']['TARGET'] == 1:
                flipx = 1
                flipy = 1
            elif trial[1]['TRIAL']['TARGET'] == 2:
                flipx = -1
                flipy = 1
            elif trial[1]['TRIAL']['TARGET'] == 3:
                flipx = -1
                flipy = -1
            elif trial[1]['TRIAL']['TARGET'] == 4:
                flipx = 1
                flipy = -1
            
            target_show_to_movebackish = np.arange(trial[1]['vigor']['idx']['target_show'],
                                            trial[1]['vigor']['idx']['offset'])
            # onset_to_movebackish = peak_v_align
            
            # for it, item in enumerate(test_var_names):
            #     if 'X' in item:
            #         flipit = flipx
            #     else:
            #         flipit = 1

            #     if 'Y' in item:
            #         flipit = flipy
            #     else:
            #         flipit = 1

            ## Target_show
            target_show_to_movebackish = np.arange(trial[1]['vigor']['idx']['target_show'],
                                            trial[1]['vigor']['idx']['offset'])
            data = np.array(trial[1]['rad_v'])[target_show_to_movebackish]
            nans = np.zeros(4000) + np.nan
            data = np.append(data,nans)
            data = data[0:1900]
            rad_v_target_showalign.append(data)

            ## Onset
            onset_to_movebackish = np.arange(trial[1]['vigor']['idx']['onset']-200,
                                            trial[1]['vigor']['idx']['offset'])
            data = np.array(trial[1]['rad_v'])[onset_to_movebackish]
            nans = np.zeros(4000) + np.nan
            data = np.append(data,nans)
            data = data[0:1700]
            rad_v_onsetalign.append(data)

            ## PeakV align
            data = np.array(trial[1]['rad_v'])
            nans = np.zeros(4000) + np.nan
            data = np.append(data,nans)
            peak_v_align = np.arange(trial[1]['vigor']['idx']['peakv']-500+1,
                                    trial[1]['vigor']['idx']['peakv']+500-1)
            data = data[peak_v_align]
            rad_v_peakvalign.append(data)
            
            ## RETPeakV align
            data = np.array(trial[1]['rad_v'])
            nans = np.zeros(4000) + np.nan
            data = np.append(data,nans)
            RETpeak_v_align = np.arange(trial[1]['vigor']['idx']['retpeakv']-500+1,
                                        trial[1]['vigor']['idx']['retpeakv']+500-1)
            data = data[RETpeak_v_align]
            rad_v_retpeakvalign.append(data)

            # data = np.array(trial[1][str(item)])[onset_to_offset]*flipit
            # interped = interp_func(data,onset_to_offset)
            # exec(test_var_norm[it]+'.append(interped)')
            
            est_prob.append(trial[1]['est_prob'])
            r_prob.append(trial[1]['r_prob'])
            RPE.append(trial[1]['RPE'])
            prior_RPE.append(trial[1]['prior_RPE'])
            prior_RWD.append(trial[1]['prior_RWD'])
            RWD.append(trial[1]['rewarded'])
            trial_count += 1
    print('Subj: '+str(s)+', Trial: '+str(trial_count))

#%%
# arr_variables = ['c','subject','p','x','y','v',
#                  'rad_v','vx','vy','a','ax','ay',
#                  'est_prob','r_prob','RPE','prior_RPE','prior_RWD','RWD']
# labels = ['Condition',
#            'Subject',
#            'Position (m)',
#            'X Position (m)',
#            'Y Position (m)',
#            'Velocity (m/s)',
#            'Radial Velocity (m/s)',
#            'X Velocity (m/s)',
#            'Y Velocity (m/s)',
#            'Acceleration (m^2/s)',
#            'X Acceleration (m^2/s)',
#            'Y Acceleration (m^2/s)',
#            'Reward Probability',
#            'Reward Prediction Error (RPE)',
#            'Reward Prediction Error Previous Trial (RPE)'
#            'Reward on previous trial',
#            'Reward on current trial'
# ]
# labels2 = ['Position',
#            'X_Position',
#            'Y_Position',
#            'Velocity',
#            'Radial Velocity',
#            'X_Velocity',
#            'Y_Velocity',
#            'Acceleration',
#            'X_Acceleration',
#            'Y_Acceleration'
# ]
arr_variables = ['c','subject','p','rad_v','est_prob','r_prob','RPE','prior_RPE','prior_RWD','RWD']
labels = ['Condition',
           'Subject',
           'Position (m)',
           'Radial Velocity (m/s)',
           'Reward Probability',
           'Reward Prediction Error (RPE)',
           'Reward Prediction Error Previous Trial (RPE)'
           'Reward on previous trial',
           'Reward on current trial'
]
labels2 = ['Position',
           'Radial Velocity'
]
alignment_vars = ['rad_v_target_showalign', 'rad_v_onsetalign', 'rad_v_peakvalign','rad_v_retpeakvalign']
alignment_labs = ['target show','onset','peak velocity','return peak velocity']
for item in alignment_vars:
    exec(item + ' = np.array('+item+')')

#%%
from matplotlib import cm
import matplotlib.gridspec as gridspec

os.chdir('Graphs')
RPEs = [np.round(item,2) for item in np.unique(RPE)]

plt.close('all')
fig = plt.figure(figsize = (32, 20))
widths = [1, 1, 1, 1, 1, 1, 1, 1, 1]
heights = [1, 1, 1, 1, 1, 1]
spec5 = fig.add_gridspec(ncols=len(widths), nrows=len(heights), width_ratios=widths,height_ratios=heights)
count = 0
axes=[]
for row in range(len(heights)):
    for col in range(len(widths)):
        axes.append(fig.add_subplot(spec5[row, col]))
        count += 1
        
hide_axis_numbers = [1,2,3,4,5,6,7,8,9,10,19,28,37,46]
for num in hide_axis_numbers:
    axes[num-1].axis('off')


fig.text(5/9, .93,'Alignment',size = 80, ha='center')
fig.text(2/9, .85,'Target Show',size = 40, ha='center')
fig.text(4/9, .85,'Onset',size = 40, ha='center')
fig.text(6/9, .85,'Peak Velocity',size = 40, ha='center')
fig.text(8/9, .85,'Ret Peak Velocity',size = 40, ha='center')

fig.text(0.02, 2.5/6, 'Probability Metric', size = 80, va = 'center', rotation=90)
fig.text(0.07, 4.5/6, 'R-Prob', size = 40, va = 'center', rotation=90)
fig.text(0.07, 3.5/6, 'RWD', size = 40, va = 'center', rotation=90)
fig.text(0.07, 2.5/6, 'Prior RWD', size = 40, va = 'center', rotation=90)
fig.text(0.07, 1.5/6, 'RPE', size = 40, va = 'center', rotation=90)
fig.text(0.07, 0.5/6, 'Prior RPE', size = 40, va = 'center', rotation=90)



#%%
ax_num = 10
for probability_method in ['r_prob','RWD','prior_RWD','RPE','prior_RPE']:#,'r_prob','RPE','prior_RPE']:
    if probability_method == 'prior_RWD':
        independent_var = prior_RWD
    elif probability_method == 'RWD':
        independent_var = RWD
    elif probability_method == 'r_prob':
        independent_var = r_prob
    elif probability_method == 'RPE':
        independent_var = RPE
    elif probability_method == 'prior_RPE':
        independent_var = prior_RPE
    else:
        raise 'Didn\'t select a probability method'
    independent_var_unique = np.unique(independent_var)
    independent_var_labs = [np.round(item,2) for item in np.unique(independent_var)]

    for a, aligns in enumerate(alignment_vars):
        test_var_num = 1
        print('Processing avg_plot '+aligns+' '+ probability_method)
        
        test_var_label = labels[test_var_num+2]
        test_var = []
        exec('test_var = '+ aligns)

        viridis = cm.get_cmap('viridis')
        viridis_big = cm.get_cmap('viridis', len(np.unique(independent_var)))
        viridis_4 = cm.get_cmap('viridis', 4)
        
        ## AVG plot
        lines = []
        for k, cond in enumerate(np.unique(independent_var)):
            lines.append(axes[ax_num].plot(np.nanmean(test_var[independent_var==cond],0),
                                        color = viridis_big.colors[k]))
            y = np.nanmean(test_var[independent_var==cond],0)
            err = np.nanstd(test_var[independent_var==cond],0)/np.sqrt(sum(independent_var==cond))
            axes[ax_num].fill_between(range(1,np.shape(test_var)[1]+1),y - err, y + err,
                color = viridis_big.colors[k],
                alpha = .3,
                linewidth = 0)
        lines = [line[0] for l, line in enumerate(lines)]

        axes[ax_num].legend(title = probability_method,
                            handles = lines,
                            labels = [np.round(item,2) for item in np.unique(independent_var)],
                            loc='upper left')
        
        
        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(200))
        axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000,1))
        if a ==1:
            axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(200))
            axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.2,1))
        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(40))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000+0.2,2))
        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(200))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.2,1))
        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(10))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.05,2))
        axes[ax_num].set_ylabel(test_var_label)
        axes[ax_num].set_xlabel('Time (s), aligned to '+alignment_labs[a])

        # %%
        # Bunch O lines
        ax_num += 1
        for k, pos in enumerate(test_var):
            if k % 20 == 0:
                axes[ax_num].plot(pos, 
                                color = viridis((independent_var[k]-min(independent_var_unique))/(max(independent_var_unique)-min(independent_var_unique))))
        axes[ax_num].legend(title = probability_method,
                            handles = lines,
                            labels = [np.round(item,2) for item in np.unique(independent_var)],
                            loc='upper left')
        # sm = plt.cm.ScalarMappable(cmap=cm.get_cmap('viridis'))
        # divider = make_axes_locatable(axes[ax_num])
        # cax2 = divider.append_axes("right", size="3%", pad=0.05)
        # cbar = fig.colorbar(sm,
        #                     ticks = [(item-min(independent_var_unique))/(max(independent_var_unique)-min(independent_var_unique)) for item in independent_var_unique],
        #                     ax=axes[ax_num],
        #                     pad=0.004,
        #                     cax = cax2)
        # cbar.ax.set_yticklabels([str(np.round(item,2)) for item in independent_var_unique])
        # cbar.set_label('Estimated Probability of Reward')

        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(200))
        axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000,1))
        if a ==1:
            axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(200))
            axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.2,1))

        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(40))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000,2))
        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(100))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.2,1))
        # axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(10))
        # axes[ax_num].set_xticklabels(np.round(axes[ax_num].get_xticks()/1000-0.05,2))
        axes[ax_num].set_ylabel(test_var_label)
        axes[ax_num].set_xlabel('Time (s), aligned to '+alignment_labs[a])
        ax_num += 1
    ax_num += 1
        # fig_str = ('SPM_'+labels2[test_var_num][0:3]+'_'+probability_method+'peakvalign.pdf')
plt.tight_layout()
# fig_str = 'Avg_plot_ALL_subj0.pdf'
fig_str = 'Avg_plot_ALL.pdf'
fig.savefig(fig_str,format = 'pdf')
#%%
os.chdir('..')