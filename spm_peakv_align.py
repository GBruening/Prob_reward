#%%
# Loading Data

import pickle
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable
from scipy.interpolate import interp1d

# Load the data
exp_name = '4t_180trial_4block' # Change this to determine which data set to use
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
    if file.endswith('.pickle') and file[11:13]=='4t':
        # file_name = 'vigor_conf_'+exp_name+'_'+str(n_files)+'.pickle'
        file_name = file
        with open(file_name,'rb') as f:
            temp = pickle.load(f)
            peakv_subj_sum = 0
            for t, trial in enumerate(temp[0].items()):
                peakv_subj_sum += np.max(trial[1]['rad_v'])
            peakv_subj_sum = peakv_subj_sum/736
            if peakv_subj_sum < 1:
                cfdata.append(temp[0])
                target_data.append(temp[1])
            del temp
        n_files += 1
        # if n_files > 3:
        #     break
os.chdir('..')

os.chdir(dname+'\\..\\')
os.chdir(exp_name)
cfdf = pd.read_csv('vigor_conf_'+exp_name+'.csv')

# rm_subjects = [1,6,12]
# peakv_avg = []
# for s, subj in enumerate(cfdata):
#     peakv_subj_sum = 0
#     for t, trial in enumerate(subj.items()):
#         peakv_subj_sum += np.max(trial[1]['rad_v'])
#     peakv_avg.append(peakv_subj_sum/736)

#%%
# Functions
def interp_func(data, onset_to_moveback):
        # Do Some Normalization stuff
        f2 = interp1d(onset_to_moveback,data,kind='cubic')
        xnew = np.linspace(onset_to_moveback[0],onset_to_moveback[-1], num = 1000, endpoint = True)
        return f2(xnew)

for s in range(0,10):
    for tar in [1,2,3,4]:
        r_sum  = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==1'))
        nr_sum = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==0'))
        f_sum  = len(cfdf.query('subj=='+str(s)).query('trial>16').query('target=='+str(tar)).query('rewarded==-1'))
        if r_sum != 90:
            print('Subject '+str(s)+' Target '+str(tar)+': Rewarded '+str(r_sum)+', Not '+str(nr_sum))
        if r_sum+nr_sum+f_sum != 180:
            print('Subject '+str(s)+' Target '+str(tar)+': Rewarded '+str(r_sum)+', Not '+str(nr_sum)+', F '+str(f_sum))

for s in range(0,10):
    for tar in  [1,2,3,4]:
        n_tar = len(cfdf.query('subj=='+str(s)).query('target=='+str(tar)))
        if n_tar != 184:
            print('Subject '+str(s)+' Target '+str(tar)+': '+str(n_tar))

#%%
# onset peakv at_target offset target_show moveback
onset_or_targetshow = 'onset' #'move_back'
offset_or_moveback = 'move_back' # 'offset'

min_thin_onset = 100000
min_thin_target_show = 100000
min_thin_peakv = 100000
max_thin_peakv = 100000
for s, subj in enumerate(cfdata):
    for t, trial in enumerate(subj.items()):
        if max(trial[1]['P']) > .08 and len(trial[1]['P'])-trial[1]['vigor']['idx']['peakv']>500:
            min_thin_onset = min([min_thin_onset,
                                len(trial[1]['P'])-trial[1]['vigor']['idx']['onset']])
            min_thin_target_show = min([min_thin_target_show,
                                        len(trial[1]['P'])-trial[1]['vigor']['idx'][onset_or_targetshow]])
            min_thin_peakv = min([min_thin_peakv,trial[1]['vigor']['idx']['peakv']])
            if trial[1]['vigor']['idx']['peakv'] == 98:
                asdfsdafsadffsda
            max_thin_peakv = min([max_thin_peakv,len(trial[1]['P'])-trial[1]['vigor']['idx']['peakv']])
# min_thin_peakv = max(min_thin_peakv,500)
min_thin_peakv = 500

# Put data into correct format
r_prob = []
est_prob = []
RPE = []
prior_RPE = []
c = []
subject = []

test_var_abs = ['p','x','y','v','v_rad','vx','vy','a','ax','ay']
test_var_norm = [item+'_norm' for item in test_var_abs]
test_var_peakv_align= [item+'_peakv_align' for item in test_var_abs]
test_var_names = ['P',
        'px_abs',
        'py_abs',
        'v_sign',
        'rad_v',
        'vx_abs',
        'vy_abs',
        'a_sign',
        'ax_abs',
        'ay_abs']
for item in test_var_abs:
    exec(item+'= []')
for item in test_var_norm:
    exec(item+'= []')
for item in test_var_peakv_align:
    exec(item+'= []')

#%% Loopin
for s, subj in enumerate(cfdata):
    trial_count = 0
    c1 = 0
    c2 = 0
    c3 = 0
    c4 = 0
    for t, trial in enumerate(subj.items()):
        if max(trial[1]['P']) > .08 and len(trial[1]['P'])-trial[1]['vigor']['idx']['peakv']>500:
            block = 0
            trial_in_block = (t-16) % 180
            if trial_in_block == 0:
                c1 = 0
                c2 = 0
                c3 = 0
                c4 = 0
                block += 1

            if t < 50 or trial[1]['rewarded']==-1:
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
            # onset_to_moveback = np.arange(trial[1]['vigor']['idx']['onset'],trial[1]['vigor']['idx']['move_back']+1)
            
            onset_to_movebackish = np.arange(trial[1]['vigor']['idx'][onset_or_targetshow],
                                            trial[1]['vigor']['idx'][onset_or_targetshow]+min_thin_target_show-1)
            onset_to_offset = np.arange(trial[1]['vigor']['idx'][onset_or_targetshow],
                                        trial[1]['vigor']['idx'][offset_or_moveback])
            peak_v_align = np.arange(trial[1]['vigor']['idx']['peakv']-500+1,
                                    trial[1]['vigor']['idx']['peakv']+500-1)
            for it, item in enumerate(test_var_names):
                if 'X' in item:
                    flipit = flipx
                else:
                    flipit = 1

                if 'Y' in item:
                    flipit = flipy
                else:
                    flipit = 1

                data = np.array(trial[1][str(item)])[onset_to_movebackish]*flipit
                exec(test_var_abs[it]+'.append(data)')

                data = np.array(trial[1][str(item)])[onset_to_offset]*flipit
                interped = interp_func(data,onset_to_offset)
                exec(test_var_norm[it]+'.append(interped)')

                data = np.array(trial[1][str(item)][peak_v_align])*flipit
                exec(test_var_peakv_align[it]+'.append(data)')
            
            est_prob.append(trial[1]['est_prob'])
            r_prob.append(trial[1]['r_prob'])
            RPE.append(trial[1]['RPE'])
            prior_RPE.append(trial[1]['prior_RPE'])
            trial_count += 1
    print('Subj: '+str(s)+', Trial: '+str(trial_count))

#%%
arr_variables = ['c','subject','p','x','y','v',
                 'v_rad','vx','vy','a','ax','ay',
                 'est_prob','r_prob','RPE','prior_RPE']
labels = ['Condition',
           'Subject',
           'Position (m)',
           'X Position (m)',
           'Y Position (m)',
           'Velocity (m/s)',
           'Radial Velocity (m/s)',
           'X Velocity (m/s)',
           'Y Velocity (m/s)',
           'Acceleration (m^2/s)',
           'X Acceleration (m^2/s)',
           'Y Acceleration (m^2/s)',
           'Reward Probability',
           'Reward Prediction Error (RPE)',
           'Reward Prediction Error Previous Trial (RPE)'
]
labels2 = ['Position',
           'X_Position',
           'Y_Position',
           'Velocity',
           'Radial Velocity',
           'X_Velocity',
           'Y_Velocity',
           'Acceleration',
           'X_Acceleration',
           'Y_Acceleration'
]

for item in arr_variables:
    exec(item + ' = np.array('+item+')')

for item in test_var_norm:
    exec(item + ' = np.array('+item+')')

for item in test_var_peakv_align:
    exec(item + ' = np.array('+item+')')

# %%
for cond in np.arange(1,5):
    count = 0
    for item in c:
        if item == cond:
            count += 1
    print('Count for condition '+str(cond)+': '+str(count))

for cond in [0.00,0.33,0.66,1.00]:
    count = 0
    for item in r_prob:
        if item == cond:
            count += 1
    print('Count for condition '+str(cond)+': '+str(count))

# %%
# Do Spm Stuff
import spm1d
from matplotlib import cm
import matplotlib.gridspec as gridspec

os.chdir('Graphs')

RPEs = [np.round(item,2) for item in np.unique(RPE)]
probability_method = 'r_prob'

for probability_method in ['RPE','prior_RPE','r_prob',]:#
    if probability_method == 'r_prob':
        independent_var = r_prob
    elif probability_method == 'RPE':
        independent_var = RPE
    elif probability_method == 'prior_RPE':
        independent_var = prior_RPE
    else:
        raise 'Didn\'t select a probability method'
    independent_var_unique = np.unique(independent_var)
    independent_var_labs = [np.round(item,2) for item in np.unique(independent_var)]

    for test_var_num in [0,3,4,7]:#range(len(test_var_abs)):
        plt.close('all')
        fig = plt.figure(figsize = (10,20))
        widths = [1, 1]
        # heights = [.1, 1, .2, 1, .5, 1, .2, 1]
        heights = [.8, 1, .2, 1, .5, 1, .2, 1]
        spec5 = fig.add_gridspec(ncols=len(widths), nrows=len(heights), width_ratios=widths,height_ratios=heights)
        count = 0
        axes=[]
        for row in range(len(heights)):
            for col in range(len(widths)):
                axes.append(fig.add_subplot(spec5[row, col]))
                count += 1

        print('Processing SPM '+labels2[test_var_num]+' '+ probability_method)
        # hide_axis_numbers = [1,2,5,6,9,10,13,14]
        hide_axis_numbers = [0,1,4,5,8,9,12,13]
        for number in hide_axis_numbers:
            axes[number].axis('off')

            # axis_numbers = [3,4,5,6,9,10,11,12,15,16,17,18,21,22,23,24]
            # axis_numbers = [num - 1 for num in axis_numbers]

        # fig.text(.5,.91,'Absolute data',size = 30, ha='center')
        # fig.text(.5,.89,'ANOVA Results',size = 20, ha='center')
        # fig.text(.5,.69,'Regression Results',size = 20, ha='center')

        # fig.text(.5,.48,'Normalized data',size = 30, ha='center')
        # fig.text(.5,.46,'ANOVA Results',size = 20, ha='center')
        # fig.text(.5,.28,'Regression Results',size = 20, ha='center')

        fig.text(.5,.91,'Absolute data',size = 30, ha='center')
        fig.text(.5,.88,'ANOVA Results',size = 20, ha='center')
        fig.text(.5,.66,'Regression Results',size = 20, ha='center')

        fig.text(.5,.43,'Normalized data',size = 30, ha='center')
        fig.text(.5,.40,'ANOVA Results',size = 20, ha='center')
        fig.text(.5,.185,'Regression Results',size = 20, ha='center')

        fig.text(.5,.95,labels[test_var_num+2]+' SPM Analysis',size = 40, ha='center')
        ax_num = 2
        
        test_var_label = labels[test_var_num+2]
        test_var = []
        exec('test_var = ' + test_var_peakv_align[test_var_num])
        norm_lab = 'Peak Velocity Align'

        viridis = cm.get_cmap('viridis')
        viridis_big = cm.get_cmap('viridis', len(np.unique(independent_var)))
        viridis_4 = cm.get_cmap('viridis', 4)
        
        ## Anova
        lines = []
        for k, cond in enumerate(np.unique(independent_var)):
            lines.append(spm1d.plot.plot_mean_sd(test_var[independent_var==cond],
                        linecolor = viridis_big.colors[k],
                        ax = axes[ax_num]))
        lines = [line[0] for l, line in enumerate(lines)]
#%%

        axes[ax_num].legend(handles = lines,
                            labels = [np.round(item,2) for item in np.unique(independent_var)],
                            loc='upper left')

        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(100))
        axes[ax_num].set_xticklabels(axes[ax_num].get_xticks()/1000)
        axes[ax_num].set_ylabel(test_var_label)
        axes[ax_num].set_xlabel('Aligned to Peak V')

        ax_num += 1
        ## plot SPM results:
        # F    = spm1d.stats.anova1(test_var, independent_var, equal_var = True )
        F    = spm1d.stats.anova1(test_var, c, equal_var = True )
        Fi   = F.inference(0.05)
        Fi.plot(ax=axes[ax_num], label='Between-subjects analysis')
        # Frm  = spm1d.stats.anova1rm( test_var, c, subject, equal_var = True )
        # Firm = Frm.inference(0.05)
        # Firm.plot(ax=axes[ax_num], color='r', facecolor=(0.8,0.3,0.3), label='Within-subjects analysis')
        # Firm.plot_p_values(ax=axes[ax_num]) 
        # Firm.plot_threshold_label(ax=axes[ax_num], pos=(10,5.0))
        # axes[ax_num].set_title(label = 'rmANOVA p-value: '+str(Firm.p_set))
        axes[ax_num].legend(fontsize=8)
        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(100))
        axes[ax_num].set_xticklabels(axes[ax_num].get_xticks()/1000)
        axes[ax_num].set_xlabel('Aligned to Peak V')

        print('     ANOVArm SPM '+norm_lab+' '+labels2[test_var_num]+' done.')
        
        # %%
        # Regression
        ax_num += 3
        for k, pos in enumerate(test_var):
            if k % 20 == 0:
                axes[ax_num].plot(pos, 
                                    color = viridis((independent_var[k]-min(independent_var_unique))/(max(independent_var_unique)-min(independent_var_unique))))
        sm = plt.cm.ScalarMappable(cmap=cm.get_cmap('viridis'))
        divider = make_axes_locatable(axes[ax_num])
        cax2 = divider.append_axes("right", size="3%", pad=0.05)
        cbar = fig.colorbar(sm,
                            ticks = [(item-min(independent_var_unique))/(max(independent_var_unique)-min(independent_var_unique)) for item in independent_var_unique],
                            ax=axes[ax_num],
                            pad=0.004,
                            cax = cax2)
        cbar.ax.set_yticklabels([str(np.round(item,2)) for item in independent_var_unique])
        cbar.set_label('Estimated Probability of Reward')

        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(100))
        axes[ax_num].set_xticklabels(axes[ax_num].get_xticks()/1000)
        axes[ax_num].set_xlabel('Aligned to Peak V')
        axes[ax_num].set_ylabel(test_var_label)

        ax_num += 1
        t = spm1d.stats.regress(test_var, independent_var)
        ti = t.inference(alpha=0.05)
        ti.plot(ax=axes[ax_num])
        ti.plot_p_values(ax=axes[ax_num]) 
        ti.plot_threshold_label(ax=axes[ax_num], pos=(10,5.0))
        axes[ax_num].set_title(label = 'Regression p-value: '+str(ti.p_set))
        axes[ax_num].xaxis.set_major_locator(plt.MultipleLocator(100))
        axes[ax_num].set_xticklabels(axes[ax_num].get_xticks()/1000)
        axes[ax_num].set_xlabel('Aligned to Peak V')
        plt.tight_layout()
        ax_num += 3
        print('     Regress SPM '+norm_lab+' '+labels2[test_var_num]+' done.')
        
        print('   Done with SPM '+labels2[test_var_num]+' '+probability_method)
        fig_str = ('SPM_'+labels2[test_var_num][0:3]+'_'+probability_method+'_'+
            'PeakV_Align'+'.pdf')
        fig.savefig(fig_str,format = 'pdf')
        
        a = 1
#%%
os.chdir('..')