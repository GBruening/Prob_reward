
library(ggplot2)
library(ggthemes)
library(lmtest)
library(tidyr)
library(dplyr)
library(multcomp)
library(lme4)
library(nlstools)
library(knitr)
library("RColorBrewer")
library(wesanderson)
library(extrafont)
library(cowplot)
library(latex2exp)
library('R.matlab')
library(boot)
library(viridis)
library(combinat)

#====================== Load Fonts
# font_import()
loadfonts(device='win')


theme_set(theme_classic(base_family = "Times")+
            theme(text = element_text(color='black'),
                  axis.text.x = element_text(color='black'),
                  axis.text.y = element_text(color='black'),
                  axis.ticks  = element_line(color='black'),
                  axis.line = element_line(color='black',linetype='solid'),
                  plot.title = element_text(hjust = 0.5)))


# setwd('d:/Users/Gary/Google Drive/Yan Experiment')
setwd('F:/Google Drive/Yan Experiment')

exp_name = '4t_180trial_4block'
setwd(exp_name)
test = read.csv(paste('vigor_conf_',exp_name,'.csv',sep=''))

test = filter(test,trial>16,error_dist < .05)
test$subj = test$subj+1

trials = c()
counter = 1
subj = 1
for (k in 1:length(test$trial)){
  if (test[k,]$subj!=subj){
    counter = 1
    subj = test[k,]$subj
  }
  trials = c(trials,counter)
  if (test[k,'diff_prob']>0){
    test[k,'diff_prob'] = test[k,'diff_prob']-.01
  }
  counter = counter + 1
}
test$trial_in_block= (trials-1)%%180+1

blocks = c()
probs = c()
counter = 1
subj = 1
block = 1

block_prob = matrix(c(1,.66,.33,0,.33,0,1,.66,0,1,.66,.33,.66,.33,0,1),
                    nrow = 4,
                    byrow = TRUE)

for (k in 1:length(test$trial)){
  if (test[k,]$subj!=subj){
    subj = test[k,]$subj
    block = 1
  }
  blocks = c(blocks,block)
  probs = c(probs,block_prob[block,test[k,]$target])
  if (test[k,]$trial_in_block == 180){
    block = block + 1
  }
}
test$probs = probs
test$block = blocks

testable_vars_abs = c('peak_vel',
                      'peak_vel_moveback',
                      'move_dur',
                      'react_time',
                      'error_dist',
                      'react_vel',
                      'error_angle',
                      'maxex')
testable_labels_abs = c('Peak Velocity (m/s)',
                        'Peak Velocity Moveback (m/s)',
                        'Movement Duration (s)',
                        'Reaction time (s)',
                        'Error Distance (m)',
                        'Reaction Velocity (m/s)',
                        'Error Angle (deg)',
                        'Maximum Excursion (m)')

testable_vars_norm = c('peak_vel_norm',
                       'peak_vel_moveback_norm',
                       'move_dur_norm',
                       'react_time_norm',
                       'error_dist_norm',
                       'react_vel_norm',
                       'error_angle_norm',
                       'maxex_norm')
testable_labels_norm = c('Peak Velocity Norm (m/s)',
                         'Peak Velocity Moveback Norm (m/s)',
                         'Movement Duration Norm (s)',
                         'Reaction time Norm (s)',
                         'Error Distance Norm (m)',
                         'Reaction Velocity Norm (m/s)',
                         'Error Angle Norm (deg)',
                         'Maximum Excursion Norm (m)')

testable_vars_diff = c('peak_vel_diff',
                       'peak_vel_moveback_diff',
                       'move_dur_diff',
                       'react_time_diff',
                       'error_dist_diff',
                       'react_vel_diff',
                       'error_angle_diff',
                       'maxex_diff')

testable_labels_diff = c('Peak Velocity Diff (m/s)',
                         'Peak Velocity Moveback Diff (m/s)',
                         'Movement Duration Diff (s)',
                         'Reaction time Diff (s)',
                         'Error Distance Diff (m)',
                         'Reaction Velocity Diff (m/s)',
                         'Error Angle Diff (deg)',
                         'Maximum Excursion Diff (m)')

# CHECK RADIAL VELOCITY?

for (item in c(testable_vars_abs,testable_vars_norm,testable_vars_diff)){
  test[,paste(item, '_norm',sep='')] = 0
  test[,paste(item, '_diff',sep='')] = 0
  item_norm = paste(item, '_norm',sep='')
  item_diff = paste(item, '_diff',sep='')
  
  for (subj_num in unique(test$subj)){
    subj_filt = filter(test,subj==subj_num)
    
    mean_0 = mean(filter(subj_filt, r_prob == 0)[,item])
    test[test$subj == subj_num,item_norm] = test[test$subj == subj_num,item]/mean_0
    test[test$subj == subj_num,item_diff] = test[test$subj == subj_num,item]-mean_0
  }
}

filt_data = c()
filt_data = filter(test,trial_in_block>36)

# testable_vars = c(testable_vars,testable_vars_norm)
# testable_labels = c(testable_labels,testable_labels_norm)

ano_res <- list()
ano_res_t14 <- list()
lme_test <- list()
lme_test_t14 <- list()

aov_p_vals = c()
aov_p_vals_t14 = c()
lme_p_vals = c()
lme_p_vals_t14 = c()

prob_method = 'r_prob'
testing_var = 'peak_vel'
norm_meth = 'abs'

testing_data = filt_data[,c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob',prob_method,testing_var)]
colnames(testing_data) = c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob','probability_metric', 'testing_var')

n_samps = c(3,5,7,8)
# prob_arr = matrix(, nrow = 500, ncol = length(n_samps))
prob_list = c()
samp_list = c()

samp_count = 0
for (n_samp in n_samps){
  samp_count = samp_count + 1
  subjects = combn(c(1,2,3,4,5,6,7,8,9,10),n_samp)
  temp = c()
  for (subjs in split(subjects, rep(1:ncol(subjects), each = nrow(subjects)))){
    pval = cftest(lmer(testing_var ~ factor(target) + probability_metric + (1|subj),
                       data = filter(testing_data,subj %in% subjs)))
    temp = c(temp,pval$test$pvalues[5])
  }
  # prob_arr[1:length(temp),samp_count] = temp
  samp_list = c(samp_list,rep(n_samp,length(temp)))
  prob_list = c(prob_list,temp)
}
# prob_arr = as.data.frame(prob_arr)
# colnames(prob_arr) = c('3','5','7','9')

prob_df = as.data.frame(cbind(samp_list,prob_list))
colnames(prob_df) = c('num_samp','probs')

ggplot(data=prob_df,aes(x=probs))+
  geom_histogram(aes(y=..density..),
                 color='black',fill='lightgrey',bins=30)+
  facet_wrap(.~num_samp)+
  labs(x = 'P-Value for LME', y = '# of Occurances/Density')


plots = list()
for (n_samp in n_samps){
  frac = sum(filter(prob_df,num_samp==n_samp)$probs<0.05)/length(filter(prob_df,num_samp==n_samp)$probs)
  plots[[n_samp]] <- ggplot(data=filter(prob_df,num_samp==n_samp),
                       aes(x=probs))+
                  geom_histogram(aes(y=..density..),
                                 color='black',fill='lightgrey',bins=30)+
                  labs(x = 'P-Value for LME',
                       y = '# of Occurances/Density',
                       title = paste(n_samp,' Random Subjects\nFraction below 0.05: ',round(frac,3),sep=''))
    
}
group_plot = plot_grid(plots[[3]],
                       plots[[5]],
                       plots[[7]],
                       plots[[9]])
