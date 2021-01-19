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
library(english)

#====================== Load Fonts ================
# font_import()
loadfonts(device='win')

theme_set(theme_classic(base_family = "Times")+
            theme(text = element_text(color='black'),
                  axis.text.x = element_text(color='black'),
                  axis.text.y = element_text(color='black'),
                  axis.ticks  = element_line(color='black'),
                  axis.line = element_line(color='black',linetype='solid'),
                  plot.title = element_text(hjust = 0.5)))

# # Set these lines to the base directory of the experiment.
# setwd('d:/Users/Gary/Google Drive/Yan Experiment')
# setwd('F:/Google Drive/Yan Experiment')
main_folder = 'D:/Google Drive/Yan Experiment'
setwd(main_folder)

exp_name = '4t_180trial_4block'
setwd(exp_name)
raw_data = read.csv(paste('vigor_conf_',exp_name,'.csv',sep=''))

raw_data$vigor = 1/(raw_data$react_time+raw_data$move_dur)
raw_data$total_dur = raw_data$react_time+raw_data$move_dur

#====================== Filter and add Probs ================
rm_fail_data = raw_data[raw_data$rewarded != -1,]
famil_data = filter(rm_fail_data,trial<=16)
prefilt_data = filter(rm_fail_data,trial<=(16+36))

non_famil_data = rm_fail_data[rm_fail_data$trial >= 17,]

trials = c()
counter = 1
subj = 1
for (k in 1:length(non_famil_data$trial)){
  if (non_famil_data[k,]$subj!=subj){
    counter = 1
    subj = non_famil_data[k,]$subj
  }
  trials = c(trials,counter)
  if (non_famil_data[k,'diff_prob']>0){
    non_famil_data[k,'diff_prob'] = non_famil_data[k,'diff_prob']-.01
  }
  counter = counter + 1
}
non_famil_data$trial_in_block= (trials-1)%%180+1

blocks = c()
probs = c()
counter = 1
subj = 1
block = 1

block_prob = matrix(c(1,.66,.33,0,.33,0,1,.66,0,1,.66,.33,.66,.33,0,1),
                    nrow = 4,
                    byrow = TRUE)

for (k in 1:length(non_famil_data$trial)){
  if (non_famil_data[k,]$subj!=subj){
    subj = non_famil_data[k,]$subj
    block = 1
  }
  blocks = c(blocks,block)
  probs = c(probs,block_prob[block,non_famil_data[k,]$target])
  if (non_famil_data[k,]$trial_in_block == 180){
    block = block + 1
  }
}
non_famil_data$probs = probs
non_famil_data$block = blocks

add_n_back <- function(data,n_back){
  data[1,paste(as.english(n_back),'_back_rate',sep='')] = 0
  data[1,paste(as.english(n_back),'_back_rate_tar',sep='')] = 0
  for (k in 1:length(data$subj)){
    subject = data[k,'subj']
    block = data[k,'block']
    cur_tar = data[k,'target']
    cur_trial = data[k,'trial_in_block']
    
    rate = 0
    tar_rate = 0
    nb = 0
    last_back = cur_trial-n_back
    
    if (last_back <= 0){
      last_back = 1
    }
    if(cur_trial == 1){
      rate = 0
      tar_rate = 0
    } else {
      for (j in (cur_trial-1):last_back){
        nb = nb + 1
        rate = rate + (.5^(nb))*data[data$subj == subject & data$block == block & data$trial_in_block == j,'rewarded']
        
        if (data[j,'target'] == cur_tar){
          tar_rate = tar_rate + (.5^(nb))*data[j,'rewarded']
        } else{
          tar_rate = tar_rate + (.25^(nb))*data[j,'rewarded']
        }
      }
    }
    data[k,paste(as.english(n_back),'_back_rate',sep='')] = rate
    data[k,paste(as.english(n_back),'_back_rate_tar',sep='')] = tar_rate
  }
  return(data)
}

for (nbacks in c(1)){#},3,5,10)){
  non_famil_data <- add_n_back(non_famil_data,nbacks)
}


# 'RWD_Run', #
# 'NRWD_Run', #
# 'Prior_Target', #
# 'Prior_Prob', #
# 'Ave_2_RWD', # Number of rewards not average
# 'Ave_3_RWD', # Change to num 2 reward
# 'Ave_4_RWD', #
# 'Ave_5_RWD', #
# 'Ave_10_RWD', #
# 'Ave_2_Prob', # Average of 
# 'Ave_3_Prob', #
# 'Ave_4_Prob', #
# 'Ave_5_Prob', #
# 'Ave_10_Prob', #

# ==================== Renaming =================
dan_names = c('Subject',
              'Trial',
              'Block',
              'Prior_RWD',
              'Target',
              'Probability',
              'Vigor',
              'Duration',
              'TotalDur',
              'PeakV',
              'ReturnPeakV',
              'RT',
              'RWD',
              'Error_Dist',
              'Error_Ang',
              'Ret_Error',
              'Max_Ex',
              'Trial_in_Block',
              'Target_num',
              'NRWD_Run')

gary_names = c('subj',
               'trial',
               'block',
               'one_back_rate',
               'target',
               'probs',
               'vigor',
               'move_dur',
               'total_dur',
               'peak_vel',
               'peak_vel_moveback',
               'react_time',
               'rewarded',
               'error_dist',
               'error_angle',
               'move_back_error',
               'maxex',
               'trial_in_block',
               'target_num',
               't_since_reward')

k = 0
for (dan_name in dan_names){
  k = k+1
  names(non_famil_data)[names(non_famil_data) == gary_names[k]] <- dan_name
}
names(non_famil_data)[names(non_famil_data) == 'react_vel'] <- 'RT_vel'

#====================== Create Data Frame ================
testable_vars_abs = c('Vigor',
                      'TotalDur',
                      'Duration',
                      'PeakV',
                      'ReturnPeakV',
                      'RT',
                      'RT_vel',
                      'Error_Dist',
                      'Error_Ang',
                      'Ret_Error',
                      'Max_Ex')

testable_labels_abs = c('Vigor (???)',
                        'Total Duration (s)',
                        'Movement Duration (s)',
                        'Peak Velocity (m/s)',
                        'Peak Velocity Moveback (m/s)',
                        'Reaction time (s)',
                        'Reaction Velocity (m/s)',
                        'Error Distance (m)',
                        'Error Angle (deg)',
                        'Return Error (m)',
                        'Maximum Excursion (m)')

testable_vars_norm = c('NormVigor',
                       'NormTotalDur',
                       'NormDuration',
                       'NormPeakV',
                       'NormReturnPeakV',
                       'NormRT',
                       'NormRT_vel',
                       'NormError_Dist',
                       'NormError_Ang',
                       'NormRet_Error',
                       'NormMax_Ex')

testable_labels_norm = c('Vigor Norm (???)',
                         'Total Duration Norm (s)',
                         'Movement Duration Norm (s)',
                         'Peak Velocity Norm (m/s)',
                         'Peak Velocity Moveback Norm (m/s)',
                         'Reaction time Norm (s)',
                         'Reaction Velocity Norm (m/s)',
                         'Error Distance Norm (m)',
                         'Error Angle Norm (deg)',
                         'Return Error Norm (m)',
                         'Maximum Excursion Norm (m)')

testable_vars_diff = c('DeltaVigor',
                       'DeltaTotalDur',
                       'DeltaDuration',
                       'DeltaPeakV',
                       'DeltaReturnPeakV',
                       'DeltaRT',
                       'DeltaRT_vel',
                       'DeltaError_Dist',
                       'DeltaError_Ang',
                       'DeltaRet_Error',
                       'DeltaMax_Ex')

testable_labels_diff = c('Vigor Delta (???)',
                         'Total Duration Delta (s)',
                         'Movement Duration Delta (s)',
                         'Peak Velocity Delta (m/s)',
                         'Peak Velocity Moveback Delta (m/s)',
                         'Reaction time Delta (s)',
                         'Reaction Velocity Delta (m/s)',
                         'Error Distance Delta (m)',
                         'Error Angle Delta (deg)',
                         'Return Error Delta (m)',
                         'Maximum Excursion Delta (m)')

# CHECK RADIAL VELOCITY?


#====================== Add Normalization ================
# Add Normalization to Not Filtered data.
for (item in testable_vars_abs){
  non_famil_data[,paste(item, '_norm',sep='')] = 0
  non_famil_data[,paste(item, '_diff',sep='')] = 0
  item_norm = paste(item, '_norm',sep='')
  item_diff = paste(item, '_diff',sep='')
  
  for (Subject_num in unique(non_famil_data$Subject)){
    for (tar in unique(non_famil_data$Target)){
      mean_0 = mean(non_famil_data[non_famil_data$r_prob==0 & non_famil_data$Subject == Subject_num & non_famil_data$Target == tar,item])
      
      non_famil_data[non_famil_data$Subject == Subject_num & non_famil_data$Target == tar,item_norm] <- non_famil_data[non_famil_data$Subject == Subject_num & non_famil_data$Target == tar,item]/mean_0
      non_famil_data[non_famil_data$Subject == Subject_num & non_famil_data$Target == tar,item_diff] <- non_famil_data[non_famil_data$Subject == Subject_num & non_famil_data$Target == tar,item]-mean_0
    }
  }
}

# Add Normalization to filtered data.
filt_data = c()
# filt_data = filter(non_famil_data,Trial_in_Block>36)
filt_data = non_famil_data

for (item in testable_vars_abs){
  filt_data[,paste('Norm',item,sep='')] = 0
  filt_data[,paste('Delta',item,sep='')] = 0
  item_norm = paste('Norm',item,sep='')
  item_diff = paste('Delta',item,sep='')
  
  for (Subject_num in unique(filt_data$Subject)){
    
    for (tar in unique(filt_data$Target)){
      mean_0 = mean(filt_data[filt_data$r_prob==0 & filt_data$Subject == Subject_num & filt_data$Target == tar,item])
      
      filt_data[filt_data$Subject == Subject_num & filt_data$Target == tar,item_norm] <- filt_data[filt_data$Subject == Subject_num & filt_data$Target == tar,item]/mean_0
      filt_data[filt_data$Subject == Subject_num & filt_data$Target == tar,item_diff] <- filt_data[filt_data$Subject == Subject_num & filt_data$Target == tar,item]-mean_0
    }
  }
  if (as.numeric(as.character(mean(filt_data[filt_data$r_prob==0,item_norm]))) != 1){
    mean(filt_data[filt_data$r_prob==0,item_norm])
    print(item)
  }
}

# Init some lists
ano_res <- list()
ano_res_t14 <- list()
lme_test <- list()
lme_test_t14 <- list()

#====================== Loopin ================
setwd(paste('Graphs/',sep = ''))
