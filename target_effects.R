---
title: "target_effects"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

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
setwd('D:/Google Drive/Yan Experiment')

exp_name = '4t_180trial_4block'
setwd(exp_name)
test = read.csv(paste('vigor_conf_',exp_name,'.csv',sep=''))

#====================== Filter and add Probs ================
famil_data = filter(test,trial<=16)
prefilt_data = filter(test,trial<=(16+36))
test = filter(test,trial>16,error_dist < .05)

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

add_n_back <- function(data,n_back){
  data[1,paste(as.english(n_back),'_back_rate',sep='')] = 0
  data[1,paste(as.english(n_back),'_back_rate_tar',sep='')] = 0
  for (k in 2:length(data$subj)){
    subject = data[k,'subj']
    block = data[k,'block']
    cur_tar = data[k,'target']
    rate = 0
    tar_rate = 0
    nb = 0
    last_back = k-n_back
    if (last_back <= 0){
      last_back = 1
    }
    for (j in (k-1):last_back){
      if (data[j,'subj'] != subject | data[j,'block'] != block){
        # print(paste('Breaking at ',j,sep=''))
        break
      }
      nb = nb + 1
      rate = rate + (.5^(nb))*data[j,'rewarded']
      
      if (data[j,'target'] == cur_tar){
        tar_rate = tar_rate + (.5^(nb))*data[j,'rewarded']
      } else{
        tar_rate = tar_rate + (.25^(nb))*data[j,'rewarded']
      }
    }
    data[k,paste(as.english(n_back),'_back_rate',sep='')] = rate
    data[k,paste(as.english(n_back),'_back_rate_tar',sep='')] = tar_rate
  }
  return(data)
}

for (nbacks in c(1)){#},3,5,10)){
  test <- add_n_back(test,nbacks)
}
# 
# add_n_back <- function(data,n_back){
#   data[1,paste(n_back,'_back_rate',sep='')] = 0
#   n_back_str = paste(n_back,'_back_rate',sep='')
#   n_back_tar_str = paste(n_back,'_back_rate_tar',sep='')
#   for (k in 2:length(data$subj)){
#     if (data[k,'subj'] != data[k-1,'subj'] | data[k,'block'] != data[k-1,'block']){
#       data[k,n_back_str] = 0
#       data[k,n_back_tar_str] = 0
#     }
#     
#     data[k,n_back_str] = .5*data[k-1,n_back_str] + (.5*data[k-1,'rewarded'])
#     
#     if (data[k,'target'] == data[k-1,'target']){
#       tar_rate = .5*data[k-1,n_back_tar_str] + (.5*data[k-1,'rewarded'])
#     } else{
#       tar_rate = .25*data[k-1,n_back_tar_str] + (.25*data[k-1,'rewarded'])
#     }
#     
#     
#     subject = data[k,'subj']
#     block = data[k,'block']
#     cur_tar = data[k,'target']
#     rate = 0
#     tar_rate = 0
#     nb = 0
#     last_back = k-n_back
#     if (last_back <= 0){
#       last_back = 1
#     }
#     for (j in k:last_back){
#       if (data[j,'subj'] != subject | data[j,'block'] != block){
#         # print(paste('Breaking at ',j,sep=''))
#         break
#       }
#       nb = nb + 1
#       rate = rate + (.5^(nb-1))*data[j,'rewarded']
#       
#       if (data[j,'target'] == cur_tar){
#         tar_rate = tar_rate + (.5^(nb-1))*data[j,'rewarded']
#       } else{
#         tar_rate = tar_rate + (.25^(nb-1))*data[j,'rewarded']
#       }
#     }
#     data[k,paste(n_back,'_back_rate',sep='')] = rate
#     data[k,paste(n_back,'_back_rate_tar',sep='')] = tar_rate
#   }
#   return(data)
# }


#====================== Create Data Frame ================
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

#====================== Add Normalization ================
# Add Normalization to Not Filtered data.
for (item in testable_vars_abs){
  test[,paste(item, '_norm',sep='')] = 0
  test[,paste(item, '_diff',sep='')] = 0
  item_norm = paste(item, '_norm',sep='')
  item_diff = paste(item, '_diff',sep='')
  
  for (subj_num in unique(test$subj)){
    
    for (tar in unique(test$target)){
      mean_0 = mean(test[test$r_prob==0 & test$subj == subj_num & test$target == tar,item])
      
      test[test$subj == subj_num & test$target == tar,item_norm] <- test[test$subj == subj_num & test$target == tar,item]/mean_0
      test[test$subj == subj_num & test$target == tar,item_diff] <- test[test$subj == subj_num & test$target == tar,item]-mean_0
    }
    
    # mean_0 = mean(test[test$r_prob==0 & test$subj == subj_num,item])
    # test[test$subj == subj_num,item_norm] <- test[test$subj == subj_num,item]/mean_0
    # test[test$subj == subj_num,item_diff] <- test[test$subj == subj_num,item]-mean_0
    
    # a = mean(test[test$r_prob==0 & test$subj == subj_num,item_norm])
    # b = test[test$subj == subj_num,item]/mean_0
  }
}

# Add Normalization to filtered data.
filt_data = c()
filt_data = filter(test,trial_in_block>36)

for (item in testable_vars_abs){
  filt_data[,paste(item, '_norm',sep='')] = 0
  filt_data[,paste(item, '_diff',sep='')] = 0
  item_norm = paste(item, '_norm',sep='')
  item_diff = paste(item, '_diff',sep='')
  
  for (subj_num in unique(filt_data$subj)){
    
    for (tar in unique(filt_data$target)){
      mean_0 = mean(filt_data[filt_data$r_prob==0 & filt_data$subj == subj_num & filt_data$target == tar,item])
      
      filt_data[filt_data$subj == subj_num & filt_data$target == tar,item_norm] <- filt_data[filt_data$subj == subj_num & filt_data$target == tar,item]/mean_0
      filt_data[filt_data$subj == subj_num & filt_data$target == tar,item_diff] <- filt_data[filt_data$subj == subj_num & filt_data$target == tar,item]-mean_0
    }
    
    # mean_0 = mean(filt_data[filt_data$r_prob==0 & filt_data$subj == subj_num,item])
    # filt_data[filt_data$subj == subj_num,item_norm] <- as.numeric(as.character(filt_data[filt_data$subj == subj_num,item]/mean_0))
    # filt_data[filt_data$subj == subj_num,item_diff] <- as.numeric(as.character(filt_data[filt_data$subj == subj_num,item]-mean_0))
    
    # if (as.numeric(as.character(mean(filt_data[filt_data$r_prob==0 & filt_data$subj == subj_num,item_norm]))) != 1){
    #   print(as.numeric(as.character(mean(filt_data[filt_data$r_prob==0 & filt_data$subj == subj_num,item_norm]))))
    #   print(item)
    #   print(subj_num)
    #   # break
    # }
  }
  if (as.numeric(as.character(mean(filt_data[filt_data$r_prob==0,item_norm]))) != 1){
    mean(filt_data[filt_data$r_prob==0,item_norm])
    print(item)
    # break
  }
}


```

## No Facet ABS

```{r}

#============= No Facet Abs
a1 = aggregate(peak_vel ~ r_prob + subj ,filt_data,mean)
a  = aggregate(peak_vel ~ r_prob ,a1,mean)
b  = aggregate(peak_vel ~ r_prob ,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel)
colnames(c) = c('r_prob','peak_vel','peak_vel_se')

ggplot(data=filt_data,
             aes(x = factor(r_prob),
                 y = peak_vel))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel-peak_vel_se,
                    ymax = peak_vel+peak_vel_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['abs']][['peak_vel']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['abs']][['peak_vel']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['abs']][['peak_vel']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')

```

## Facet ABS

```{r}

#============= Facet Abs
a1 = aggregate(peak_vel ~ r_prob + subj + target,filt_data,mean)
a  = aggregate(peak_vel ~ r_prob + target,a1,mean)
b  = aggregate(peak_vel ~ r_prob + target,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel)
colnames(c) = c('r_prob','target','peak_vel','peak_vel_se')

ggplot(data=filt_data,
       aes(x = factor(r_prob),
           y = peak_vel))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel-peak_vel_se,
                    ymax = peak_vel+peak_vel_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['abs']][['peak_vel']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['abs']][['peak_vel']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['abs']][['peak_vel']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')+
  facet_wrap(.~target)


```

## No Facet Norm

```{r}

#============= No Facet Norm
a1 = aggregate(peak_vel_norm ~ r_prob + subj ,filt_data,mean)
a  = aggregate(peak_vel_norm ~ r_prob ,a1,mean)
b  = aggregate(peak_vel_norm ~ r_prob ,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel_norm)
colnames(c) = c('r_prob','peak_vel_norm','peak_vel_norm_se')

ggplot(data=filt_data,
       aes(x = factor(r_prob),
           y = peak_vel_norm))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel_norm),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel_norm),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel_norm-peak_vel_norm_se,
                    ymax = peak_vel_norm+peak_vel_norm_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel_norm),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['norm']][['peak_vel_norm']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['norm']][['peak_vel_norm']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['norm']][['peak_vel_norm']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')

```

## Facet Norm

```{r}
#============= Facet Norm
a1 = aggregate(peak_vel_norm ~ r_prob + subj + target,filt_data,mean)
a  = aggregate(peak_vel_norm ~ r_prob + target,a1,mean)
b  = aggregate(peak_vel_norm ~ r_prob + target,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel_norm)
colnames(c) = c('r_prob','target','peak_vel_norm','peak_vel_norm_se')

ggplot(data=filt_data,
       aes(x = factor(r_prob),
           y = peak_vel_norm))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel_norm),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel_norm),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel_norm-peak_vel_norm_se,
                    ymax = peak_vel_norm+peak_vel_norm_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel_norm),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['norm']][['peak_vel_norm']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['norm']][['peak_vel_norm']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['norm']][['peak_vel_norm']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')+
  facet_wrap(.~target)


```

## No Facet Diff

```{r}

#============= No Facet Diff
a1 = aggregate(peak_vel_diff ~ r_prob + subj ,filt_data,mean)
a  = aggregate(peak_vel_diff ~ r_prob ,a1,mean)
b  = aggregate(peak_vel_diff ~ r_prob ,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel_diff)
colnames(c) = c('r_prob','peak_vel_diff','peak_vel_diff_se')

ggplot(data=filt_data,
       aes(x = factor(r_prob),
           y = peak_vel_diff))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel_diff),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel_diff),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel_diff-peak_vel_diff_se,
                    ymax = peak_vel_diff+peak_vel_diff_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel_diff),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['diff']][['peak_vel_diff']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['diff']][['peak_vel_diff']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['diff']][['peak_vel_diff']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')

```

## Facet Diff

```{r}

#============= Facet Diff
a1 = aggregate(peak_vel_diff ~ r_prob + subj + target,filt_data,mean)
a  = aggregate(peak_vel_diff ~ r_prob + target,a1,mean)
b  = aggregate(peak_vel_diff ~ r_prob + target,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
c  = cbind(a,b$peak_vel_diff)
colnames(c) = c('r_prob','target','peak_vel_diff','peak_vel_diff_se')

ggplot(data=filt_data,
       aes(x = factor(r_prob),
           y = peak_vel_diff))+
  geom_point(data=c,
             aes(x = factor(r_prob),
                 y = peak_vel_diff),
             size = 5)+
  geom_line(data=c,
            aes(x = factor(r_prob),
                y = peak_vel_diff),
            group = 1,
            size = 1)+
  geom_errorbar(data=c,
                aes(x = factor(r_prob),
                    ymin = peak_vel_diff-peak_vel_diff_se,
                    ymax = peak_vel_diff+peak_vel_diff_se),
                size = 1,
                width = 0)+
  geom_hline(yintercept = mean(filter(filt_data,r_prob == 0)$peak_vel_diff),linetype='dashed',size=.5)+
  labs(x = 'Probability of Reward',
       # y = testable_labels[var_select],
       title = paste('LMER P-Value: ',
                     formatC(lme_test[['r_prob']][['diff']][['peak_vel_diff']]$test$pvalues[5],format="e",digits = 3),
                     ', Slope: ',
                     formatC(lme_test[['r_prob']][['diff']][['peak_vel_diff']]$test$coefficients[5],format="e",digits = 3),
                     '\nOnly target 1/4: ',
                     round(lme_test_t14[['r_prob']][['diff']][['peak_vel_diff']]$test$pvalues[5],digits = 4),
                     sep=''))+
  theme(legend.position = 'none')+
  facet_wrap(.~target)






# #========
# 
# ggplot()+geom_violin(data = testing_data,
#                      aes(x = factor(r_prob),
#                          y = peak_vel_diff))+
#   facet_wrap(.~target)

```
