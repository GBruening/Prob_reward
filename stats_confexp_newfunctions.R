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

# ============ Add Prior Reward Diff ========
for (k in 1:length(non_famil_data$subj)){
  if (non_famil_data[k,'trial_in_block'] == 1)
    non_famil_data[k,'DiffPrior_Prob'] = 0
  else {
    if (as.numeric(non_famil_data[k,'probs']-non_famil_data[k-1,'probs']) == -0.67){
      asdfasdfasdfasdf
    }
    non_famil_data[k,'DiffPrior_Prob'] = round(as.numeric(non_famil_data[k,'probs']-non_famil_data[k-1,'probs']),2)
  }
}
non_famil_data[non_famil_data$DiffPrior_Prob == -0.67, 'DiffPrior_Prob'] = non_famil_data[non_famil_data$DiffPrior_Prob == -0.67, 'DiffPrior_Prob']+0.01
non_famil_data[non_famil_data$DiffPrior_Prob == -0.34, 'DiffPrior_Prob'] = non_famil_data[non_famil_data$DiffPrior_Prob == -0.34, 'DiffPrior_Prob']+0.01
non_famil_data[non_famil_data$DiffPrior_Prob == 0.67, 'DiffPrior_Prob'] = non_famil_data[non_famil_data$DiffPrior_Prob == 0.67, 'DiffPrior_Prob']-0.01
non_famil_data[non_famil_data$DiffPrior_Prob == 0.34, 'DiffPrior_Prob'] = non_famil_data[non_famil_data$DiffPrior_Prob == 0.34, 'DiffPrior_Prob']-0.01

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
non_famil_data$one_back_rate <- non_famil_data$one_back_rate*2

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
              'NRWD_Run',
              'DiffCur_Prob')

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
               't_since_reward',
               'diff_prob')

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

add_norm_diff_func <- function(data, item){
  data[,paste('Norm',item,sep='')] = 0
  data[,paste('Delta',item,sep='')] = 0
  item_norm = paste('Norm',item,sep='')
  item_diff = paste('Delta',item,sep='')
  
  for (Subject_num in unique(data$Subject)){
    for (tar in unique(data$Target)){
      mean_0 = mean(data[data$r_prob==0 & data$Subject == Subject_num & data$Target == tar,item])
      
      data[data$Subject == Subject_num & data$Target == tar,item_norm] <- data[data$Subject == Subject_num & data$Target == tar,item]/mean_0
      data[data$Subject == Subject_num & data$Target == tar,item_diff] <- data[data$Subject == Subject_num & data$Target == tar,item]-mean_0
    }
  }
  return(data)
}

for (item in testable_vars_abs){
  non_famil_data <- add_norm_diff_func(non_famil_data,item)
  filt_data <- non_famil_data
}

# Init some lists
ano_res <- list()
ano_res_t14 <- list()
lme_test <- list()
lme_test_t14 <- list()

#====================== Loopin ================
setwd(paste('Graphs/',sep = ''))

Prob_plotting <- function(data,
                          prob_method = 'Probability',
                          # norm_meth = 'abs',
                          plot_var,
                          var_lab,
                          plot_type = 'nv',
                          Prior_RWD = 'Prior_RWD'){

  
  if (Prior_RWD == 'Prior_RWD'){
    
    plotting_data = data[,c('Subject','Trial','Trial_in_Block','Target','Target_num','Block','RWD','Prior_RWD','NRWD_Run','Probability',prob_method,plot_var)]
    colnames(plotting_data) = c('Subject','Trial','Trial_in_Block','Target','Target_num','Block','RWD','Prior_RWD','NRWD_Run','Probability','probability_metric', 'plot_var')
    
    lme_test = list()
    lme_test <- cftest(lmer(plot_var ~ probability_metric*Prior_RWD + (1|Subject:Target), data = plotting_data))
    lme_test_t14 = list()
    lme_test_t14 <- cftest(lmer(plot_var ~ probability_metric*Prior_RWD + (1|Subject:Target), 
                                data = rbind(filter(plotting_data,probability_metric==1),
                                             filter(plotting_data,probability_metric==0))))
    
    #============= No Facet Abs
    a1 = aggregate(plot_var ~ probability_metric + Prior_RWD + Subject ,plotting_data,mean)
    a  = aggregate(plot_var ~ probability_metric + Prior_RWD ,a1,mean)
    b  = aggregate(plot_var ~ probability_metric + Prior_RWD ,a1,sd)/sqrt(max(a1$Subject))#/sqrt(length(plotting_data[,1]))
    c  = cbind(a,b$plot_var)
    colnames(c) = c('probability_metric','Prior_RWD','plot_var','plot_var_se')
    
    g<-ggplot(data = c,
              aes(x = factor(probability_metric),
                  y = plot_var,
                  color = factor(Prior_RWD),
                  group = factor(Prior_RWD)))+
      geom_point(data = c,
                 aes(x = factor(probability_metric),
                     y = plot_var,
                     color = factor(Prior_RWD),
                     group = factor(Prior_RWD)),
                 size = 5,
                 position = position_dodge(width = .5))+
      geom_line(data=c,
                aes(x = factor(probability_metric),
                    y = plot_var,
                    color = factor(Prior_RWD),
                    group = factor(Prior_RWD)),
                size = 1,
                position = position_dodge(width = .5))+
      geom_errorbar(data=c,
                    aes(x = factor(probability_metric),
                        ymin = plot_var-plot_var_se,
                        ymax = plot_var+plot_var_se,
                        color = factor(Prior_RWD)),
                    size = 1,
                    width = 1,
                    position = position_dodge(width = .5))+
      # geom_hline(yintercept = mean(filter(plotting_data,probability_metric == 0)$plot_var),linetype='dashed',size=.5)+
      labs(x = 'Probability of Reward',
           y = var_lab,
           title = paste('LMER P-Values. Prob Metric: ',
                         formatC(lme_test$test$pvalues[2],format="e",digits = 3),
                         ',\nPrior Rwd: ',
                         formatC(lme_test$test$pvalues[3],format="e",digits = 3),
                         ', Interaction: ',
                         formatC(lme_test$test$pvalues[4],format="e",digits = 3),
                         # , Slope: ',
                         # formatC(lme_test$test$coefficients[2],format="e",digits = 3),
                         # '\nOnly target 1/4: ',
                         # round(lme_test_t14$test$pvalues[2],digits = 4),
                         sep=''),
           color = 'Prior Reward')#+theme(legend.position = 'none')
  } else {
    plotting_data = data[,c('Subject','Trial','Trial_in_Block','Target','Target_num','Block','RWD','NRWD_Run','Probability',prob_method,plot_var)]
    colnames(plotting_data) = c('Subject','Trial','Trial_in_Block','Target','Target_num','Block','RWD','NRWD_Run','Probability','probability_metric', 'plot_var')
    
    ano_res2 = anova(lm(plot_var ~ Target + probability_metric + Target*probability_metric + (1|Subject), data = plotting_data))
    
    lme_test = cftest(lmer(plot_var ~ probability_metric + (1|Subject:Target), data = plotting_data))
    lme_test_t14 = cftest(lmer(plot_var ~ probability_metric + (1|Subject:Target),
                                data = rbind(filter(plotting_data,Probability==1),
                                             filter(plotting_data,Probability==0))))
    
    plotting_data2 = aggregate(plot_var ~ Subject + Target + Probability,plotting_data,mean)
    ano_res = anova(lm(plot_var ~ Probability + (1|Subject),
                                                                  data = plotting_data2))
    ano_res_t14 = anova(lm(plot_var ~ Probability + (1|Subject),
                            data = rbind(filter(plotting_data2,Probability==1),
                                         filter(plotting_data2,Probability==0))))
    
    aov_p_vals = c(aov_p_vals,ano_res$`Pr(>F)`[1])
    aov_p_vals_t14 = c(aov_p_vals,ano_res_t14$`Pr(>F)`[1])
    lme_p_vals = c(lme_p_vals,lme_test$test$pvalues[2])
    lme_p_vals_t14 = c(lme_p_vals_t14,lme_test_t14$test$pvalues[2])
    
    if (plot_type == 'violin'){
      #====================== Violin Plot ================
      # Violin Plots
      a = aggregate(plot_var ~ probability_metric ,plotting_data,mean)
      b = aggregate(plot_var ~ probability_metric ,plotting_data,sd)#/sqrt(length(plotting_data[,1]))
      c = cbind(a,b$plot_var)
      colnames(c) = c('probability_metric','plot_var','plot_var_sd')
      
      g <- ggplot(data=plotting_data,
                  aes(x = factor(probability_metric),
                      y = plot_var))+
        geom_violin(aes(fill = factor(probability_metric)))+
        geom_point(data=aggregate(plot_var~probability_metric,plotting_data,mean),
                   aes(x = factor(probability_metric),
                       y = plot_var),
                   size = 5)+
        geom_line(data=aggregate(plot_var~probability_metric,plotting_data,mean),
                  aes(x = factor(probability_metric),
                      y = plot_var),
                  group = 1,
                  size = 1)+
        geom_errorbar(data=c,
                      aes(x = factor(probability_metric),
                          ymin = plot_var-plot_var_sd,
                          ymax = plot_var+plot_var_sd),
                      size = 1,
                      width = 0)+
        geom_hline(yintercept = mean(filter(plotting_data,probability_metric == 0)$plot_var),linetype='dashed',size=.5)+
        labs(x = 'Probability of Reward',
             y = var_lab,
             title = paste('LMER P-Value: ',
                           formatC(lme_test$test$pvalues[2],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test$test$coefficients[2],format="e",digits = 3),
                           '\nOnly Target 1/4: ',
                           round(lme_test_t14$test$pvalues[2],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
    } else if (plot_type == 'nv') {
      
      # Non-Violin Plot
      a1 = aggregate(plot_var ~ probability_metric + Subject,plotting_data,mean)
      a  = aggregate(plot_var ~ probability_metric ,a1,mean)
      b  = aggregate(plot_var ~ probability_metric ,a1,sd)/sqrt(max(a1$Subject))#/sqrt(length(plotting_data[,1]))
      c  = cbind(a,b$plot_var)
      colnames(c) = c('probability_metric','plot_var','plot_var_se')
      
      g <- ggplot(data=plotting_data,
                   aes(x = factor(probability_metric),
                       y = plot_var))+
        geom_point(data=aggregate(plot_var~probability_metric,plotting_data,mean),
                   aes(x = factor(probability_metric),
                       y = plot_var),
                   size = 5)+
        geom_line(data=aggregate(plot_var~probability_metric,plotting_data,mean),
                  aes(x = factor(probability_metric),
                      y = plot_var),
                  group = 1,
                  size = 1)+
        geom_errorbar(data=c,
                      aes(x = factor(probability_metric),
                          ymin = plot_var-plot_var_se,
                          ymax = plot_var+plot_var_se),
                      size = 1,
                      width = 0)+
        geom_hline(yintercept = mean(filter(plotting_data,probability_metric == 0)$plot_var),linetype='dashed',size=.5)+
        labs(x = 'Probability of Reward',
             y = var_lab,
             title = paste('LMER P-Value: ',
                           formatC(lme_test$test$pvalues[2],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test$test$coefficients[2],format="e",digits = 3),
                           '\nOnly Target 1/4: ',
                           round(lme_test_t14$test$pvalues[2],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
    }
  }
  if (prob_method == 'diff_prob'){
    g <- g+labs(x = 'Probability difference (P(t)-P(t-1))')
  }
  return(g)
}


test = Prob_plotting(data = filt_data, prob_method = 'DiffPrior_Prob', plot_var = 'PeakV', var_lab = 'Peak vellllll', plot_type = 'violin', Prior_RWD = 'Prior_RWD')







