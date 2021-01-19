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
raw_data = read.csv(paste('vigor_conf_',exp_name,'.csv',sep=''))

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
        # if (data[j,'subj'] != subject | data[j,'block'] != block){
        #   break
        # } else {
        nb = nb + 1
        rate = rate + (.5^(nb))*data[data$subj == subject & data$block == block & data$trial_in_block == j,'rewarded']
        
        if (data[j,'target'] == cur_tar){
          tar_rate = tar_rate + (.5^(nb))*data[j,'rewarded']
        } else{
          tar_rate = tar_rate + (.25^(nb))*data[j,'rewarded']
        }
        # }
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
non_famil_dataable_vars_abs = c('peak_vel',
                                'peak_vel_moveback',
                                'move_dur',
                                'react_time',
                                'error_dist',
                                'react_vel',
                                'error_angle',
                                'maxex')

non_famil_dataable_labels_abs = c('Peak Velocity (m/s)',
                                  'Peak Velocity Moveback (m/s)',
                                  'Movement Duration (s)',
                                  'Reaction time (s)',
                                  'Error Distance (m)',
                                  'Reaction Velocity (m/s)',
                                  'Error Angle (deg)',
                                  'Maximum Excursion (m)')

non_famil_dataable_vars_norm = c('peak_vel_norm',
                                 'peak_vel_moveback_norm',
                                 'move_dur_norm',
                                 'react_time_norm',
                                 'error_dist_norm',
                                 'react_vel_norm',
                                 'error_angle_norm',
                                 'maxex_norm')

non_famil_dataable_labels_norm = c('Peak Velocity Norm (m/s)',
                                   'Peak Velocity Moveback Norm (m/s)',
                                   'Movement Duration Norm (s)',
                                   'Reaction time Norm (s)',
                                   'Error Distance Norm (m)',
                                   'Reaction Velocity Norm (m/s)',
                                   'Error Angle Norm (deg)',
                                   'Maximum Excursion Norm (m)')

non_famil_dataable_vars_diff = c('peak_vel_diff',
                                 'peak_vel_moveback_diff',
                                 'move_dur_diff',
                                 'react_time_diff',
                                 'error_dist_diff',
                                 'react_vel_diff',
                                 'error_angle_diff',
                                 'maxex_diff')

non_famil_dataable_labels_diff = c('Peak Velocity Diff (m/s)',
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
for (item in non_famil_dataable_vars_abs){
  non_famil_data[,paste(item, '_norm',sep='')] = 0
  non_famil_data[,paste(item, '_diff',sep='')] = 0
  item_norm = paste(item, '_norm',sep='')
  item_diff = paste(item, '_diff',sep='')
  
  for (subj_num in unique(non_famil_data$subj)){
    
    for (tar in unique(non_famil_data$target)){
      mean_0 = mean(non_famil_data[non_famil_data$r_prob==0 & non_famil_data$subj == subj_num & non_famil_data$target == tar,item])
      
      non_famil_data[non_famil_data$subj == subj_num & non_famil_data$target == tar,item_norm] <- non_famil_data[non_famil_data$subj == subj_num & non_famil_data$target == tar,item]/mean_0
      non_famil_data[non_famil_data$subj == subj_num & non_famil_data$target == tar,item_diff] <- non_famil_data[non_famil_data$subj == subj_num & non_famil_data$target == tar,item]-mean_0
    }
    
    # mean_0 = mean(non_famil_data[non_famil_data$r_prob==0 & non_famil_data$subj == subj_num,item])
    # non_famil_data[non_famil_data$subj == subj_num,item_norm] <- non_famil_data[non_famil_data$subj == subj_num,item]/mean_0
    # non_famil_data[non_famil_data$subj == subj_num,item_diff] <- non_famil_data[non_famil_data$subj == subj_num,item]-mean_0
    
    # a = mean(non_famil_data[non_famil_data$r_prob==0 & non_famil_data$subj == subj_num,item_norm])
    # b = non_famil_data[non_famil_data$subj == subj_num,item]/mean_0
  }
}

# Add Normalization to filtered data.
filt_data = c()
# filt_data = filter(non_famil_data,trial_in_block>36)
filt_data = non_famil_data

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

# Init some lists
ano_res <- list()
ano_res_t14 <- list()
lme_test <- list()
lme_test_t14 <- list()


#====================== Loopin ================
setwd(paste('Graphs/',sep = ''))

for (prob_method in c('r_prob','diff_prob')){
  setwd(paste(prob_method,sep = ''))
  for (norm_meth in c('abs','diff','norm')){
    setwd(norm_meth)

    testable_vars = eval(parse(text = paste('testable_vars_',norm_meth,sep='')))
    testable_labels = eval(parse(text = paste('testable_labels_',norm_meth,sep='')))
    var_select = 0
    
    aov_p_vals = c()
    aov_p_vals_t14 = c()
    lme_p_vals = c()
    lme_p_vals_t14 = c()

    for (testing_var in testable_vars){
      #====================== Stat Tests ================
      var_select = var_select + 1
      testing_data = filt_data[,c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob',prob_method,testing_var)]
      colnames(testing_data) = c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob','probability_metric', 'testing_var')

      ano_res2 = anova(lm(testing_var ~ target + probability_metric + target*probability_metric + (1|subj), data = testing_data))

      # ano_res[[testing_var]] = anova(lm(testing_var ~ probability_metric + (1|subj), data = testing_data))
      # ano_res_t14[[testing_var]] = anova(lm(testing_var ~ probability_metric + (1|subj),
      #                        data = rbind(filter(testing_data,probability_metric==1),filter(testing_data,probability_metric==0))))
      lme_test[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ probability_metric + (1|subj:target), data = testing_data))
      lme_test_t14[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ probability_metric + (1|subj:target),
                                                data = rbind(filter(testing_data,r_prob==1),
                                                             filter(testing_data,r_prob==0))))

      testing_data2 = aggregate(testing_var ~ subj + target + r_prob,testing_data,mean)
      ano_res[[prob_method]][[norm_meth]][[testing_var]] = anova(lm(testing_var ~ r_prob + (1|subj),
                                        data = testing_data2))
      ano_res_t14[[prob_method]][[norm_meth]][[testing_var]] = anova(lm(testing_var ~ r_prob + (1|subj),
                                            data = rbind(filter(testing_data2,r_prob==1),
                                                         filter(testing_data2,r_prob==0))))
      
      aov_p_vals = c(aov_p_vals,ano_res[[prob_method]][[norm_meth]][[testing_var]]$`Pr(>F)`[1])
      aov_p_vals_t14 = c(aov_p_vals,ano_res_t14[[prob_method]][[norm_meth]][[testing_var]]$`Pr(>F)`[1])
      lme_p_vals = c(lme_p_vals,lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2])
      lme_p_vals_t14 = c(lme_p_vals_t14,lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2])

      
      #====================== Violin Plot ================
      # Violin Plots
      a = aggregate(testing_var ~ probability_metric ,testing_data,mean)
      b = aggregate(testing_var ~ probability_metric ,testing_data,sd)#/sqrt(length(testing_data[,1]))
      c = cbind(a,b$testing_var)
      colnames(c) = c('probability_metric','testing_var','testing_var_sd')

      g <- ggplot(data=testing_data,
                  aes(x = factor(probability_metric),
                      y = testing_var))+
        geom_violin(aes(fill = factor(probability_metric)))+
        geom_point(data=aggregate(testing_var~probability_metric,testing_data,mean),
                   aes(x = factor(probability_metric),
                       y = testing_var),
                   size = 5)+
        geom_line(data=aggregate(testing_var~probability_metric,testing_data,mean),
                  aes(x = factor(probability_metric),
                       y = testing_var),
                  group = 1,
                  size = 1)+
        geom_errorbar(data=c,
                      aes(x = factor(probability_metric),
                          ymin = testing_var-testing_var_sd,
                          ymax = testing_var+testing_var_sd),
                      size = 1,
                      width = 0)+
        geom_hline(yintercept = mean(filter(testing_data,probability_metric == 0)$testing_var),linetype='dashed',size=.5)+
        labs(x = 'Probability of Reward',
             y = testable_labels[var_select],
             title = paste('LMER P-Value: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[2],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
      # facet_grid(cols = vars(subj))
      
      #====================== NV Plot ================
      # Non-Violin Plot
      a1 = aggregate(testing_var ~ probability_metric + subj,testing_data,mean)
      a  = aggregate(testing_var ~ probability_metric ,a1,mean)
      b  = aggregate(testing_var ~ probability_metric ,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
      c  = cbind(a,b$testing_var)
      colnames(c) = c('probability_metric','testing_var','testing_var_se')
      
      g2 <- ggplot(data=testing_data,
                  aes(x = factor(probability_metric),
                      y = testing_var))+
        geom_point(data=aggregate(testing_var~probability_metric,testing_data,mean),
                   aes(x = factor(probability_metric),
                       y = testing_var),
                   size = 5)+
        geom_line(data=aggregate(testing_var~probability_metric,testing_data,mean),
                  aes(x = factor(probability_metric),
                      y = testing_var),
                  group = 1,
                  size = 1)+
        geom_errorbar(data=c,
                      aes(x = factor(probability_metric),
                          ymin = testing_var-testing_var_se,
                          ymax = testing_var+testing_var_se),
                      size = 1,
                      width = 0)+
        geom_hline(yintercept = mean(filter(testing_data,probability_metric == 0)$testing_var),linetype='dashed',size=.5)+
        labs(x = 'Probability of Reward',
             y = testable_labels[var_select],
             title = paste('LMER P-Value: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[2],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')

      # 
      #====================== Bar plots diff prob ================
      if (norm_meth == 'diff'){
        a = aggregate(testing_var ~ probability_metric, filter(testing_data,r_prob != 0), mean)
        b = aggregate(testing_var ~ probability_metric, filter(testing_data,r_prob != 0), sd)/sqrt(max(testing_data$subj))#/sqrt(length(filter(testing_data,r_prob != 0)))
        c = cbind(a,b$testing_var)
        colnames(c) = c('probability_metric','testing_var','testing_var_sd')

        g <- ggplot(data=testing_data,
                    aes(x = factor(probability_metric),
                        y = testing_var))+
          geom_bar(data = c,position='dodge',stat='identity')+
          geom_errorbar(data=c,
                        aes(x = factor(probability_metric),
                            ymin = testing_var-testing_var_sd,
                            ymax = testing_var+testing_var_sd),
                        size = 1,
                        width = 0)+
          geom_hline(yintercept = 0,
                     linetype='dashed',size=.5)+
          labs(x = 'Probability of Reward',
               y = testable_labels[var_select],
               title = paste('LMER P-Value: ',
                             formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],format="e",digits = 3),
                             ', Slope: ',
                             formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[2],format="e",digits = 3),
                             '\nOnly target 1/4: ',
                             round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],digits = 4),
                             sep=''))+
          theme(legend.position = 'none')
      }
      
      #====================== Save the Violin/Non Violin Plots ================
      # plot_name = paste(testable_vars[var_select],'_4t_180trial_4block',sep='')
      plot_name = testable_vars[var_select]
      eval(parse(text = paste(plot_name,'<-g',sep='')))
      eval(parse(text = paste(plot_name,'_nv<-g2',sep='')))
      
      ggsave(paste(plot_name,'.pdf',sep=''),
             plot = g,
             width=6,
             height=4,
             useDingbats = FALSE)
      ggsave(paste(plot_name,'_nv.pdf',sep=''),
             plot = g2,
             width=6,
             height=4,
             useDingbats = FALSE)

      a = aggregate(testing_var ~ probability_metric ,testing_data,mean)
      b = aggregate(testing_var ~ probability_metric ,testing_data,sd)
      c = cbind(a,b$testing_var)
      colnames(c) = c('probability_metric','testing_var','testing_var_sd')

    }
    
    #====================== Pval Bar Plots ================
    p_vals = data.frame('var' = testable_vars, 'var_num' = c(1:length(testable_vars)), 'p_vals' = lme_p_vals)
    pval_plot <- ggplot()+
      geom_hline(yintercept =  0.05,
                 color = 'red',
                 size = 2)+
      geom_bar(data=p_vals,
               aes(x=var_num,
                   y=p_vals),
               stat = 'identity')+
      scale_x_discrete(limits = c(1:length(testable_labels)), labels = testable_labels)+
      labs(x = 'Tested Variable',
           y = 'LMER P-Value')+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      scale_y_continuous(expand = c(0,0))

    p_vals_14 = data.frame('var' = testable_vars, 'var_num' = c(1:length(testable_vars)), 'p_vals' = lme_p_vals_t14)
    pval_plot_t14 <- ggplot()+
      geom_hline(yintercept =  0.05,
                 color = 'red',
                 size = 2)+
      geom_bar(data=p_vals,
               aes(x=var_num,
                   y=p_vals),
               stat = 'identity')+
      scale_x_discrete(limits = c(1:length(testable_labels)), labels = testable_labels)+
      labs(x = 'Tested Variable',
           y = 'LMER P-Value, Only Target 1 and 4')+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      scale_y_continuous(expand = c(0,0))

    pval_plot_grid <- plot_grid(pval_plot,pval_plot_t14,
                                nrow=2,
                                align = 'vh')
    
    ggsave(paste('pval_plot_',prob_method,'_',norm_meth,'.pdf',sep = ''),
           plot = pval_plot_grid,
           width=6,
           height=8,
           useDingbats = FALSE)
    setwd('..')
  }
  setwd('..')
}

#====================== Surprise Factor ================
# Surprise factor thing
# setwd(paste('Graphs/',sep = ''))
for (prob_method in c('diff_prob')){
  setwd(paste(prob_method,sep = ''))
  for (norm_meth in c('diff','abs','norm')){
    setwd('surprise')
    setwd(norm_meth)
    
    testable_vars = eval(parse(text = paste('testable_vars_',norm_meth,sep='')))
    testable_labels = eval(parse(text = paste('testable_labels_',norm_meth,sep='')))
    var_select = 0
    
    ano_res_surprise <- list()
    ano_res_t14_surprise <- list()
    lme_test_surprise <- list()
    lme_test_t14_surprise <- list()
    
    aov_p_vals_surprise = c()
    aov_p_vals_t14_surprise = c()
    lme_p_vals_surprise = c()
    lme_p_vals_t14_surprise = c()
    
    for (testing_var in testable_vars){
      
      #====================== Stats ================
      var_select = var_select + 1
      testing_data = filt_data[,c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob',prob_method,testing_var)]
      colnames(testing_data) = c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob','probability_metric', 'testing_var')
      
      testing_data$probability_metric = abs(testing_data$probability_metric)
      
      ano_res2_surprise = anova(lm(testing_var ~ target + probability_metric + target*probability_metric + (1|subj), data = testing_data))
      
      lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ factor(target) + probability_metric + (1|subj), data = testing_data))
      lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ factor(target) + probability_metric + (1|subj),
                                                                            data = rbind(filter(testing_data,r_prob==1),
                                                                                         filter(testing_data,r_prob==0))))
      
      testing_data2_surprise = aggregate(testing_var ~ subj + target + r_prob,testing_data,mean)
      ano_res_surprise[[prob_method]][[norm_meth]][[testing_var]] = anova(lm(testing_var ~ r_prob + (1|subj), 
                                                                    data = testing_data2))
      ano_res_t14_surprise[[prob_method]][[norm_meth]][[testing_var]] = anova(lm(testing_var ~ r_prob + (1|subj), 
                                                                        data = rbind(filter(testing_data2,r_prob==1), 
                                                                                     filter(testing_data2,r_prob==0))))
      
      aov_p_vals_surprise = c(aov_p_vals_surprise,ano_res_surprise[[prob_method]][[norm_meth]][[testing_var]]$`Pr(>F)`[1])
      aov_p_vals_t14_surprise = c(aov_p_vals_surprise,ano_res_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$`Pr(>F)`[1])
      lme_p_vals_surprise = c(lme_p_vals_surprise,lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2])
      lme_p_vals_t14_surprise = c(lme_p_vals_t14_surprise,lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2])
      
      #====================== Violin Plot ================
      # Violin Plots
      a = aggregate(testing_var ~ probability_metric ,testing_data,mean)
      b = aggregate(testing_var ~ probability_metric ,testing_data,sd)#/sqrt(length(testing_data[,1]))
      c = cbind(a,b$testing_var)
      colnames(c) = c('probability_metric','testing_var','testing_var_sd')
      
      g <- ggplot(data=testing_data,
                  aes(x = factor(probability_metric),
                      y = testing_var))+
        geom_violin(aes(fill = factor(probability_metric)))+
        geom_point(data=aggregate(testing_var~probability_metric,testing_data,mean),
                   aes(x = factor(probability_metric),
                       y = testing_var),
                   size = 5)+
        geom_line(data=aggregate(testing_var~probability_metric,testing_data,mean),
                  aes(x = factor(probability_metric),
                      y = testing_var),
                  group = 1,
                  size = 1)+
        geom_errorbar(data=c,
                      aes(x = factor(probability_metric),
                          ymin = testing_var-testing_var_sd,
                          ymax = testing_var+testing_var_sd),
                      size = 1,
                      width = 0)+
        geom_hline(yintercept = mean(filter(testing_data,probability_metric == 0)$testing_var),linetype='dashed',size=.5)+
        labs(x = 'Probability of Reward',
             y = testable_labels[var_select],
             title = paste('LMER P-Value: ',
                           formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[2],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
      # facet_grid(cols = vars(subj))
      
      #====================== Bar Plots ================
      # Bar plots for diff probability
      if (norm_meth == 'diff'){
        a = aggregate(testing_var ~ probability_metric, filter(testing_data,r_prob != 0), mean)
        b = aggregate(testing_var ~ probability_metric, filter(testing_data,r_prob != 0), sd)#/sqrt(length(filter(testing_data,r_prob != 0)))
        c = cbind(a,b$testing_var)
        colnames(c) = c('probability_metric','testing_var','testing_var_sd')
        
        g <- ggplot(data=testing_data,
                    aes(x = factor(probability_metric),
                        y = testing_var))+
          geom_bar(data = c,position='dodge',stat='identity')+
          geom_errorbar(data=c,
                        aes(x = factor(probability_metric),
                            ymin = testing_var-testing_var_sd,
                            ymax = testing_var+testing_var_sd),
                        size = 1,
                        width = 0)+
          geom_hline(yintercept = 0,
                     linetype='dashed',size=.5)+
          labs(x = 'Probability of Reward',
               y = testable_labels[var_select],
               title = paste('LMER P-Value: ',
                             formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],format="e",digits = 3),
                             ', Slope: ',
                             formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[2],format="e",digits = 3),
                             '\nOnly target 1/4: ',
                             round(lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[2],digits = 4),
                             sep=''))+
          theme(legend.position = 'none')
      }
      
      plot_name = testable_vars[var_select]
      eval(parse(text = paste(plot_name,'<-g',sep='')))
      
      ggsave(paste(plot_name,'.pdf',sep=''), 
             plot = g, 
             width=6, 
             height=4, 
             useDingbats = FALSE)
      
      a = aggregate(testing_var ~ probability_metric ,testing_data,mean)
      b = aggregate(testing_var ~ probability_metric ,testing_data,sd)
      c = cbind(a,b$testing_var)
      colnames(c) = c('probability_metric','testing_var','testing_var_sd')
      
    }
    
    #====================== Pval Plots ================
    p_vals = data.frame('var' = testable_vars, 'var_num' = c(1:length(testable_vars)), 'p_vals' = lme_p_vals_surprise)
    pval_plot <- ggplot()+
      geom_hline(yintercept =  0.05,
                 color = 'red',
                 size = 2)+
      geom_bar(data=p_vals,
               aes(x=var_num,
                   y=p_vals),
               stat = 'identity')+
      scale_x_discrete(limits = c(1:length(testable_labels)), labels = testable_labels)+
      labs(x = 'Tested Variable',
           y = 'LMER P-Value')+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+ 
      scale_y_continuous(expand = c(0,0))
    
    p_vals_14 = data.frame('var' = testable_vars, 'var_num' = c(1:length(testable_vars)), 'p_vals' = lme_p_vals_t14_surprise)
    pval_plot_t14 <- ggplot()+
      geom_hline(yintercept =  0.05,
                 color = 'red',
                 size = 2)+
      geom_bar(data=p_vals,
               aes(x=var_num,
                   y=p_vals),
               stat = 'identity')+
      scale_x_discrete(limits = c(1:length(testable_labels)), labels = testable_labels)+
      labs(x = 'Tested Variable',
           y = 'LMER P-Value, Only Target 1 and 4')+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+ 
      scale_y_continuous(expand = c(0,0))
    
    pval_plot_grid <- plot_grid(pval_plot,pval_plot_t14,
                                nrow=2,
                                align = 'vh')
    
    ggsave(paste('pval_plot_',prob_method,'_',norm_meth,'.pdf',sep = ''), 
           plot = pval_plot_grid, 
           width=6, 
           height=8, 
           useDingbats = FALSE)
    setwd('..')
    setwd('..')
  }
  setwd('..')
}

