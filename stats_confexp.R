# cd('F:\Google Drive\Yan Experiment')
# setwd('F:\Google Drive\Yan Experiment')

# Does 33% change anything
# is the effect the same as erik's?
# Test between 0, 33, and 66

# LME for 0,33,66 and 33,66,1
# Probability matching, bimodal distributions?

# Add beta distrubtion update


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
      var_select = var_select + 1
      testing_data = filt_data[,c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob',prob_method,testing_var)]
      colnames(testing_data) = c('subj','trial','trial_in_block','target','target_num','block','rewarded','t_since_reward','r_prob','probability_metric', 'testing_var')

      ano_res2 = anova(lm(testing_var ~ target + probability_metric + target*probability_metric + (1|subj), data = testing_data))

      # ano_res[[testing_var]] = anova(lm(testing_var ~ probability_metric + (1|subj), data = testing_data))
      # ano_res_t14[[testing_var]] = anova(lm(testing_var ~ probability_metric + (1|subj),
      #                        data = rbind(filter(testing_data,probability_metric==1),filter(testing_data,probability_metric==0))))
      lme_test[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ factor(target) + probability_metric + (1|subj), data = testing_data))
      lme_test_t14[[prob_method]][[norm_meth]][[testing_var]] = cftest(lmer(testing_var ~ factor(target) + probability_metric + (1|subj),
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
      lme_p_vals = c(lme_p_vals,lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5])
      lme_p_vals_t14 = c(lme_p_vals_t14,lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5])


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
             # title = paste('rmANOVA P-Value: ',
             #               round(ano_res$`Pr(>F)`[1],digits = 4),
             #               '\nOnly target 1/4: ',
             #               round(ano_res_t14$`Pr(>F)`[1],digits = 4),
             #               sep=''))+
             title = paste('LMER P-Value: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[5],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
      # facet_grid(cols = vars(subj))
      
      # Non-Violin Plot
      a1 = aggregate(testing_var ~ probability_metric + subj,testing_data,mean)
      a = aggregate(testing_var ~ probability_metric ,a1,mean)
      b = aggregate(testing_var ~ probability_metric ,a1,sd)/sqrt(max(a1$subj))#/sqrt(length(testing_data[,1]))
      c = cbind(a,b$testing_var)
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
             # title = paste('rmANOVA P-Value: ',
             #               round(ano_res$`Pr(>F)`[1],digits = 4),
             #               '\nOnly target 1/4: ',
             #               round(ano_res_t14$`Pr(>F)`[1],digits = 4),
             #               sep=''))+
             title = paste('LMER P-Value: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[5],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')

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
                             formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],format="e",digits = 3),
                             ', Slope: ',
                             formatC(lme_test[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[5],format="e",digits = 3),
                             '\nOnly target 1/4: ',
                             round(lme_test_t14[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],digits = 4),
                             sep=''))+
          theme(legend.position = 'none')
      }

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
      # legend.position = c(.15,.8),
      # legend.background = element_blank(),
      # legend.title = element_text(size=10),
      # legend.text = element_text(size=7),
      # legend.key.size = unit(0.3, "cm"))+#,axis.line = element_line(color='black',size = 1,linetype='solid'))+
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
      # legend.position = c(.15,.8),
      # legend.background = element_blank(),
      # legend.title = element_text(size=10),
      # legend.text = element_text(size=7),
      # legend.key.size = unit(0.3, "cm"))+#,axis.line = element_line(color='black',size = 1,linetype='solid'))+
      labs(x = 'Tested Variable',
           y = 'LMER P-Value, Only Target 1 and 4')+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      scale_y_continuous(expand = c(0,0))

    pval_plot_grid <- plot_grid(pval_plot,pval_plot_t14,
                                nrow=2,
                                align = 'vh')

    # ggsave(paste('pval_plot_',prob_method,'_4t_180trial_4block.pdf',sep = ''),
    #        plot = pval_plot_grid,
    #        width=6,
    #        height=8,
    #        useDingbats = FALSE)
    ggsave(paste('pval_plot_',prob_method,'_',norm_meth,'.pdf',sep = ''),
           plot = pval_plot_grid,
           width=6,
           height=8,
           useDingbats = FALSE)
    setwd('..')
  }
  setwd('..')
}
asfdasdfasfd
# Surprise factor thing
setwd(paste('Graphs/',sep = ''))
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
      lme_p_vals_surprise = c(lme_p_vals_surprise,lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5])
      lme_p_vals_t14_surprise = c(lme_p_vals_t14_surprise,lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5])
      
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
                           formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],format="e",digits = 3),
                           ', Slope: ',
                           formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[5],format="e",digits = 3),
                           '\nOnly target 1/4: ',
                           round(lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],digits = 4),
                           sep=''))+
        theme(legend.position = 'none')
      # facet_grid(cols = vars(subj))
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
                             formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],format="e",digits = 3),
                             ', Slope: ',
                             formatC(lme_test_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$coefficients[5],format="e",digits = 3),
                             '\nOnly target 1/4: ',
                             round(lme_test_t14_surprise[[prob_method]][[norm_meth]][[testing_var]]$test$pvalues[5],digits = 4),
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

# compare .67 to -.33
# DO_NOVA_N_ARIABILITY_TUFF
# Break up the 0 surprise factor into rewarded and non rewarded

# anova(lm(peakvel ~ r_prob + (1|subj) + (1|subj), data = test))
# anova(lm(peakvel ~ r_prob + (1|subj) + (1|target), data = test))
# anova(lm(peakvel ~ r_prob + (1|subj*target), data = test))
# anova(lm(peakvel ~ r_prob + (1|subj*target), data = filter(test,trial>20)))
# 
# cftest(lmer(peakvel ~ r_prob + (1|subj*target), data = fitler(test,trial>20)))
# cftest(lmer(peakvel ~ r_prob + (1|subj*target), data = filter(test,trial>20)))
# cftest(lmer(peakvel ~ r_prob + (1|subj)+(1|target), data = filter(test,trial>20)))
# cftest(lmer(movedur ~ r_prob + (1|subj)+(1|target), data = filter(test,trial>20)))
# 
# cftest(lmer(movedur ~ react_time + (1|subj)+(1|target), data = filter(test,trial>20)))