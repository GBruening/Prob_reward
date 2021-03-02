setwd('D:/Google Drive/Yan Experiment/4t_180trial_4block/Graphs/from_rmd')

chunk_var = 'PeakV'

a = plot_grid(plots[[chunk_var]][['abs']][['plot']],
              plots[[chunk_var]][['Delta']][['plot']],
              plots[[chunk_var]][['Delta_subj']][['plot']],
              ncol=3)
ggsave(paste(chunk_var,'.pdf',sep=''),plot =a, useDingbats = FALSE, width = 15, height = 5)

a = plot_grid(plots[[chunk_var]][['Prior_RWD']][['plot']]+theme(legend.position = 'none'),
              plots[[paste('Delta',chunk_var,sep='')]][['Prior_RWD']][['plot']]+theme(legend.position = 'none'),
              get_legend(plots[[chunk_var]][['Prior_RWD']][['plot']]),
              rel_widths = c(1,1,.2),
              ncol = 3)
ggsave(paste(chunk_var,'_PriorRWD.pdf',sep=''),plot =a, useDingbats = FALSE, width = 10, height = 5)

a = plot_grid(plots[[chunk_var]][['RPE_split_prob']][['plot']],
              plots[[chunk_var]][['RPE_split_prior']][['plot']],
              plots[[chunk_var]][['RPE_no_split']][['plot']])
ggsave(paste(chunk_var,'_RPE.pdf',sep=''),plot =a, useDingbats = FALSE, width = 12, height = 12)



chunk_var = 'RT'

a = plot_grid(plots[[chunk_var]][['abs']][['plot']],
              plots[[chunk_var]][['Delta']][['plot']],
              plots[[chunk_var]][['Delta_subj']][['plot']],
              ncol=3)
ggsave(paste(chunk_var,'.pdf',sep=''),plot =a, useDingbats = FALSE, width = 15, height = 5)

a = plot_grid(plots[[chunk_var]][['Prior_RWD']][['plot']]+theme(legend.position = 'none'),
              plots[[paste('Delta',chunk_var,sep='')]][['Prior_RWD']][['plot']]+theme(legend.position = 'none'),
              get_legend(plots[[chunk_var]][['Prior_RWD']][['plot']]),
              rel_widths = c(1,1,.2),
              ncol = 3)
ggsave(paste(chunk_var,'_PriorRWD.pdf',sep=''),plot =a, useDingbats = FALSE, width = 10, height = 5)

a = plot_grid(plots[[chunk_var]][['RPE_split_prob']][['plot']],
              plots[[chunk_var]][['RPE_split_prior']][['plot']],
              plots[[chunk_var]][['RPE_no_split']][['plot']])
ggsave(paste(chunk_var,'_RPE.pdf',sep=''),plot =a, useDingbats = FALSE, width = 12, height = 12)



chunk_var = 'PeakVinout'

a = plot_grid(plots[[chunk_var]][['abs']][['plot']],
              plots[[chunk_var]][['Delta']][['plot']],
              plots[[chunk_var]][['Delta_subj']][['plot']],
              ncol=3)
ggsave(paste(chunk_var,'.pdf',sep=''),plot =a, useDingbats = FALSE, width = 15, height = 5)

a = plot_grid(plots[[chunk_var]][['RPE_split_prob']][['plot']],
              plots[[chunk_var]][['RPE_split_prior']][['plot']],
              plots[[chunk_var]][['RPE_no_split']][['plot']])
ggsave(paste(chunk_var,'_RPE.pdf',sep=''),plot =a, useDingbats = FALSE, width = 12, height = 12)



a = plot_grid(plots[['PeakV']][['abs']][['plot']],
              plots[['PeakV']][['Delta']][['plot']],
              plots[['PeakV']][['Delta_subj']][['plot']],
              plots[['RT']][['abs']][['plot']],
              plots[['RT']][['Delta']][['plot']],
              plots[['RT']][['Delta_subj']][['plot']],
              ncol=3,
              labels = 'AUTO')
ggsave(paste('PeakV_RT.pdf',sep=''),plot =a, useDingbats = FALSE, width = 12, height = 8)

a = plot_grid(plots[['PeakV']][['Prior_RWD']][['plot']],
              plots[['DeltaPeakV']][['Prior_RWD']][['plot']],
              plots[['RT']][['Prior_RWD']][['plot']],
              plots[['DeltaRT']][['Prior_RWD']][['plot']],
              ncol=2,
              labels = 'AUTO')
ggsave(paste('PeakV_RT.pdf',sep=''),plot =a, useDingbats = FALSE, width = 12, height = 8)


library(predictmeans)
a = aggregate(DeltaRT ~ Probability + Subject, filt_data, mean)
b = aggregate(DeltaRT ~ Probability, filt_data, mean)
a = ggplot()+
  # geom_point(data = b, aes(x = Probability, y = DeltaRT), size = 10)+
  # geom_line(data = b, aes(x = Probability, y = DeltaRT), size = 3)+
  geom_point(data = a, aes(x = Probability, y = DeltaRT, group = Subject))+
  geom_line(data = a, aes(x = Probability, y = DeltaRT, group = Subject))+
  labs(x = 'Probability of Reward', y = 'Reaction Time DELTA (S)')
ggsave(paste('RT_subj.pdf',sep=''),plot =a, useDingbats = FALSE, width = 10, height = 10)