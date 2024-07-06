source('test/create_data.R')

# Waitlist Size -----

waitlist_plot <- ggplot()+
  geom_line(data=waitlist_summary,aes(x=time,y=waitlist),col=thf,linewidth=1) +
  xlab('Simulation Time') +
  ylab('Number of arrivals') +
  ggtitle('Waitlist Size',
          subtitle = 'Total size of unfinished arrivals at each period') +
  theme_bw()+
  theme(text = element_text(size = 16))

# Mean flow times -----

flow_times_plot <- ggplot()+
  geom_line(data=flow_times_avg,aes(x=time,y=flow),col=thf,linewidth=1)+
  xlab('Simulation Time') +
  ylab('Flow Time (weeks)')+
  ggtitle('Mean flow time for finished arrivals',
          subtitle ='Average time from arrival start date to arrival end date at each period')+
  theme_bw()+
  theme(text = element_text(size = 16))

# Queue distribution -------

#Distribution
queue_plot <- ggplot()+
  geom_col(data=queue,aes(x=flow%/% 4,y=n),fill=thf)+
  xlab('Waiting times (months)') +
  ylab('Number of arrivals in queue')+
  ggtitle('Queue size',
          subtitle='Number of arrivals by waiting bucket at simulation end') +
  theme_bw()+
  theme(text = element_text(size = 16))

# Trajectories ----

#Severity distribution
severity_plot <- ggplot()+
  geom_histogram(data=severity,aes(x=value),fill=thf,col='black') +
  geom_vline(xintercept=mean(severity$value,na.rm=T),linewidth=2,linetype=2) +
  ylab('Count of arrivals')+
  xlab('Severity')+
  theme_bw()+
  theme(text = element_text(size = 16))

#DRA plot
dra_plot <- ggplot()+
  geom_col(data=traj_diag_dra,aes(x=key,y=ratio),fill=thf,col='black')+
  geom_label(data=traj_diag_dra,aes(x=key,y=ratio,label=paste0(round(ratio*100,1),'%')))+
  xlab('')+
  ylab('Percentage (%) of all simulated arrivals')+
  ggtitle('Diagnostic plots',
          subtitle='Diagnostic plots of simulated arrivals by pathway')+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(text = element_text(size = 16))

#Ratio of unfinished arrivals
print((unfinished |> filter(finished==F))$ratio)

#Priority distribution
priority_plot <- ggplot()+
  geom_density(data=priorities,aes(x=as.integer(priority)),bw=0.4,col='black',fill=thf)+
  geom_vline(xintercept=mean(priorities$priority,na.rm=T),linewidth=2,linetype=2)+
  theme_bw()+
  xlab('Priority Score') +
  ylab('Density') +
  ggtitle('Simulated priority density at simulation end')+
  theme(text = element_text(size = 16))

#Fups plot
fups_plot <- ggplot()+
  geom_density(data=fups,aes(x=as.integer(value)),bw=0.4,col='black',fill=thf)+
  geom_vline(xintercept=mean(fups$value,na.rm=T),linewidth=2,linetype=2)+
  theme_bw()+
  xlab('Number of Follow-ups') +
  ylab('Density') +
  ggtitle('Simulated follow-up density at simulation end')+
  theme(text = element_text(size = 16))

# Strain on resources -------

#acute capacity
acute_strain_plot <- plot(resources, metric = "usage",'acute_capacity') +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  xlab('Simulation time') +
  ylab('Resource usage (units)') +
  labs(col='Measure')

# Diagnostics - Dropoffs -----

dropoff_times_plot <- ggplot()+
  geom_col(data=dropoff_times,aes(x=time,y=n),fill=thf,col='black') +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  xlab('Wait times (weeks)') +
  ylab('Number of dropoffs as a proportion of wait bucket')

patience_plot <- ggplot()+
  geom_histogram(data=patience_data,aes(x=value),fill=thf,col='black')+
  ylab('Count of arrivals')+
  xlab('Maximum time before renegeing')+
  theme_bw()+
  theme(text = element_text(size = 16))

# Additional Plots -----

#Utilization
plot(resources, metric = "utilization") +
  geom_hline(yintercept=0.85) +
  theme_bw()

#Usage
plot(resources, metric = "usage")
plot(patient)

# Save plots ----

ggplot2::ggsave(plot=plot_flow_times,filename='res/flow_times.png')
ggplot2::ggsave(plot=plot_waitlist_size,filename='res/waitlist_size.png')

