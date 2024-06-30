source('test/create_data.R')

# Waitlist Size -----

plot_waitlist_size <- ggplot()+
  #geom_line(data=waitlist,aes(x=time,y=waitlist,group=replication),alpha=1)+
  geom_line(data=waitlist_summary,aes(x=time,y=waitlist),col=thf,linewidth=1) +
  xlab('Simulation Time') +
  ylab('Waitlist Size') +
  xlim(0,sim_time-10)+
  theme_bw()+
  theme(text = element_text(size = 16))


# Mean flow times -----

plot_flow_times <- ggplot()+
  geom_line(data=flow_times,aes(x=time,y=flow,group=replication),alpha=0.1)+
  geom_line(data=flow_times_avg,aes(x=time,y=flow),col=thf,linewidth=1)+
  xlab('Simulation Time') +
  ylab('Flow Time (weeks)')+
  theme_bw()+
  theme(text = element_text(size = 16))

plot_flow_dist <- ggplot() +
  geom_histogram(data=flow_dist |>filter(finished==T),aes(x=flow),fill=thf,col='black')+
  geom_vline(xintercept=mean((flow_dist |>filter(finished==T))$flow,na.rm=T),linewidth=2,linetype=2) +
  ylab('Count of arrivals') +
  xlab('Flow Time (weeks)')+
  theme_bw()+
  theme(text = element_text(size = 16))


# Trajectories ----

#Severity distribution
severity <- ggplot()+
  geom_histogram(data=traj_diag,aes(x=severity),fill=thf,col='black') +
  geom_vline(xintercept=mean(traj_diag$severity,na.rm=T),linewidth=2,linetype=2) +
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
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(text = element_text(size = 16))

#Ratio of unfinished arrivals
print((unfinished |> filter(finished==F))$ratio)

#Priority distribution
priority_plot <- ggplot()+
  geom_density(data=priorities,aes(x=as.integer(value)),bw=0.4,col='black',fill=thf)+
  geom_vline(xintercept=mean(priorities$value,na.rm=T),linewidth=2,linetype=2)+
  scale_x_continuous(breaks=c(0:10)) +
  theme_bw()+
  theme(text = element_text(size = 16))

#Fups plot
fups_plot <- ggplot()+
  geom_density(data=fups,aes(x=as.integer(value)),bw=0.4,col='black',fill=thf)+
  geom_vline(xintercept=mean(priorities$value,na.rm=T),linewidth=2,linetype=2)+
  scale_x_continuous(breaks=c(0:10)) +
  theme_bw()+
  theme(text = element_text(size = 16))

# Additional Plots -----

plot(resources, metric = "utilization")
plot(resources, metric = "usage",items = "server")
plot(resources, metric = "usage",items = "queue")
plot(resources, metric = "usage",items = "system")

# Save plots ----

ggplot2::ggsave(plot=plot_flow_times,filename='res/flow_times.png')
ggplot2::ggsave(plot=plot_waitlist_size,filename='res/waitlist_size.png')

