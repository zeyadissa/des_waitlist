source('src/simulation.R')

# Waitlist Size -----

plot_waitlist_size <- ggplot()+
  geom_line(data=waitlist,aes(x=time,y=waitlist,group=replication),alpha=0.1)+
  geom_line(data=waitlist_summary,aes(x=time,y=waitlist),col=thf,linewidth=1) +
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Waitlist Size') +
  xlim(0,sim_time-10)

# Mean flow times -----

plot_flow_times <- ggplot()+
  geom_line(data=flow_times,aes(x=time,y=flow,group=replication),alpha=0.1)+
  geom_line(data=flow_times_avg,aes(x=time,y=flow),col=thf,linewidth=1)+
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Flow Time')

# Dropoffs ----

plot_dropoff_rates <- ggplot()+
  geom_histogram(data=dropoffs,aes(x=dropoff_percent),bins=10,fill=thf) +
  geom_vline(xintercept=mean(dropoffs$dropoff_percent),linewidth = 1, linetype =2,col='black') +
  scale_x_continuous(labels=scales::percent) +
  theme_bw()+
  xlab('Drop-off Percentage') +
  ylab('')

# Unfinished ----

plot_unfinished_arrivals <- ggplot()+
  geom_histogram(data=unfinished |> filter(finished==F),aes(x=ratio),fill=thf,bins=10) +
  geom_vline(xintercept=mean((unfinished |> filter(finished==F))$ratio),linewidth = 1, linetype =2,col='black') +
  scale_x_continuous(labels=scales::percent) +
  theme_bw()+
  xlab('Unfinished trajectories') +
  ylab('')

# Additional Plots -----

plot(resources, metric = "utilization")
plot(resources, metric = "usage",items = "server")
plot(resources, metric = "usage",items = "queue")
plot(resources, metric = "usage",items = "system")

# Save plots ----

ggplot2::ggsave(plot=plot_flow_times,filename='res/flow_times.png')
ggplot2::ggsave(plot=plot_waitlist_size,filename='res/waitlist_size.png')

