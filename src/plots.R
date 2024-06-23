source('src/simulation.R')

# Waitlist Size -----

plot_waitlist_size <- ggplot()+
  geom_line(data=waitlist,aes(x=time,y=waitlist,group=replication),alpha=0.1)+
  geom_line(data=waitlist_summary,aes(x=time,y=waitlist),col='red',linewidth=1) +
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Waitlist Size') + 
  xlim(0,75)

# Mean flow times -----

plot_flow_times <- ggplot()+
  geom_line(data=flow_times,aes(x=time,y=flow,group=replication),alpha=0.1)+
  geom_line(data=flow_times_avg,aes(x=time,y=flow),col='red',linewidth=1)+
  ylim(0,35)+
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Flow Time')

# Additional Plots -----

plot(resources, metric = "utilization")
plot(resources, metric = "usage",items = "server")
plot(resources, metric = "usage",items = "queue")
plot(resources, metric = "usage",items = "system")



  
