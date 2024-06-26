#Save test results

#write.csv(flow_times_avg,'test/low_severity_flow_sum.csv')
#write.csv(flow_times,'test/low_severity_flow.csv')

#write.csv(waitlist_summary,'test/low_severity_waitlist_sum.csv')
#write.csv(waitlist,'test/low_severity_waitlist.csv')

#Read test results
flow_1 <- read.csv('test/low_severity_flow_sum.csv')
flow_2 <- read.csv('test/low_severity_flow.csv')

waitlist_1 <- read.csv('test/low_severity_waitlist_sum.csv')
waitlist_2 <- read.csv('test/low_severity_waitlist.csv')

#Test graphs 
#blue = low severity
#red = high severity

ggplot()+
  geom_line(data=flow_times_avg,aes(x=time,y=flow),col=thf,linewidth=1)+
  geom_line(data=flow_1,aes(x=time,y=flow),linewidth=1,col='blue')+
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Flow Time')


ggplot()+
  geom_line(data=waitlist,aes(x=time,y=waitlist,group=replication),alpha=0.1)+
  geom_line(data=waitlist_summary,aes(x=time,y=waitlist),col=thf,linewidth=1) +
  geom_line(data=waitlist_2,aes(x=time,y=waitlist,group=replication),alpha=0.1)+
  geom_line(data=waitlist_1,aes(x=time,y=waitlist),col='blue',linewidth=1) +
  theme_bw() +
  xlab('Simulation Time') +
  ylab('Waitlist Size') +
  xlim(0,sim_time-10)

