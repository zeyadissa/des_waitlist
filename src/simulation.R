#Dependencies -----

#clear environment
rm(list=ls())

#trajectories
source('src/trajectories.R')

# Outputs -----

attributes <- simmer.plot::get_mon_attributes(sims)
arrivals <- simmer.plot::get_mon_arrivals(sims)
resources <- simmer.plot::get_mon_resources(sims)
arrivals_per_resource <- simmer::get_mon_arrivals(sims,per_resource=T)

# Waitlist size -----

starts <- arrivals|>
  mutate(time = round(start_time)) |> 
  group_by(time,replication) |>
  tally() |> 
  rename('start'=n)

stops <- arrivals|>
  mutate(time = round(end_time)) |> 
  group_by(time,replication) |>
  count() |> 
  rename('end'=n)

waitlist <- expand.grid('time' = 1:sim_time,'replication'=1:rep_n) |> 
  left_join(starts,by=c('time','replication')) |> 
  left_join(stops,by=c('time','replication')) %>%
  replace(is.na(.), 0) |> 
  group_by(replication) |> 
  mutate(stops_cumu = cumsum(end),
         starts_cumu = cumsum(start),
         waitlist = starts_cumu - stops_cumu)

waitlist_summary <- expand.grid('time' = 1:sim_time,'replication'=1:rep_n) |> 
  left_join(starts,by=c('time','replication')) |> 
  left_join(stops,by=c('time','replication')) %>%
  replace(is.na(.), 0) |> 
  group_by(replication) |> 
  mutate(stops_cumu = cumsum(end),
         starts_cumu = cumsum(start),
         waitlist = starts_cumu - stops_cumu) |> 
  group_by(time) |> 
  summarise(waitlist = mean(waitlist,na.rm=T))

# Waits

wait_by_resource<-resources %>%
  dplyr::group_by(resource,replication) |> 
  dplyr::summarise(active = sum(head(server, -1) * diff(time)),
            waiting = sum(head(queue, -1) * diff(time))
  ) |> 
  dplyr::group_by(resource) |> 
  dplyr::summarise(waiting = mean(waiting),
            active = mean(active),
            ratio = waiting/active)

# Activity time

flow_times_avg <- arrivals |> 
  mutate(flow = end_time-start_time) |> 
  mutate(time = round(end_time)) |> 
  group_by(time) |> 
  summarise(flow = mean(flow,na.rm=T))

flow_times <- arrivals |> 
  mutate(flow = end_time-start_time) |> 
  mutate(time = round(end_time)) |> 
  group_by(replication,time) |> 
  summarise(flow = mean(flow,na.rm=T))

#Warning: slow to run. I'm sure there's a faster way. Someone tell me how.
# df_waits <- lapply(
#   1:sim_time,
#   function(x){
#     df <- clocks |>
#       dplyr::mutate(
#         wait = case_when(
#           x < clock_start ~ NA,
#           x >= clock_start & x <= clock_stop ~ x - clock_start,
#           x > clock_stop ~ NA)) |>
#       dplyr::select(replication,name,wait) |>
#       tidyr::drop_na() |>
#       dplyr::mutate(t = x)
#   }
# ) |>
#   data.table::rbindlist() |>
#   dplyr::group_by(replication,t) |>
#   dplyr::summarise(wait_time = mean(wait,na.rm=T))
# 
