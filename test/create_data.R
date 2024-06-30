#Dependencies -----

#clear environment
rm(list=ls())

#trajectories
source('src/simulation.R')

# Outputs -----

attributes <- simmer.plot::get_mon_attributes(sims)
arrivals <- simmer.plot::get_mon_arrivals(sims,ongoing=T)
resources <- simmer.plot::get_mon_resources(sims)
arrivals_per_resource <- simmer::get_mon_arrivals(sims,per_resource=T)

# Waitlist size -----

starts <- arrivals|>
  dplyr::mutate(time = round(start_time)) |> 
  dplyr::group_by(time,replication) |>
  dplyr::tally() |> 
  dplyr::rename('start'=n)

stops <- arrivals|>
  dplyr::mutate(time = round(end_time)) |> 
  dplyr::group_by(time,replication) |>
  dplyr::count() |> 
  dplyr::rename('end'=n)

waitlist <- expand.grid('time' = 1:sim_time,'replication'=1:rep_n) |> 
  dplyr::left_join(starts,by=c('time','replication')) |> 
  dplyr::left_join(stops,by=c('time','replication')) %>%
  replace(is.na(.), 0) |> 
  dplyr::group_by(replication) |> 
  dplyr::mutate(stops_cumu = cumsum(end),
         starts_cumu = cumsum(start),
         waitlist = starts_cumu - lag(stops_cumu))

waitlist_summary <- expand.grid('time' = 1:sim_time,'replication'=1:rep_n) |> 
  dplyr::left_join(starts,by=c('time','replication')) |> 
  dplyr::left_join(stops,by=c('time','replication')) %>%
  replace(is.na(.), 0) |> 
  dplyr::group_by(replication) |> 
  dplyr::mutate(stops_cumu = cumsum(end),
         starts_cumu = cumsum(start),
         waitlist = starts_cumu - lag(stops_cumu)) |> 
  dplyr::group_by(time) |> 
  dplyr::summarise(waitlist = mean(waitlist,na.rm=T))

# Waits

wait_by_resource<-resources |> 
  dplyr::group_by(resource,replication) |> 
  dplyr::summarise(active = sum(head(server, -1) * diff(time)),
            waiting = sum(head(queue, -1) * diff(time))
  ) |> 
  dplyr::group_by(resource) |> 
  dplyr::summarise(waiting = mean(waiting),
            active = mean(active),
            ratio = waiting/active)

# Activity time

flow_dist <- arrivals |> 
  dplyr::mutate(flow = end_time-start_time) |> 
  dplyr::mutate(time = round(end_time)) 

flow_times_avg <- arrivals |> 
  dplyr::mutate(flow = end_time-start_time) |> 
  dplyr::mutate(time = round(end_time)) |> 
  dplyr::group_by(time) |> 
  dplyr::summarise(flow = mean(flow,na.rm=T))

flow_times <- arrivals |> 
  dplyr::mutate(flow = end_time-start_time) |> 
  dplyr::mutate(time = round(end_time)) |> 
  dplyr::group_by(replication,time) |> 
  dplyr::summarise(flow = mean(flow,na.rm=T))

# Dropoffs -------

dropoffs <- attributes |> 
  dplyr::filter(key == 'dropoff_flag') |> 
  dplyr::group_by(replication) |> 
  dplyr::summarise(dropoffs = sum(value,na.rm=T)) |> 
  dplyr::left_join(attributes |> dplyr::filter(key == 'sex') |> group_by(replication) |> tally(),
            by='replication') |>  
  dplyr::mutate(dropoff_percent = dropoffs/n)

# Unfinished arrivals -----

unfinished <- arrivals |> 
  dplyr::group_by(replication,finished) |> 
  dplyr::tally() |> 
  dplyr::left_join(arrivals |> 
                     dplyr::group_by(replication) |> 
                     dplyr::tally() |> 
                     dplyr::rename('tot'=n),
            by='replication') |> 
  dplyr::mutate(ratio = n/tot)

#Other trajectory diagnostics -------

traj_diag <- attributes |> 
  dplyr::select(!c(time,replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key != 'fups') |> 
  tidyr::pivot_wider(names_from='key',values_from='value') 

traj_diag[is.na(traj_diag)] <- 0

# Dropoffs, referrals and admissions -----

traj_diag_dra <- attributes |> 
  dplyr::select(!c(time,replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key %in% c('dropoff_flag','admit_flag','referral_flag')) |> 
  dplyr::group_by(key) |> 
  dplyr::summarise(ratio=sum(value,na.rm=T)/length(unique(attributes$name)))

# Priorities -----

priorities <- attributes |> 
  dplyr::select(!c(time,replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key %in% c('priority'))

# Fups ------

fups <- attributes |> 
  dplyr::filter(key == 'fups') |> 
  dplyr::group_by(name) |> 
  dplyr::summarise(value=max(value))

