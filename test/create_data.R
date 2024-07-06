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

flow_times_avg <- arrivals |> 
  dplyr::mutate(flow = end_time-start_time) |> 
  dplyr::mutate(time = round(end_time)) |> 
  dplyr::group_by(time) |> 
  dplyr::summarise(flow = mean(flow,na.rm=T))

# Dropoffs -------

dropoffs <- attributes |> 
  dplyr::filter(key == 'dropoff_flag') |> 
  dplyr::group_by(replication) |> 
  dplyr::summarise(dropoffs = sum(value,na.rm=T)) |> 
  dplyr::left_join(attributes |> 
                     dplyr::filter(key == 'sex') |> 
                     group_by(replication) |> 
                     tally(),
            by='replication') |>  
  dplyr::mutate(dropoff_percent = dropoffs/n)

dropped_off_patients <- (dropoffs <- attributes |> 
  dplyr::filter(key == 'dropoff_flag') |> 
  dplyr::filter(value == 1))$name |> unique()

tot_arr <- arrivals |> 
  mutate(time = round(activity_time)) |> 
  group_by(time) |> 
  tally(name='tot')

dropoff_times <- arrivals |> 
  filter(name %in% dropped_off_patients) |> 
  mutate(time = round(activity_time)) |> 
  group_by(time) |> 
  tally()

patience_data <- attributes |> 
  dplyr::filter(key == 'patience')

  
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

# Waitlist / Queue -----

referred_patients <- (attributes |> 
                        filter(key == 'referral_flag') |> 
                        filter(value == 1))$name |>  unique()

queue <- arrivals |> 
  dplyr::filter(finished == F &
                  name %in% referred_patients) |> 
  dplyr::mutate(flow_time = sim_time - start_time,
                flow = round(flow_time)) |> 
  group_by(flow) |> 
  tally()

#Severity -------

severity <- attributes |> 
  dplyr::select(!c(time,replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key %in% c('severity'))

# Dropoffs, referrals and admissions -----

traj_diag_dra <- attributes |> 
  dplyr::select(!c(time,replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key %in% c('dropoff_flag','admit_flag','referral_flag','treatment_decision_flag')) |> 
  dplyr::group_by(key) |> 
  dplyr::summarise(ratio=sum(value,na.rm=T)/length(unique(attributes$name))) |> 
  dplyr::arrange(desc(ratio))

# Priorities -----

priorities <- attributes |> 
  dplyr::select(!c(replication)) |> 
  dplyr::ungroup() |> 
  dplyr::filter(key %in% c('priority')) |>
  group_by(name) |> 
  tidyr::pivot_wider(names_from='key',values_from='value') |> 
  left_join(attributes |>   
              dplyr::filter(key %in% c('arrival_time','severity')) |> 
              select(key,value,name) |> 
              pivot_wider(names_from='key',values_from='value'),
            by='name') |> 
  mutate(flow=time-arrival_time)

# Priorities -----
age_plot <- attributes |> 
  filter(key == 'age')

# Fups ------

fups <- attributes |> 
  dplyr::filter(key == 'fups') |> 
  dplyr::group_by(name) |> 
  dplyr::summarise(value=max(value))

