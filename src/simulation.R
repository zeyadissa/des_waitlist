source('const/glob.R')
source('src/attributes.R')
source('src/trajectories.R')

# Simulation: parallel -----

# #Actual simulation
# sims <- parallel::mclapply(1:rep_n, function(i) {
#   
#   sim <- simmer::simmer('sim')
#   
#   patient <- simmer::trajectory() |>
#     #Sets all attributes
#     simmer::set_attribute('sex', sex_sim) |> 
#     simmer::set_attribute('age', age_sim) |> 
#     simmer::set_attribute('deprivation', deprivation_sim) |> 
#     simmer::set_attribute('patience', patience_sim) |> 
#     simmer::set_attribute('severity', severity_sim) |> 
#     #prioritisation
#     simmer::set_prioritization(function() {
#       prio <- simmer::get_prioritization(sim)
#       attr <- simmer::get_attribute(sim,'severity')
#       c(attr, prio[[2]]+1, FALSE)
#     }) |> 
#     #set attributes: these will be determined elsewhere.
#     simmer::set_attribute('fups',0) |> 
#     #Patient arrives
#     simmer::log_('Patient ARRIVING...') |>
#     #Renege at any point if patience is exceeded. Patience is a calculated
#     #variable based off of underlying patient characteristics.
#     simmer::renege_in(t= 
#                         #Unhappy with this, but can't figure out a way to get it to work
#                         function(){
#                           
#                           #get attributes
#                           attr_severity <- simmer::get_attribute(sim,'severity')
#                           #This is absolutely moronic. But take it as is for now.
#                           attr_pat <- abs(simmer::get_attribute(sim,'patience')) |> exp()
#                           
#                           patience_val <- cent_pat * rbeta(n=1,shape1=attr_severity,shape2 = attr_pat)
#                           
#                           return(patience_val)
#                           
#                           },
#                       #Dropoff trajectory. Patient just says byeeee
#                       out = simmer::trajectory() |> 
#                         simmer::log_('Patient DROPPED OFF') |> 
#                         # Set drop-off flag
#                         simmer::set_attribute('dropoff_flag',1)) |> 
#     #Initial GP appointment: this occurs immediately.
#     simmer::join(gp_trajectory) |> 
#     simmer::set_attribute('clock_start', function() {simmer::now(sim)}) |> 
#     simmer::timeout(time) |> 
#     #clock start
#     simmer::join(op_trajectory) |> 
#     simmer::timeout(time) |> 
#     simmer::join(outcome_trajectory) |> 
#     simmer::handle_unfinished(unused_trajectory)
#   
#   sim |>
#     simmer::add_resource("primary_care_capacity", capacity = gp_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)|>
#     simmer::add_resource("op_capacity", capacity = op_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
#     simmer::add_resource("acute_doctor", capacity = acute_doctor_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
#     simmer::add_resource("acute_nurse", capacity = acute_nurse_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
#     simmer::add_resource("bed", capacity = bed_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
#     simmer::add_generator("patient", patient, function() rexp(1, pat_n), mon=2) |>
#     simmer::run(until=sim_time) |> 
#     simmer::wrap()
#   
# })

# Simple One -----

sim <- simmer::simmer('sim')

sims <- sim |>
  simmer::add_resource("primary_care_capacity", capacity = gp_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)|>
  simmer::add_resource("op_capacity", capacity = op_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_resource("acute_doctor", capacity = acute_doctor_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_resource("acute_nurse", capacity = acute_nurse_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_resource("bed", capacity = bed_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_generator("patient", patient, function() rexp(1, pat_n), mon=2) |>
  simmer::run(until=sim_time) |> 
  invisible()
