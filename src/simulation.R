source('const/glob.R')
source('src/attributes.R')
source('src/trajectories.R')

# Simple One ---------

sim <- simmer::simmer('sim')

sims <- sim |>
  simmer::add_resource("primary_care_capacity", capacity = gp_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)|>
  simmer::add_resource("op_capacity", capacity = op_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_resource("acute_capacity", capacity = acute_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
  simmer::add_generator("patient", patient, function() rexp(1, pat_n), mon=2) |>
  simmer::run(until=sim_time) |> 
  invisible()
