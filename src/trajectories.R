source('const/glob.R')
source('src/attributes.R')

gp_trajectory <- simmer::trajectory() |> 
  #The underlying assumption here is that everybody who gets an appointment needs to see
  #a GP - they can either be referred or not but need to be seen and no drop-off can occur
  #at this period. This is a weakness in the model, but not a particularly strong one.
  simmer::log_('Patient attending GP APPOINTMENT') |> 
  simmer::seize('primary_care_capacity',1) |> 
  simmer::timeout(gp_wait_time) |> 
  #Probability is weak. Needs to be functionalised.
  simmer::branch(option = referral_probability,
                 continue=c(T,T),
                 #GP decides not to refer
                 simmer::trajectory('no_referral') |> 
                   #Referral decision made
                   simmer::set_attribute('referral_flag',0) |> 
                   simmer::log_('Patient REJECTED') |> 
                   simmer::release('primary_care_capacity',1) |> 
                   simmer::leave(prob = 1),
                 #GP decides to refer
                 simmer::trajectory("referral") |> 
                   #Referral decision made
                   simmer::set_attribute('referral_flag',1) |> 
                   simmer::log_('Patient REFERRED') |> 
                   simmer::release("primary_care_capacity", 1))

op_trajectory <- simmer::trajectory() |> 
  #Time to get an OP appointment (queue)
  #Seize either a nurse or a doctor or both. This needs to be determined by a probability
  # based off of the patient severity, and queue availability.
  simmer::log_('Attending FIRST appointment...',tag='op_branch') |> 
  simmer::seize('op_capacity',1) |> 
  simmer::timeout(op_wait_times) |> 
  #OUTCOME reached in FIRST appointment
  simmer::log_('Patient TREATED') |> 
  simmer::release('op_capacity',1)

non_admit_trajectory <- simmer::trajectory() |> 
  simmer::log_(tag = 'non_admit_start', 'Patient awaiting OUTCOME...') |> 
  simmer::seize('op_capacity',1) |> 
  simmer::timeout(non_admit_wait_time) |> 
  simmer::log_('Attending FUP appointment...') |> 
  simmer::set_attribute('fups',1,mod='+') |> 
  simmer::release('op_capacity',1) |> 
  #rollback probit depends on a function for it.
  simmer::rollback('non_admit_start',check=treatment_probability)

admit_trajectory <- simmer::trajectory() |> 
  simmer::log_('Patient given DECISION TO ADMIT') |>
  simmer::set_attribute('admit_flag',1) |> 
  simmer::log_('Patient ADMITTED') |>
  simmer::seize('bed',1) |> 
  simmer::seize('acute_nurse',1) |> 
  simmer::seize('acute_doctor',1) |> 
  #length of stay: average should be less than 1 (most are DC)
  simmer::timeout(los) |> 
  simmer::log_('Patient DISCHARGED') |>
  simmer::release('bed',1) |> 
  simmer::release('acute_nurse',1) |> 
  simmer::release('acute_doctor',1)
  
outcome_trajectory <- simmer::trajectory() |>
  simmer::branch(option = admit_probability,
                 continue = c(T,T),
                 #Patient is given DECISION TO ADMIT
                 admit_trajectory,
                 non_admit_trajectory)

unused_trajectory <- simmer::trajectory() |> 
  simmer::log_('Patient UNFINISHED')

# Simulation -----

#Actual simulation
sims <- parallel::mclapply(1:rep_n, function(i) {
  
  sim <- simmer::simmer('sim')
  
  patient <- simmer::trajectory() |>
    #Sets all attributes
    simmer::set_attribute('sex', sex_sim) |> 
    simmer::set_attribute('age', age_sim) |> 
    simmer::set_attribute('deprivation', deprivation_sim) |> 
    simmer::set_attribute('patience', patience_sim) |> 
    simmer::set_attribute('severity', severity_sim) |> 
    #prioritisation
    simmer::set_prioritization(function() {
      prio <- simmer::get_prioritization(sim)
      attr <- simmer::get_attribute(sim,'severity')
      c(attr, prio[[2]]+1, FALSE)
    }) |> 
    #set attributes: these will be determined elsewhere.
    simmer::set_attribute('fups',0) |> 
    #Patient arrives
    simmer::log_('Patient ARRIVING...') |>
    #Renege at any point if patience is exceeded. Patience is a calculated
    #variable based off of underlying patient characteristics.
    simmer::renege_in(t= 
                        #Unhappy with this, but can't figure out a way to get it to work
                        function(){
                          
                          #get attributes
                          attr_severity <- simmer::get_attribute(sim,'severity')
                          #This is absolutely moronic. But take it as is for now.
                          attr_pat <- abs(simmer::get_attribute(sim,'patience')) |> exp()
                          
                          patience_val <- cent_pat * rbeta(n=1,shape1=attr_severity,shape2 = attr_pat)
                          
                          return(patience_val)
                          
                          },
                      #Dropoff trajectory. Patient just says byeeee
                      out = simmer::trajectory() |> 
                        simmer::log_('Patient DROPPED OFF') |> 
                        # Set drop-off flag
                        simmer::set_attribute('dropoff_flag',1)) |> 
    #Initial GP appointment: this occurs immediately.
    simmer::join(gp_trajectory) |> 
    simmer::set_attribute('clock_start', function() {simmer::now(sim)}) |> 
    simmer::timeout(time) |> 
    #clock start
    simmer::join(op_trajectory) |> 
    simmer::timeout(time) |> 
    simmer::join(outcome_trajectory) |> 
    simmer::handle_unfinished(unused_trajectory)
  
  sim |>
    simmer::add_resource("primary_care_capacity", capacity = gp_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)|>
    simmer::add_resource("op_capacity", capacity = op_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
    simmer::add_resource("acute_doctor", capacity = acute_doctor_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
    simmer::add_resource("acute_nurse", capacity = acute_nurse_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
    simmer::add_resource("bed", capacity = bed_cap, queue_size=Inf, queue_size_strict=T, preemptive=TRUE)  |>
    simmer::add_generator("patient", patient, function() rexp(1, pat_n), mon=2) |>
    simmer::run(until=sim_time) |> 
    simmer::wrap()
  
})

