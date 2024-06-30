source('const/glob.R')
source('src/attributes.R')
source('src/functions.R')

#GP Trajectory -----

gp_trajectory <- simmer::trajectory() |> 
  #The underlying assumption here is that everybody who gets an appointment needs to see
  #a GP - they can either be referred or not but need to be seen and no drop-off can occur
  #at this period. This is a weakness in the model, but not a particularly strong one.
  simmer::log_('Patient attending GP APPOINTMENT') |> 
  simmer::seize('primary_care_capacity',1) |> 
  simmer::timeout(gp_wait_time) |> 
  #Probability is weak. Needs to be functionalised.
  simmer::branch(option = GetRefProb,
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

#OP Trajectory -----

op_trajectory <- simmer::trajectory() |> 
  #Time to get an OP appointment (queue)
  #Seize either a nurse or a doctor or both. This needs to be determined by a probability
  # based off of the patient severity, and queue availability.
  simmer::log_('Attending FIRST appointment...',tag='op_branch') |> 
  simmer::set_attribute('first',1) |> 
  simmer::set_attribute("arrival_time", function() simmer::now(sim)) |> 
  #Important to recompute probability
  simmer::renege_if(
    "recompute_priority",
    out = simmer::trajectory() |>
      # e.g., increase priority if wait_time > 3
      simmer::set_prioritization(function() {
        if (simmer::now(sim) - simmer::get_attribute(sim, "arrival_time") > 18)
          c(1, NA, NA)     # only change the priority
        else c(NA, NA, NA) # don't change anything
      }, mod="+")  |>
      # go 2 steps back to renege_if
      simmer::rollback(2)) |>
  simmer::seize('op_capacity',1) |>
  simmer::renege_abort() |>
  #OUTCOME could be reached?
  simmer::log_('Finished FIRST appointment') |> 
  simmer::timeout(op_wait_times) |> 
  # Send signal that resource is being released
  simmer::send("recompute_priority") |>
  simmer::timeout(0) |>
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

#Acute Trajectory -----

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

#Outcome & Other Trajectory -----

outcome_trajectory <- simmer::trajectory() |>
  simmer::branch(option = GetAdmitProb,
                 continue = c(T,T),
                 #Patient is given DECISION TO ADMIT
                 admit_trajectory,
                 non_admit_trajectory)

unused_trajectory <- simmer::trajectory() |> 
  simmer::log_('Patient UNFINISHED')

# Patient trajectory -----
patient <- simmer::trajectory() |>
  #Sets all attributes
  simmer::set_attribute('sex', sex_sim) |> 
  simmer::set_attribute('age', age_sim) |> 
  simmer::set_attribute('deprivation', deprivation_sim) |> 
  simmer::set_attribute('patience', patience_sim) |> 
  simmer::set_attribute('severity', severity_sim) |> 
  #set attributes: these will be determined elsewhere.
  simmer::set_attribute('fups',0) |> 
  #Patient arrives
  simmer::log_('Patient ARRIVING...') |>
  #Renege at any point if patience is exceeded. Patience is a calculated
  #variable based off of underlying patient characteristics.
  simmer::renege_in(t= GetPat,
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
  simmer::handle_unfinished(unused_trajectory) |> 
  simmer::set_attribute('priority', function() {simmer::get_prioritization(sim)[[1]]} )

