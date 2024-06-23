source('const/glob.R')

#Attributes -------

#severity generating function
severity_sim <- function() rpois(1,l_severity)
#Generates age, deprivation and exit thetas based on asd_means and asd_varcov
age_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['age']
deprivation_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['deprivation']
patience_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['patience']

#gender sim; is it me or this useless for now? no corr, but future implementation
#for potential split in los / specialty.
sex_sim <- function() ifelse(runif(1)<0.5,1,0)

# Priotisation --------

#Very basic scenario where priotisation is based off severity
calc_prio <- function() {
  attr_severity <- simmer::get_attribute(sim,'severity')
  return(attr_severity)
}

# Patience -------

pat_patience <- function(){
  
  #get attributes
  attr_severity <- simmer::get_attribute(sim,'severity')
  attr_pat <- simmer::get_attribute(sim,'patience')
  
  patience_val <- rpois(1,cent_pat) - (attr_severity/exp(attr_pat))
  
  return(patience_val)
}

#Probabilities --------

#probability of being referred (we assume it's 0.9 here)
referral_probability <- function() ifelse(runif(1) > 0.9, 1, 2)
#probability of getting treated at first fup
treatment_probability <- function() runif(1) > 0.37
#admission probability. This is 0.40 on avg.
admit_probability <- function() ifelse(runif(1) < 0.4, 1, 2)
#average wait times for a finished first is 21w now. needs to change.
op_wait_times <- function() rnorm(1,21,1)
#assumed FUP within a month, max
fup_wait_time <- function() runif(1, min = 1, max = 4)
#Same for a GP. (What are GP appointment times on avg?)
gp_wait_time <- function() runif(1, min = 1, max = 4)
#this is confusing, but inter-arrival times?
time <- function() runif(1, min = 1, max = 3)
#waits for non-admit pathway (fups?)
non_admit_wait_time <- function() rnorm(1,15,1)
los <- function() runif(1, min = 0.001, max = 0.4)
patient_sim <- function() c(rexp(1, 1), rep(0,(pat_n)-1))