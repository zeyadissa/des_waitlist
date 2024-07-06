source('const/glob.R')

#Attributes -------

#severity generating function
severity_sim <- function() runif(1,min=1.1,max=l_severity)
#Generates age, deprivation and exit thetas based on asd_means and asd_varcov
# age_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['age']
# deprivation_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['deprivation']
patience_sim  <- function() MASS::mvrnorm(n=1,mu=adp_means,Sigma=adp_varcov)['patience']

#This are temporary for now just to see what it actually looks like. 
age_sim <- function() rnorm(1,mean=35,sd=20) |> abs()
deprivation_sim <- function() runif(1,min=0,max=1)


#gender sim; is it me or this useless for now? no corr, but future implementation
#for potential split in los / specialty.
sex_sim <- function() ifelse(runif(1)<p_sex,1,0)

#Probabilities --------

#probability of being referred (we assume it's 0.9 here)
referral_probability <- function() ifelse(runif(1) > 0.9, 1, 2)
#probability of getting treated at first fup
treatment_probability <- function() runif(1) > 0.37
#Interarrival times?
non_admit_wait_time <- glob_t
time <- glob_t
los <- glob_t
gp_wait_time <- glob_t
fup_wait_time <- glob_t
op_wait_times <- glob_t

#Arrival schedules
patient_sim <- function() c(rexp(1, 1), rep(0,(pat_n)-1))