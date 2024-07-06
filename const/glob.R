# Attributes -----

#Probability of male
p_sex <- 0.5
#lambda for rpois
l_severity <- 4
#Central patience: adjust for median wait: a higher value results in longer waits.
#Suggest max 100, min = 18
cent_pat <- 30

# Random matrix of n x n ; this needs to be populated with
# Actual values from HES, but currently unable to do so...
n <- 3
A <- matrix(runif(n^2)*2-1, ncol=n) 
adp_varcov <- t(A) %*% A
#names for ease of access
colnames(adp_varcov) <- c('age','deprivation','patience')
rownames(adp_varcov) <- c('age','deprivation','patience')

#waits
glob_t <- function() rexp(1,1)

#means, again need to be realistic
adp_means <- c(1,1,1)

# Global -----

#Simulation time in weeks (1yr)
warmup <- 52
sim_time <- 104 + warmup
#Number of patients simulated per week
pat_n <- 50
#number of simulations
rep_n <- 1

gp_prop <- 1.3
op_prop <- 1
acute_prop <- 0.2

#CAPACITY METRICS: DO NOT TOUCH. REMOVE LATER
gp_cap <- simmer::schedule(1:sim_time, 1^(1:sim_time)*gp_prop*pat_n, period=sim_time)
op_cap <- simmer::schedule(1:sim_time, 1^(1:sim_time)*op_prop*pat_n, period=sim_time)
acute_cap <- simmer::schedule(1:sim_time, 1^(1:sim_time)*acute_prop*pat_n, period=sim_time)

thf<-'#dd0031'
