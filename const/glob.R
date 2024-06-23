# Attributes -----

#Probability of male
p_sex <- 0.5
#lambda for rpois
l_severity <- 10
#Central patience: adjust for median wait
cent_pat <- 35

# Random matrix of n x n ; this needs to be populated with
# Actual values from HES, but currently unable to do so...
n <- 3
A <- matrix(runif(n^2)*2-1, ncol=n) 
adp_varcov <- t(A) %*% A
#names for ease of access
colnames(adp_varcov) <- c('age','deprivation','patience')
rownames(adp_varcov) <- c('age','deprivation','patience')

#means, again need to be realistic
adp_means <- c(35,0,0)

# Global -----

#Simulation time in weeks (1yr)
warmup <- 52
sim_time <- 52 + warmup
#Number of patients simulated per week
pat_n <- 10
#number of simulations
rep_n <- 50

#TEST METRICS: DO NOT TOUCH. REMOVE LATER
gp_cap <- 15
op_cap <- 10
acute_cap <- 2
