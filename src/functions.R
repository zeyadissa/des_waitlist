#Patience limit
GetPat <- function(){
  #get attributes
  attr_severity <- simmer::get_attribute(sim,'severity')
  #This is absolutely moronic. But take it as is for now.
  attr_pat <- abs(simmer::get_attribute(sim,'private_prop')) |> exp()
  
  patience_val <- cent_pat * rbeta(n=1,shape1=attr_severity,shape2 = attr_pat) + 1
  
  return(patience_val)}

#Admit probability
GetAdmitProb <- function(){
  
  #get attributes
  attr_severity <- simmer::get_attribute(sim,'severity')
  attr_age <- simmer::get_attribute(sim,'age')
  
  #Equation for admit probability
  #Roll to get admipro
  admipro <- ifelse(runif(1) >= rbeta(n=1,shape1=attr_severity,shape2=3),2,1)
  
  return(admipro)
  
}

#Referral probability
GetRefProb <- function(){
  
  #get attributes
  attr_severity <- simmer::get_attribute(sim,'severity')
  attr_age <- simmer::get_attribute(sim,'age')
  
  #Equation for admit probability
  #Roll to get admipro
  admipro <- ifelse(runif(1) >= rbeta(n=1,shape1=attr_severity,shape2=0.3),1,2)
  
  return(admipro)
  
}

GetFirstDiagProb <- function(){
  
  diagpro <- ifelse(runif(1) >= 0.4,1,2)
  
  return(diagpro)
  
}
