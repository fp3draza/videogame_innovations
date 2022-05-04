power_law <- function(x,lambda,beta){
  return(lambda*(x**beta))
}

stretched_exp <- function(x,lambda,beta){
  return(exp(-power_law(x,lambda,beta)))
}

straight_line <- function(x,p,m){
  return(x*m +p)
}

beta_classifier <- function(beta){
  shape <- "CE" # compressed exponential
  if(beta<1) shape <- "SE" # stretched exponential
  if(beta>4) shape <- "FD" # Fermi-Dirac: sudden drop after some time 
  return(shape)
}

# vectorize function
beta_classifier_V <- Vectorize(beta_classifier)

tau_classifier <- function(tau){
  range <- "E" # > 4000
  if(tau<=1000) range <- "A"  
  if(tau>1000 & tau<=2000) range <- "B"
  if(tau>2000 & tau<=3000) range <- "C"
  if(tau>3000 & tau<=4000) range <- "D"
  return(range)
}

# vectorize function
tau_classifier_V <- Vectorize(tau_classifier)
