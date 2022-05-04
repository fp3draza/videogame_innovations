power_law <- function(x,lambda,beta){
  return(lambda*(x**beta))
}

stretched_exp <- function(x,lambda,beta){
  return(exp(-power_law(x,lambda,beta)))
}

straight_line <- function(x,p,m){
  return(x*m +p)
}

beta_classifier  <- function(beta){
  shape <- "CE" # compressed exponential
  if(beta<1) shape <- "SE" # stretched exponential
  if(beta>4) shape <- "FD" # Fermi-Dirac: sudden drop after some time 
  return(shape)
}
