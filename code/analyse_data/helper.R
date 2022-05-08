power_law <- function(x,lambda,beta){
  return(lambda*(x**beta))
}

stretched_exp <- function(x,lambda,beta,I_inf){
  fun <- I_inf + (1.-I_inf)*exp(-power_law(x,lambda,beta))
  return(fun)
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
  range <- "E" # > 3000
  if(tau<=1000) range <- "A"  
  if(tau>1000 & tau<=2000) range <- "B"
  if(tau>2000 & tau<=3000) range <- "C"
  return(range)
}

# vectorize function
tau_classifier_V <- Vectorize(tau_classifier)


my_R_Sq <- function(measure, prediction, k){
  # k: number of predictors (fitted parameters) 
  
  tss <- sum((measure -mean(measure))^2)
  rss <- sum((measure - prediction)^2)
  n <-length(measure)
  R_sq <- 1- (rss/tss)    
  R_sq_adj <- rss*(n-1)/(tss*(n-k-1)) 
  R_sq_adj <- 1-R_sq_adj 
  
  return(R_sq)  
}

