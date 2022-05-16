
stretched_exp_fit <- function(df, lambda = 0.1, 
                              beta  = 1,
                              I_inf = 0.5, 
                              iter_max){
  
  R_sq_old <- 0.
  epsilon_R_sq <-1e-7
  df_res <- NULL
  
  for (iter in seq(1,iter_max)){
    
    convergence <- FALSE
    
    # redefine initial guess
    lambda_guess <- lambda
    beta_guess <- beta  
    I_guess <- I_inf
    
    if(iter%%2 == 0) {
      
      fitStrExp <- nlsLM(run_time_improvement ~ stretched_exp(run_date_in_days, lambda_guess, beta, I_guess),
                         start = list(beta = beta_guess),
                         lower = c(0),
                         data = df)      
      # get fitted predictors  
      beta <- summary(fitStrExp)$coeff[1,"Estimate"]
      beta_err <- summary(fitStrExp)$coeff[1,"Std. Error"]
    }
    else 
    {
      
      fitStrExp <- nlsLM(run_time_improvement ~ stretched_exp(run_date_in_days, lambda, beta_guess, I_inf),
                         start = list(lambda = lambda_guess, 
                                      I_inf = I_guess),
                         lower = c(0,0),
                         data = df)      
      # get fitted predictors  
      lambda <- summary(fitStrExp)$coeff[1,"Estimate"]
      lambda_err <- summary(fitStrExp)$coeff[1,"Std. Error"]
      I_inf <- summary(fitStrExp)$coeff[2,"Estimate"]
      I_inf_err <- summary(fitStrExp)$coeff[2,"Std. Error"]
    }
    
    measure <- d$run_time_improvement 
    prediction <- fitted(fitStrExp)
    k <-length(coef(fitStrExp)) 
    R_sq <- my_R_Sq(measure, prediction, k)
    delta_R_sq <- R_sq-R_sq_old
    
    # check convergence 
    if (abs(delta_R_sq) < epsilon_R_sq || iter == iter_max){ 
      
      
      if (iter != iter_max) convergence <- TRUE
      
      tau0 <- lambda^(-1/beta)
      tau0_err <- lambda_err/abs(lambda*beta) # lambda contribution to the relative error 
      tau0_err <- tau0_err + beta_err*abs(log(lambda))/(beta*beta) # beta contribution to the relative error 
      tau0_err <- tau0_err*tau0 # from relative to absolute error 
      
      n <-length(measure)
      k <- 3              # actual number of predictors // fitted parameters 
      R_sq_adj <- (1-R_sq)*(n-1)/(n-k-1) 
      R_sq_adj <- 1-R_sq_adj  
      
      row <- data.frame(iter,lambda, lambda_err, beta, beta_err, I_inf, I_inf_err, tau0, tau0_err, R_sq, delta_R_sq, R_sq_adj, convergence)
      df_res <- rbind(df_res, row)
      
      break    
    }
    
    R_sq_old <- R_sq
  }
  colnames(df_res) <- c("iter", "lambda", "lambda_err",
                        "beta", "beta_err", "I_inf", "I_inf_err",
                        "tau0","tau0_err","R_sq", "delta_R_sq",
                        "R_sq_adj", "convergence")
  
  rownames(df_res) <- NULL
  return(df_res)
}
