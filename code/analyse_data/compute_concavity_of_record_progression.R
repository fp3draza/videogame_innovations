# This scripts fits a second order polynomial on each
# record trajectory and determines the concavity of the
# fit function

# requires
require(dplyr)

# load data 
df <- read.csv('data/processed/speedrun_data_clean.csv', row.names = 1)

# define function to analyse data
evaluate_second_derivative_of_polynomial_fit <- function(data_analyse){
  
  # Find the unique record identifiers
  ids <- unique(data_analyse$id)
  # Create empty dataframe
  df <- NULL
  
  # Loop through the ids
  for (id_loop in ids) {
    
    print(id_loop)
    
    # Filter data for current record id
    aa <- data_analyse %>% filter(id == id_loop)
    
    # Extract x and y
    x <- aa$run_date_in_days
    y <- aa$run_time_percentage
    
    # Fit a model of order two
    fit_second_order <- lm(y ~ I(x^2) + x + 0, offset = rep(100, length(x)))
    fit_linear <- lm(y ~ x + 0, offset = rep(100, length(x)))
    model_type <- 'quadratic'
    
    if (any(is.na(fit_second_order$coefficients))) {
      next
    }
    
    if (diff(AIC(fit_linear, fit_second_order)$AIC) > 2) {
    
      min_index <- which.min(AIC(fit_linear, fit_second_order)$AIC)
      
      if (min_index == 1) {
        
        # Extract model parameters
        model_coef <- fit_linear$coefficients
        #intercept <- as.numeric(model_coef[1])
        #x1 <- as.numeric(model_coef[2])
        x2 <- 0
        adj_rsq <- summary(fit_linear)[9]
        model_type <- 'linear'
      }
      
      if (min_index == 2) {
        
        # Extract model parameters
        model_coef <- fit_second_order$coefficients
        #intercept <- as.numeric(model_coef[1])
        #x1 <- as.numeric(model_coef[2])
        x2 <- as.numeric(model_coef[1])
        adj_rsq <- summary(fit_second_order)[9]
        
      }

    }
    
    else{
      # Extract model parameters
      model_coef <- fit_second_order$coefficients
      #intercept <- as.numeric(model_coef[1])
      #x1 <- as.numeric(model_coef[2])
      x2 <- as.numeric(model_coef[1])
      adj_rsq <- summary(fit_second_order)[9]
      
    }
    
    # Build the equation with fit parameters
    #fit_expression <- as.expression(bquote(.(intercept) + .(x1)*x + .(x2)*x^2))
    # Set the second order derivative to FALSE (i.e. negative)
    #second_deriv <- eval(D(D(fit_expression, 'x'), 'x'))
    concave_up <- FALSE
    # Check if second order derivative is greater than zero
    if (x2 * 2 > 0)  {
      concave_up <- TRUE
    }
   # append result to dataframe
    df <- rbind(df, c(id_loop, concave_up, x2 * 2, adj_rsq, model_type))
  }
  
  df <- as.data.frame(df)
  colnames(df) <- c('id', 'is_concave_up', 'second_deriv', 'adj_rsq', 'model_type')
  return(df)

}

# run the function
data_concavity <- evaluate_second_derivative_of_polynomial_fit(df)
data_concavity$id <- as.character(data_concavity$id)
data_concavity$is_concave_up <- as.logical(data_concavity$is_concave_up)
data_concavity$second_deriv <- as.numeric(data_concavity$second_deriv)
data_concavity$adj_rsq <- as.numeric(data_concavity$adj_rsq)
data_concavity$model_type <- as.character(data_concavity$model_type)

# merge with original data
df <- left_join(df, data_concavity)

# write data to file
write.csv(df, 'data/processed/speedrun_data_clean_concavity.csv')
