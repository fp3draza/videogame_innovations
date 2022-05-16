# # OLD Linear fitting
# # Data linearization for stretched-exponential decay
# 
# df2 <- my_data %>% select(id, run_date_in_days, run_time_percentage) 
# df1 <- df2 %>% mutate(log_improvement=log(run_time_percentage/100)) %>%
#   filter(log_improvement<0) %>% 
#   mutate(log_log_improvement=log(-log_improvement),
#          log_time = log(run_date_in_days))
# 
# 
# df_fit <- NULL
# 
# for (my_id in ids) {
# 
#   d <-df1 %>% filter(id == my_id) %>% select(id, log_time, log_log_improvement)
#   
#   fit <- lm(log_log_improvement ~ log_time, data = d) # fit the model
#   d$predicted <- predict(fit)   # Save the predicted values
#   d$residuals <- residuals(fit) # Save the residual values
# 
#   print("")
#   print(paste("now id = ", my_id))
# 
#   
#   beta0 <- summary(fit)$coeff[1,"Estimate"]
#   beta1 <- summary(fit)$coeff[2,"Estimate"]
#   tau0 <- exp(-beta0/beta1)
#   p_val <- summary(fit)$coeff[2,4]
#   R_sq <- summary(fit)$r.squared
#   R_sq_adj <- summary(fit)$adj.r.squared
#   
#   
#   if (is.na(beta1)){
#     print(paste0("ACHTUNG we have a problem at id = ", my_id)) 
#     print(summary(fit))
#     break
#   } 
#   
#   row <- data.frame(my_id, beta0, beta1, tau0, R_sq, R_sq_adj, p_val)
#   
#   print(row)
#     
#   df_fit <- rbind(df_fit, row)
#     
# }
# 
# colnames(df_fit) <- c("id", "beta0", "beta1", "tau0","R_sq", "R_sq_adj", "p_val")
# 
# dim(df_fit)
# df_fit %>% formattable()
# 
# # END OLD 
