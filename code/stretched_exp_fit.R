require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations/code')
data <- read.csv('../speedrun_data_clean.csv', row.names = 1)
source("helper.R")

# complete dataset
dim(data)

# choose time series with more than 10 points  
rows_per_id <- data %>% count(id) %>% filter(n >10) 
dim(rows_per_id)
rows_per_id %>% formattable()

# restrict our dataset to the selected ids
my_data <- filter(data, id %in% rows_per_id$id)

distinct(my_data, id)

# Data linearization 

#my_id <- "3dxkz0v19kv83mjdgame-level" # "yo1yv1q54xk906k0game-level" #   

df2 <- my_data %>% select(id, run_date_in_seconds, run_time_percentage) 
df1 <- df2 %>% mutate(log_improvement=log(run_time_percentage/100)) %>%
  filter(log_improvement<0) %>% 
  mutate(log_log_improvement=log(-log_improvement),
         log_time = log(run_date_in_seconds))

#ids <- sample(df1$id, replace= TRUE, 15)


match("9d3rvzwdmke9evjdgame-level",rows_per_id$id) # has only run_date_in_seconds = 1 
match("9d3rvzwd7kj3q1x2game-level",rows_per_id$id)


filter(df1, id %in% rows_per_id[48:53,]$id)

# remove problematic data sets
ids <- filter(rows_per_id, !(id %in% c("9d3rvzwdmke9evjdgame-level","9d3rvzwd7kj3q1x2game-level")))$id

match("9d3rvzwd7kj3q1x2game-level",ids)


df_fit <- NULL

for (my_id in ids) {

  d <-df1 %>% filter(id == my_id) %>% select(id, log_time, log_log_improvement)
  
  fit <- lm(log_log_improvement ~ log_time, data = d) # fit the model
  d$predicted <- predict(fit)   # Save the predicted values
  d$residuals <- residuals(fit) # Save the residual values

  
  print(summary(fit)$coeff[2,"Estimate"])
  
  
  beta0 <- summary(fit)$coeff[1,"Estimate"]
  beta1 <- summary(fit)$coeff[2,"Estimate"]
  p_val <- summary(fit)$coeff[2,4]
  R_sq <- summary(fit)$r.squared
  R_sq_adj <- summary(fit)$adj.r.squared
  
  
  if (is.na(beta1)){
    print(paste0("ACHTUNG we have a problem at id = ", my_id)) 
    print(summary(fit))
    break
  } 
  
  row <- data.frame(my_id, beta0, beta1, R_sq, R_sq_adj, p_val)
  
  print(row)
    
  df_fit <- rbind(df_fit, row)
    
}

colnames(df_fit) <- c("id", "beta0", "beta1", "R_sq", "R_sq_adj", "p_val")


# visualize the fit
ggplot(d, aes(x = log_time, y = log_log_improvement)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "lightgrey") +     # regression line
  geom_segment(aes(xend = log_time, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # color of the points mapped to residual size - green smaller, red larger
  guides(scale = "none") +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

my_id <- "9do8jge1jdzlomr2game-level"

ggplot(filter(df1, id==my_id),aes(x = log_time, y = log_log_improvement)) + 
  #geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) straight_line(x, beta0, beta1), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('log-log percentage of time first record') + 
  theme(aspect.ratio = 1) 







