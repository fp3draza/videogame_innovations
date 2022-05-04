require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations/code')
data <- read.csv('../speedrun_data_clean.csv', row.names = 1)
source("helper.R")

ids <- filter(data, run_date_in_seconds > 1000)  %>% distinct(id)  #%>% filter(., run_time_percentage < 90)
ids1 <- filter(data, run_date_in_seconds > 3600) %>% filter(., run_time_percentage < 25) %>% distinct(id) 
mydata1 <- filter(data, id %in% ids1$id)

dim(ids)
dim(distinct(data,id))
dim(data)


rows_per_id <- data %>% count(id) %>% filter(n >10) 

dim(rows_per_id)

rows_per_id %>% formattable()
ids <- filter(ids, !(id %in% ids1$id))

# mydata <- filter(data, id %in% sample(ids$id, replace= TRUE, 10))

mydata <- filter(data, id %in% sample(rows_per_id$id, replace= TRUE, 4))

mydata %>% distinct(id)

# ggplot(mydata, aes(x = run_date_in_seconds, y = log(run_time))) + geom_line(aes(group = id), size = 0.07)  + 
#   geom_point(size = 0.5) + 
#   theme_minimal() + xlab('days since first record') + ylab('time first record') + 
#   theme(aspect.ratio = 1) 
#   #scale_y_continuous(trans='log') +
  #ylim(-2, 0.1) # + xlim(0, 1)


my_id <- "3dxkz0v19kv83mjdgame-level" # "yo1yv1q54xk906k0game-level" #   


ggplot(filter(mydata,id == "3dxkz0v19kv83mjdgame-level"), aes(x = run_date_in_seconds, y = log(run_time_percentage/100))) + 
  geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) power_law(x,-2e-1,0.2), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  theme(aspect.ratio = 1) 

ggplot(filter(mydata,id == "yo1yv1q54xk906k0game-level"), aes(x = run_date_in_seconds, y = log(run_time_percentage/100))) + geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) power_law(x,-4e-5,1.2), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  theme(aspect.ratio = 1) 


# Data linearization 

my_id <- "3dxkz0v19kv83mjdgame-level" # "yo1yv1q54xk906k0game-level" #   

df2 <- mydata %>% select(id, run_date_in_seconds, run_time_percentage) 
  
df1 <- df2 %>% mutate(log_improvement=log(run_time_percentage/100)) %>%
  filter(log_improvement<0) %>% 
  mutate(log_log_improvement=log(-log_improvement),
         log_time = log(run_date_in_seconds))

ids <- sample(df1$id, replace= TRUE, 15)

df_fit <- NULL

for (my_id in ids) {

  d <-df1 %>% filter(id == my_id) %>% select(id, log_time, log_log_improvement)
  
  fit <- lm(log_log_improvement ~ log_time, data = d) # fit the model
  d$predicted <- predict(fit)   # Save the predicted values
  d$residuals <- residuals(fit) # Save the residual values

  beta0 <- summary(fit)$coeff[1,"Estimate"]
  beta1 <- summary(fit)$coeff[2,"Estimate"]
  p_val <- summary(fit)$coeff[2,4]
  R_sq <- summary(fit)$r.squared
  R_sq_adj <- summary(fit)$adj.r.squared
  
  row <- data.frame(my_id, beta0, beta1, R_sq, R_sq_adj, p_val)
    
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


ggplot(d,aes(x = log_time, y = log_log_improvement)) + 
  #geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) straight_line(x, beta0, beta1), color ="black", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('log-log percentage of time first record') + 
  theme(aspect.ratio = 1) 







