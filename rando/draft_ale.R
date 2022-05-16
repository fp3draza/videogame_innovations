require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)

setwd('~/videogame_innovations/')
data <- read.csv('./clean_data/speedrun_data_clean.csv', row.names = 1)
source("./code/analyse_data/helper.R")

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



# NON LINEAR FIT 

#my_id <- "268erm76vdoxgl9dgame-level" # bad old fit
#my_id <- "369p9l1l7kjzj3d3game-level"	# good old fitting 

my_id <- "w6jnv51j9kvpqo0kgame-level"

filter(df_fit, id==my_id)


## without conditional linearity

d <-my_data %>% filter(id == my_id) %>% 
  mutate(run_time_improvement = run_time_percentage/100) %>%
  select(run_date_in_days, run_time_improvement)

dim(d)
(id == my_id) & 
  
tmp <-  data %>% filter(((run_date_in_days == 1) & (run_time_percentage != 100))) %>%
  select(id, run_date_in_days, run_time_percentage)
dim(tmp)

source("./code/analyse_data/functions_fit.R")

# initial guess of predictors
I_inf<- min(d$run_time_improvement)
lambda <- -log(I_inf)/max(d$run_date_in_days)
beta <- 1.
iter_max <- 4000

df_conv <- stretched_exp_fit(d, lambda, beta, I_inf, iter_max) %>% 
  mutate(id=my_id)

df_conv %>%  formattable()


#summary(fitStrExp)$coeff

# ggplot(df_conv,aes(x = iter, y = R_sq)) +
#   geom_line( size = 0.07,color="blue") +
#   geom_point(size = 0.5, color="blue") +
#   theme_minimal() + xlab('iter') + ylab('y') +
#   theme(aspect.ratio = 1)


lambda1 <- exp(filter(df_fit, id==my_id)$beta0)#  summary(fitStrExp)$coeff[1,"Estimate"] # exp(filter(df_fit, id==my_id)$beta0)
beta1 <- filter(df_fit, id==my_id)$beta1 #  1. #summary(fitStrExp)$coeff[2,"Estimate"] #filter(df_fit, id==my_id)$beta1
#I_inf <- 0


filter(df_fit, id == "w6jnv51j9kvpqo0kgame-level") %>% select(id, I_inf, R_sq_adj, convergence)

my_id <- "268ex4o6w20ylqjkgame-level"
tmp <- filter(df_fit, id == my_id) # tmp <- filter(df_conv, id == my_id)
lambda <- tmp$lambda
beta <- tmp$beta
I_inf <- tmp$I_inf


ggplot(filter(my_data,id == my_id), aes(x = run_date_in_days, y = run_time_percentage/100)) + geom_line(aes(group = id), size = 0.07,color="blue") + 
  geom_point(size = 0.5, color="blue") + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta,I_inf), color ="purple", linetype = "solid") +
#  stat_function(fun = function(x) stretched_exp(x,lambda1,beta1,0.), color ="blue", linetype = "dotted") +
  theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') + 
  ggtitle(paste0("fit id = ",my_id)) +
  theme(aspect.ratio = 1) #+   xlim(c(1000,1100)) 



# coef(fitStrExp)
# confint(fitStrExp)
# deviance(fitStrExp)
# df.residual(fitStrExp)
# fitted(fitStrExp)
# formula(fitStrExp)
# logLik(fitStrExp)
# predict(fitStrExp)

my_breaks <- seq(1,11000,100)

df <- NULL
for(my_cat in unique(df_res$beta_cat)){
  tau <- filter(df_res,(R_sq_cat >=7) & (beta_cat == my_cat))$tau0 
  h_tau <- hist(tau, freq = TRUE, breaks = my_breaks) 
  df <- rbind(df, data.frame(h_tau$breaks[-length(h_tau$breaks)], h_tau$counts/max(h_tau$counts), my_cat))
}

colnames(df) <- c("tau_intervals", "norm_counts", "beta_cat")

distinct(df, beta_cat) 
head(df)

ggplot(filter(df, norm_counts > 0.), aes(x = tau_intervals, y = norm_counts, col = as.factor(beta_cat))) + 
  geom_point(size = 1.2) + 
  stat_function(fun = function(x) power_law(x,9.5,-0.75), color ="red", linetype = "solid") +
  #stat_function(fun = function(x) power_law(x,50,-0.5), color ="blue", linetype = "solid") +
  theme_minimal() + #ylab('tau0') # + xlab('beta') +
  theme(aspect.ratio = 1) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
