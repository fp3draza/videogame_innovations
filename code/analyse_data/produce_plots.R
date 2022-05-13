require(formattable)
require(ggplot2)
require(dplyr)
require(cowplot)
require(igraph)
library(latex2exp)
library(scales)
source("./code/analyse_data/helper.R")

###################################################
# RAW DATA: record development per game 

ggplot(data, aes(x = run_date_in_days, y = run_time, col = as.factor(id))) + geom_line(aes(group = id), size = 0.02)  +
 theme_minimal() + xlab('days since first record') + ylab('Raw run time [seconds ?]') + 
 scale_y_continuous(trans='log10') +
 theme(aspect.ratio = 1, legend.position="none")

ggplot(data, aes(x = run_date_in_days, y = run_time_percentage/100, col = as.factor(id))) + geom_line(aes(group = id), size = 0.02)  +
 theme_minimal() + xlab('days since first record') + ylab('percentage of time first record') +
 theme(aspect.ratio = 1, legend.position="none")

###################################################
# HISTOGRAM DATA POINT PER GAME: why we do not split in training and test sample 

ggplot(filter(rows_per_id, n<30), aes(x=n)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(title = "data-points per game", y = "percentage of the dataset", x = "numebr of points")


###################################################
# THEORETICAL DECAYS 
lambda <- 1
I_inf <- 0.3
b0 <- 0.5
b1 <- 1
b2 <- 2.5
#
legend0 <- data.frame(
  x = 3,
  y = 0.7,
  label = paste0("beta = ", b0)
)
#
legend1 <- data.frame(
  x = 3,
  y = 0.8,
  label = paste0("beta = ", b1)
)
#
legend2 <- data.frame(
  x = 3,
  y = 0.9,
  label = paste0("beta = ", b2)
)

# beta dependence 
ggplot() + 
  stat_function(fun = function(x) stretched_exp(x,lambda,b0,I_inf), color ="blue", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,lambda,b1,I_inf), color ="black", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,lambda,b2,I_inf), color ="red", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) {(1-I_inf)*exp(-1) + I_inf}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  +   xlim(c(0,5))  + 
  geom_text(data=legend0, aes( x=x, y=y, label=label),                  
          color="blue", 
          size=3.5, angle=0) +
  geom_text(data=legend1, aes( x=x, y=y, label=label),                  
          color="black", 
          size=3.5, angle=0) +
  geom_text(data=legend2, aes( x=x, y=y, label=label),                  
          color="red", 
          size=3.5, angle=0) +
  ylab(TeX("$f(t)$")) + 
  xlab(TeX("$t/\\tau_0$"))


# tau0 dependence 
legend0 <- data.frame(
  x = 3,
  y = 0.8,
  label = paste0("beta = ", b0)
)

double_tau0_lambda0 <- 2**(-b0)
double_tau0_lambda2 <- 2**(-b2)

ggplot() + 
  stat_function(fun = function(x) stretched_exp(x,lambda,b0,I_inf), color ="blue", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,double_tau0_lambda0,b0,I_inf), color ="blue", linetype = "dashed", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,lambda,b2,I_inf), color ="red", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) stretched_exp(x,double_tau0_lambda2,b2,I_inf), color ="red", linetype = "dashed", size = 0.5) +
  stat_function(fun = function(x) {(1-I_inf)*exp(-1) + I_inf}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  + xlim(c(0,5))  + 
  geom_text(data=legend0, aes( x=x, y=y, label=label),
            color="blue",
            size=3.5, angle=0) +
  geom_text(data=legend2, aes( x=x, y=y, label=label),
            color="red",
            size=3.5, angle=0) +
  ylab(TeX("$f(t)$")) + 
  xlab(TeX("$t$"))



# ###################################################
# CONVERGENCE PLOTS for the NL fitting function 

df_res_filtered <- filter(df_res,(tau0_err/tau0 <0.4) & (R_sq_adj > 0.65))
dim(df_res)
dim(df_res_filtered)

colnames(df_res_filtered)
filter(df_res_filtered, id==my_id) %>% select(id,iter,R_sq,delta_R_sq)

# remove the break condition  in the fitting function and source it again 
source("./code/analyse_data/functions_fit.R")

my_id <- "w6jnv51j7dg5ypp2game-level"
iter_max <- 1000

d <-my_data %>% filter(id == my_id) %>% 
  mutate(run_time_improvement = run_time_percentage/100) %>%
  select(run_date_in_days, run_time_improvement)

# initial guess of predictors
I_inf<- min(d$run_time_improvement)
lambda <- -log(I_inf)/max(d$run_date_in_days)  
beta <- 1.

df <- stretched_exp_fit(d, lambda, beta, I_inf, iter_max) %>% mutate(id=my_id)
colnames(df)

ggplot(df, aes(x = iter, y = R_sq)) + 
  geom_point(size = 0.8) + 
  theme_minimal() + 
  ylab(TeX("$R^2$")) + xlab('iteration') +
  theme(aspect.ratio = 1)  


###################################################
# DATA COLLAPSE - per beta categories 

df_tmp <- my_data %>% left_join(., df_res_filtered, by = c("id" = "id"))   

#OLD
#my_ids0 <- (filter(df_tmp, (beta > 0) & (beta < 1) & (I_inf > 0.) & (I_inf < 0.1) & tau0_err/tau0 <0.4) %>% distinct(id))$id
#I0 <- mean(filter(df_tmp,id %in% my_ids1)$I_inf)

my_ids0 <- (filter(df_tmp, (beta > 0.) & (beta < 1.0) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta0 <- mean(filter(df_tmp,id %in% my_ids0)$beta)
#
my_ids1 <- (filter(df_tmp, (beta > 1) & (beta < 2) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta1 <- mean(filter(df_tmp,id %in% my_ids1)$beta)
#
my_ids2 <- (filter(df_tmp, (beta > 2) & (beta < 3) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta2 <- mean(filter(df_tmp,id %in% my_ids2)$beta)
#
my_ids3 <- (filter(df_tmp, (beta > 3) & tau0_err/tau0 <0.4) %>% distinct(id))$id
beta3 <- 9
#
legend <- data.frame(
  x = 4.5,
  y = 0.38,
  label = "1/e"
)
#
lambda <- 1
I_inf <- 0 #I0 # I0 # I1
beta <- beta2 #beta0 # beta0 # beta1
my_ids <- my_ids2 # my_ids0 # my_ids1

ggplot(filter(df_tmp,id %in% my_ids), aes(x = run_date_in_days/tau0, y = normalized_decay(run_time_percentage, I_inf), col = as.factor(id))) + 
  geom_point(size = 0.8) + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta,I_inf), color ="black", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) {exp(-1)}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  xlab(TeX("$t/\\tau_0$")) + ylab('percentage of time first record') +
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  + xlim(c(0,5)) +   
  geom_text(data=legend, aes( x=x, y=y, label=label),
            color="black",
            size=3.5, angle=0) +
#  ggtitle("collapse for beta > 3 ") 
 ggtitle("collapse for 2 < beta < 3")  

# plot a single curve
# my_id<- "29d30dlpnxd1rk8qgame-level" # beta =1.29
# my_id <- "v1p4e418rklernr2game-level" # beta = 0.68
# my_id <- "m1z9l2d0wkpmj40kgame-level" # beta = 2.64
# my_id <- "m1mgkj12zdn8l8edgame-level" # beta = 5.17
my_id <- "lde3r2l6ndx4nzo2game-level" # beta = 
tmp <- filter(df_fit, id %in% my_id) 
lambda <- tmp$lambda
beta <- tmp$beta
I_inf <- tmp$I_inf

#
legend_beta <- data.frame(
  x = 8, # 1000,
  y = 0.9,
  label = paste0("beta = ", round(beta,2))
)
#
legend_tau0 <- data.frame(
  x = 8, # 1000,
  y = 0.8,
  label = paste0("tau_0 = ", round(lambda**(-1/beta),0)," days")
)

ggplot(filter(df_tmp,id == my_id), aes(x = run_date_in_days, y = run_time_percentage/100)) + 
  stat_function(fun = function(x) stretched_exp(x,unique(lambda),unique(beta),unique(I_inf)), color ="purple", linetype = "solid") +
  stat_function(fun = function(x) {(1-I_inf)*exp(-1) + I_inf}, color ="grey", linetype = "dotted", size = 0.5) +
  geom_point(size = 0.8) + 
  theme_minimal() + 
  xlab('t [days]') + ylab('percentage of time first record') +
  geom_text(data=legend_beta, aes( x=x, y=y, label=label),
            color="black",
            size=3.5, angle=0) +
  geom_text(data=legend_tau0, aes( x=x, y=y, label=label),
            color="black",
            size=3.5, angle=0) +
  theme(aspect.ratio = 1) + ylim(c(0,1)) + xlim(c(0,12)) 

# ggtitle("collapse for 0 < beta < 1") 


###################################################
# HISTOGRAMS PREDICTORS VALUES
# beta
# counts 
ggplot(df_res_filtered, aes(x=beta)) +
  geom_histogram(freq = TRUE, breaks = seq(0,10,0.5), color="orange")
#geom_density(color="red") +  xlim(0,3.2)

# count curves falling in each beta category 
df_res_filtered %>% filter((beta >0) & (beta <1)) %>% select(id, tau0, tau0_err, beta, beta_err, R_sq_adj)

# beta_cat = 0 => 44 => 44/278 = 0.158
# beta_cat = 1 => 99 => 99/278 = 0.356
# beta_cat = 2 => 47 => 44/278 = 0.169
# beta_cat = 3 => 88 => 44/278 = 0.317

# tau0
dim(df_res_filtered)

ggplot(df_res_filtered, aes(x=tau0)) +  
  geom_histogram(freq = TRUE, breaks = seq(0,2500,30), color ="purple") +
# scale_x_continuous(trans='log10') +
 scale_y_continuous(trans='log10') +
  xlab(TeX("$tau_0$"))

# count curves falling in each tau0 category 
df_res_filtered %>% filter((tau0 >500) & (tau0 <1000)) %>% select(id, tau0, tau0_err, tau0_cat)
# total number of curves
dim(df_res_filtered)

# tau0_cat = 0 => 214 => 214/278 = 0.77
# tau0_cat = 1 => 35 => 35/278 = 0.126
# tau0_cat = 2 => 16 => 16/278 = 0.0576
# tau0_cat = 3 => 5 => 5/278 = 0.018 
# tau0_cat = 4 => 4 => 4/278 = 0.0144
# tau0_cat = -1 => 4 => 4/278 = 0.0144


# ###################################################
# # ATTEMPT TO FIT COUNTS of tau0 by beta categories and log-log
# my_breaks <- seq(0,2500,250)
# df <- NULL
# for(my_cat in unique(df_res_filtered$beta_cat)){
#   df_tmp <- filter(df_res_filtered, (tau0<2500) & beta_cat == my_cat) %>% 
#     select(id,R_sq_adj,beta,beta_err,tau0,tau0_err) %>% 
#     mutate(tau0_rel_err = tau0_err/tau0)
#   
#   h_tau <- hist(df_tmp$tau0, freq = TRUE, breaks = my_breaks) 
#   df <- rbind(df, data.frame(h_tau$breaks[-length(h_tau$breaks)], h_tau$counts/max(h_tau$counts), my_cat))
# }
# 
# df %>% formattable() 
# colnames(df) <- c("tau_intervals", "norm_counts", "beta_cat")
# 
# ggplot(filter(df, norm_counts > 0), aes(x = tau_intervals, y = norm_counts, col = as.factor(beta_cat))) + 
#   geom_point(size = 1.2) + 
# #  stat_function(fun = function(x) power_law(x,9.5,-0.75), color ="black", linetype = "solid") +
#   theme_minimal() + xlab('tau0') + ylab('normalized counts') +
#   theme(aspect.ratio = 1) +
#   ggtitle("histogram tau0 in log-log scale") + 
#  # ylim(0.1,1.2) +
#   scale_x_continuous(trans='log10') +
#   scale_y_continuous(trans='log10') #, limits=c(0.01,1.2)) 
