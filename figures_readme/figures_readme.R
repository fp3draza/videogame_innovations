library(ggplot2)
library(cowplot)
library(latex2exp)
require(dplyr)
source("./code/analyse_data/helper.R")


lambda <- 1
I_inf <- 0.3
b0 <- 0.5
b1 <- 1
b2 <- 2.5
b3 <- 15
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
p1 <- ggplot() + 
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


#
legend3 <- data.frame(
  x = 1.5,
  y = 0.9,
  label = paste0("beta = ", b3)
)

p2 <- ggplot() + 
  stat_function(fun = function(x) stretched_exp(x,lambda,b3,I_inf), color ="purple", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) {(1-I_inf)*exp(-1) + I_inf}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        legend.position="none")  +   
  ylim(c(0,1))  +   xlim(c(0,2))  + 
  geom_text(data=legend3, aes( x=x, y=y, label=label),                  
            color="purple", 
            size=3.5, angle=0) +
  xlab(TeX("$t/\\tau_0$")) +   ylab(TeX("$f(t)$"))  


# plot_grid(p1, p2)

p1 
p2 
#### COLLAPSE ALL 

setwd('~/videogame_innovations')
fit_data <- read.csv('./data/processed/fit_data.csv', row.names = 1)
df_res <- read.csv('./data/processed/fit_res.csv', row.names = 1)
df_res_filtered <- filter(df_res,(tau0_err/tau0 <0.4) & (R_sq_adj > 0.65))
df_tmp <- fit_data %>% left_join(., df_res_filtered, by = c("id" = "id")) 
#
legend <- data.frame(
  x = 4.5,
  y = 0.38,
  label = "1/e"
)
#
lambda <- 1
I_inf <- 0 
beta <- 1
#
my_ids <- (filter(df_tmp, tau0_err/tau0 <0.4) %>% distinct(id))$id
#
ggplot(filter(df_tmp,id %in% my_ids), aes(x = (run_date_in_days/tau0)**beta, y = normalized_decay(run_time_percentage, I_inf), col = as.factor(id))) + 
  geom_point(size = 0.5) + 
  stat_function(fun = function(x) stretched_exp(x,lambda,beta,I_inf), color ="black", linetype = "solid", size = 0.5) +
  stat_function(fun = function(x) {exp(-1)}, color ="grey", linetype = "dotted", size = 0.5) +
  theme_minimal() + 
  xlab(TeX("$(t/\\tau_0)^\\beta$")) + ylab('percentage of time first record') +
  theme(aspect.ratio = 1,legend.position="none")  +   
  ylim(c(0,1))  + xlim(c(0,5)) +   
  geom_text(data=legend, aes( x=x, y=y, label=label),
            color="black",
            size=3.5, angle=0) 

