setwd("/Users/okada/myWork/kaggle/datasciencelab/kanko")
library(dplyr);library(data.table);library(readr)
library(ggplot2)
library(reshape2)
load("training_norm.RData")
load("date_master.RData")

#######################################################################################
## Our plan is to build a simple model for the aggregated data for Kanazawa and Toyama respectively.
## The first model will consider 
## treand + weekday + holiday 

# add week number
# training_norm <- melt(training, id.var=c("date"), value.name="visit")
# training_norm$wday <- as.POSIXlt(training_norm$date)$wday
library(rstan)
toyama <- filter(training_norm, variable=="16201_total")
kanazawa <- filter(training_norm, variable=="17201_total")

T <- nrow(kanazawa)
data <- list(T=T, Y=kanazawa$visit, D1=kanazawa$D1, D2=kanazawa$D2, wday=kanazawa$wday,
             shinkansen=kanazawa$shinkansen)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file='model_trend_season.stan')
fit_kanazawa_stan1 <- sampling(
  stanmodel, data=data,
  iter=12000, warmup=2000, thin=1, chains=3,seed=123
)
traceplot(fit_kanazawa_stan1, pars=c("s_ar","s_mu","s_s", "s_r"))
print(fit_kanazawa_stan1, digits=3)
save(fit_kanazawa_stan1, file="fit_kanazawa_stan1.RData")

## Plot
## http://heartruptcy.blog.fc2.com/blog-entry-89.html
## check convergence and trace of MCMC samples
fit_kanazawa_stan1_summary <- data.frame(summary(fit_kanazawa_stan1)$summary)
la <- extract(fit_kanazawa_stan1, permuted=TRUE)
N.day <- nrow(kanazawa)
names(la)
N.mcmc <- length(la$s_mu)

library(ggplot2)
library(reshape2)
library(dplyr)

s_mu <- la$s_mu
s_s  <- la$s_s

mu_est <- data.frame(mcmc.sample=1:N.mcmc, la$mu)
colnames(mu_est) <- c('mcmc.sample', 1:N.day)
mu_melt <- melt(mu_est[1:1000,], id=c('mcmc.sample'), variable.name='day')
mu_melt$day <- as.integer(mu_melt$day)
mu_med <- data.frame(day=1:N.day, value=apply(la$mu, 2, median), mcmc.sample=as.integer(1))
ggplot(mu_melt, aes(x=day, y=value, group=mcmc.sample)) +
  geom_line(colour='darkorange3', alpha=1/100)
## The graph shows we fail to model around day 260 when Hokuriku-shinkansen launched.

s_est <- data.frame(mcmc.sample=1:N.mcmc, la$s)
colnames(s_est) <- c('mcmc.sample', 1:N.day)
s_melt <- melt(s_est[2000:3000,], id=c('mcmc.sample'), variable.name='day')
s_melt$day <- as.integer(s_melt$day)
s_med <- data.frame(day=1:N.day, value=apply(la$s, 2, median), mcmc.sample=as.integer(1))
ggplot(s_melt, aes(x=day, y=value, group=mcmc.sample)) +
  geom_line(colour='darkorange3', alpha=1/100)

## AR component
ar_est <- data.frame(mcmc.sample=1:N.mcmc, la$ar)
colnames(ar_est) <- c('mcmc.sample', 1:N.day)
ar_melt <- melt(ar_est[2000:3000,], id=c('mcmc.sample'), variable.name='day')
ar_melt$day <- as.integer(ar_melt$day)
ar_med <- data.frame(day=1:N.day, value=apply(la$ar, 2, median), mcmc.sample=as.integer(1))
ggplot(ar_melt, aes(x=day, y=value, group=mcmc.sample)) +
  geom_line(colour='darkorange3', alpha=1/100)
# it is not doing much help..

hist(la$b1)
hist(la$b2)  
hist(la$b3) # not quite converged.. 
hist(la$c_shinkansen) # bi-modal ...
## fitted values
kanazawa_fit <- data.frame(mu=mu_med$value, s=s_med$value, ar=ar_med$value, 
                           b1=median(la$b1), b2=median(la$b2), b3=median(la$b3),
                           c_shinkansen=median(la$c_shinkansen))
kanazawa_fit <- date_master




