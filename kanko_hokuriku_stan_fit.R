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

T <- nrow(toyama)
data <- list(T=T, Y=toyama$visit, D1=toyama$D1, D2=toyama$D2, wday=toyama$wday,
             shinkansen=toyama$shinkansen)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file='model_trend_season.stan')
fit_stan1 <- sampling(
  stanmodel, data=data,
  iter=102000, warmup=2000, thin=1, chains=1,seed=123
)
traceplot(fit_stan1, pars=c("s_ar","s_mu","s_s", "s_r"))
print(fit_stan1, digits=3)
save(fit_stan1, file="fit_stan1.RData")

## Plot
## http://heartruptcy.blog.fc2.com/blog-entry-89.html
## check convergence and trace of MCMC samples
print(fit_stan1, digits=3)
fit_stan1_summary <- data.frame(summary(fit_stan1)$summary)
la <- extract(fit_stan1, permuted=TRUE)
N.day <- nrow(toyama)
names(la)
N.mcmc <- length(la$s_mu)

library(ggplot2)
library(reshape2)
library(dplyr)

s_mu <- la$s_mu
s_s  <- la$s_s

mu_est <- data.frame(mcmc.sample=1:N.mcmc, la$mu)
colnames(mu_est) <- c('mcmc.sample', 1:N.day)
mu_melt <- melt(mu_est[20000:21000,], id=c('mcmc.sample'), variable.name='day')
mu_melt$day <- as.integer(mu_melt$day)
mu_med <- data.frame(day=1:N.day, value=apply(la$mu, 2, median), mcmc.sample=as.integer(1))
ggplot(mu_melt, aes(x=day, y=value, group=mcmc.sample)) +
  geom_line(colour='darkorange3', alpha=1/100)
## The graph shows we fail to model around day 260 when Hokuriku-shinkansen launched.

s_est <- data.frame(mcmc.sample=1:N.mcmc, la$s)
colnames(s_est) <- c('mcmc.sample', 1:N.day)
s_melt <- melt(s_est[20000:21000,], id=c('mcmc.sample'), variable.name='day')
s_melt$day <- as.integer(s_melt$day)
# s_med <- data.frame(day=1:N.day, value=apply(la$s, 2, median), mcmc.sample=as.integer(1))
ggplot(s_melt, aes(x=day, y=value, group=mcmc.sample)) +
  geom_line(colour='darkorange3', alpha=1/100)


s_ar <- la$s_ar
s_r  <- la$s_r
b1   <- la$b1
b2   <- la$b2
b3   <- la$b3
bs <- data.frame(b1 = la$b1, b2=la$b2, b3=la$b3)
sigmas <- data.frame(s_mu=la$s_mu, s_s=la$s_s, s_ar=la$s_ar, s_r=la$s_r) 
bs.melt <- melt(bs, id=c(), variable.name = 'param')
bs.qua.melt <- bs.melt %>% group_by(param) %>% 
  summarise(median=median(value), 
            ymax=quantile(value, prob=0.975), 
            ymin=quantile(value, prob=0.025))
colnames(bs.qua.melt)[2] <- 'value'

sigmas.melt <- melt(sigmas, id=c(), variable.name='param')
sigmas.qua.melt <- sigmas.melt %>% group_by(param) %>% 
  summarise(median=median(value),
            ymax=quantile(value, prob=0.975),
            ymin=quantile(value, prob=0.025))

bs.melt <- data.frame(bs.melt, ymax=rep(0, N.mcmc), ymin=rep(0, N.mcmc))
p <- ggplot(bs.melt, aes(x=param, y = value, group=param, ymax=ymax, ymin=ymin, color=param))
p <- p + geom_pointrange(data=bs.qua.melt, size=0.75)
# p <- p + geom_violin(trim=FALSE)  , fill="#5B423D", linetype="blank", alpha=1/3)

print(p)

# s_r has relatively large values, so plot them separately.
ggplot(subset(sigmas.qua.melt, param!='s_r'), aes(x=param, y=median, group=param, ymax=ymax, ymin=ymin, color=param)) +
  geom_pointrange(size=0.75)
ggplot(sigmas.melt, aes(value)) + geom_histogram(binwidth=1) +
  facet_wrap(~param) + 
  scale_x_continuous(limits=c(0,20))

hist(la$c_shinkansen)
summary(la$c_shinkansen)

