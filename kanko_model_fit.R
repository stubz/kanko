setwd("/Users/okada/myWork/kaggle/datasciencelab/kanko")
library(dplyr);library(data.table);library(readr)
library(ggplot2)
library(reshape2)
load("training_norm.RData")
load("date_master.RData")
## benchmark by forecast
library(forecast)

date_master1 <- filter(date_master, date>=as.Date("2014-06-01"), date<=as.Date("2015-05-31"))
date_masterf <- filter(date_master, date>=as.Date("2015-06-01"), date<=as.Date("2015-11-30"))
## the training data cover from 2014-06-01 to 2015-05-31
## we need to forecast through 2015-11-30 (i.e. 182-ahead forecast )
dat <- filter(training_norm, variable=="16201_total")
dim(dat);head(dat);tail(dat)
# http://robjhyndman.com/hyndsight/dailydata/
# y <- msts(dat$visit, seasonal.periods=c(7, 365)) # does not work
# fit <- tbats(y)
y <- ts(dat$visit, frequency = 7)
z <- fourier(ts(dat$visit, frequency=365.25), K=3)
zf <- fourierf(ts(dat$visit, frequency=365.25), K=3, h=183)
regt <- with(date_master1, cbind(before_holiday, D1, D2, holiday, saturday, sunday, friday, shinkansen))
regf <- with(date_masterf, cbind(before_holiday, D1, D2, holiday, saturday, sunday, friday, shinkansen))
minaic<-10^10
minK<-2
fc_list <- list()
for(i in 2:12){
  z <- fourier(ts(dat$visit, frequency=365.25), K=i)
  zf <- fourierf(ts(dat$visit, frequency=365.25), K=i, h=183)  
  fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=TRUE)
  if(AIC(fit)<minaic){
    minK <- i
    minaic <- AIC(fit)
  }
  print(paste(i, AIC(fit)))
  fc_list[[i]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=TRUE)
}
fc_best <- fc_list[[minK]]
fc_best$mean




dat <- filter(training_norm, variable=="04100_total")
dim(dat);head(dat);tail(dat)
# http://robjhyndman.com/hyndsight/dailydata/
# y <- msts(dat$visit, seasonal.periods=c(7, 365)) # does not work
# fit <- tbats(y)
y <- ts(dat$visit, frequency = 7)
z <- fourier(y, K=5)
zf <- fourierf(y, K=5, h=183)
fit <- auto.arima(y, xreg=cbind(z), seasonal=TRUE)
fc <- forecast(fit, xreg=cbind(z), h=183)
plot(fc)


y <- msts(dat$visit, seasonal.periods=c(7,365.25))
z <- fourier(y, K=c(3,5))
zf <- fourierf(y, K=c(3,5), h=183)
fit <- auto.arima(y, xreg=cbind(z), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(z), h=182)
plot(fc)

###################################################################################################
## simple bats model with frequency 7
i<-1
a<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_total", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- ts(dat$visit, frequency=7)
  fit <- bats(y)
  fc <- forecast(fit, h=183)
  a[[i]] <- fc$mean
}
b<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_inbound", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- ts(dat$visit, frequency=7)
  fit <- bats(y)
  fc <- forecast(fit, h=183)
  b[[i]] <- fc$mean
}
res1 <- cbind(data.frame(date=date_masterf$date), sapply(a, cbind), sapply(b, cbind))
write.table(res1, file="bats_benchmark.csv", row.names=FALSE, col.names=FALSE, sep=",")
###################################################################################################

## auto.arima
regt <- with(date_master1, cbind(before_holiday, D1, D2, holiday, monday, tuesday, wednesday, thursday, friday, saturday, shinkansen))
regf <- with(date_masterf, cbind(before_holiday, D1, D2, holiday, monday, tuesday, wednesday, thursday, friday, saturday, shinkansen))

i<-1
a<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_total", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- ts(dat$visit, frequency=7)
  minaic<-10^10
  minK<-2
  fc_list <- list()
  for(k in 2:12){
    z <- fourier(y, K=k)
    zf <- fourierf(y, K=k, h=183)  
    fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=FALSE)
    if(AIC(fit)<minaic){
      minK <- k
      minaic <- AIC(fit)
    }
    print(paste(k, AIC(fit)))
    fc_list[[k]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=FALSE)
  }
  fc_best <- fc_list[[minK]]
  #fit <- bats(y)
  #fc <- forecast(fit, h=183)
  a[[i]] <- fc_best$mean
}
b<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_inbound", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- ts(dat$visit, frequency=7)
  minaic<-10^10
  minK<-2
  fc_list <- list()
  for(k in 2:12){
    z <- fourier(y, K=k)
    zf <- fourierf(y, K=k, h=183)  
    fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=FALSE)
    if(AIC(fit)<minaic){
      minK <- k
      minaic <- AIC(fit)
    }
    print(paste(k, AIC(fit)))
    fc_list[[k]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=FALSE)
  }
  fc_best <- fc_list[[minK]]
  #fit <- bats(y)
  #fc <- forecast(fit, h=183)
  b[[i]] <- fc_best$mean
}
res2 <- cbind(data.frame(date=date_masterf$date), sapply(a, cbind), sapply(b, cbind))
write.table(res2, file="arima_benchmark.csv", row.names=FALSE, col.names=FALSE, sep=",")

###################################################################################################

## auto.arima with msts
regt <- with(date_master1, cbind(before_holiday, D1, D2, holiday, monday, tuesday, wednesday, thursday, friday, saturday, shinkansen))
regf <- with(date_masterf, cbind(before_holiday, D1, D2, holiday, monday, tuesday, wednesday, thursday, friday, saturday, shinkansen))

i<-1
a<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_total", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- msts(dat$visit, seasonal.periods=c(7,365.25))
  minaic<-10^10
  minK<-2
  fc_list <- list()
  for(k in 2:12){
    z <- fourier(y, K=c(2,k))
    zf <- fourierf(y, K=c(2,k), h=183)  
    fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=FALSE)
    if(AIC(fit)<minaic){
      minK <- k
      minaic <- AIC(fit)
    }
    print(paste(k, AIC(fit)))
    fc_list[[k]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=FALSE)
  }
  fc_best <- fc_list[[minK]]
  #fit <- bats(y)
  #fc <- forecast(fit, h=183)
  a[[i]] <- fc_best$mean
}
b<-list()
for(i in seq(along=location$code)){
  varnm <- paste(location$code[i], "_inbound", sep="")
  dat <- filter(training_norm, variable==varnm)
  y <- msts(dat$visit, seasonal.periods=c(7,365.25))
  minaic<-10^10
  minK<-2
  fc_list <- list()
  for(k in 2:12){
    z <- fourier(y, K=c(2,k))
    zf <- fourierf(y, K=c(2,k), h=183)  
    fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=FALSE)
    if(AIC(fit)<minaic){
      minK <- k
      minaic <- AIC(fit)
    }
    print(paste(k, AIC(fit)))
    fc_list[[k]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=FALSE)
  }
  fc_best <- fc_list[[minK]]
  #fit <- bats(y)
  #fc <- forecast(fit, h=183)
  b[[i]] <- fc_best$mean
}
res3 <- cbind(data.frame(date=date_masterf$date), sapply(a, cbind), sapply(b, cbind))
write.table(res3, file="arima_msts_benchmark.csv", row.names=FALSE, col.names=FALSE, sep=",")
########################################################################################

y <- ts(dat$visit, frequency = 7)
z <- fourier(ts(dat$visit, frequency=365.25), K=3)
zf <- fourierf(ts(dat$visit, frequency=365.25), K=3, h=183)
minaic<-10^10
minK<-2
fc_list <- list()
for(i in 2:12){
  z <- fourier(ts(dat$visit, frequency=365.25), K=i)
  zf <- fourierf(ts(dat$visit, frequency=365.25), K=i, h=183)  
  fit <- auto.arima(y, xreg=cbind(z, regt), seasonal=TRUE)
  if(AIC(fit)<minaic){
    minK <- i
    minaic <- AIC(fit)
  }
  print(paste(i, AIC(fit)))
  fc_list[[i]] <- forecast(fit, xreg=cbind(zf, regf), seasonal=TRUE)
}
fc_best <- fc_list[[minK]]

