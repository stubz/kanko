# 
setwd("/Users/okada/myWork/kaggle/datasciencelab/kanko")
library(dplyr);library(data.table);library(readr)
library(ggplot2)
library(reshape2)
training <- read_csv("target_train.csv")
location <- read_tsv("target_location.tsv")
category <- read_csv("category.csv")
prefecture_master <- read_tsv("prefecture_master.tsv", col_names=FALSE)
dim(training)

## toyama, kanazawa
filter(location, grepl("富山|金沢", area))
# JIS code : 16201, 17201
select(training, date, 16201_total, 17201_total)
select(training, date)

training_norm <- melt(training, id.var=c("date"), value.name="visit")
training_kotsu <- filter(training_norm, grepl("16201|17201", variable))
training_toyama_pref <- filter(training_kotsu, grepl("16201_[0-9]{2}", variable))
training_kanazawa_pref <- filter(training_kotsu, grepl("17201_[0-9]{2}", variable))
# top 5 prefectures for Kanazawa
top5_pref_kanazawa<-training_kanazawa_pref %>% group_by(variable) %>% summarise(total_visit=sum(visit)) %>% 
  arrange(desc(total_visit)) %>% head(5) %>% select(variable)

p1 <- ggplot(filter(training_kanazawa_pref, variable %in% as.character(top5_pref_kanazawa$variable)),
             aes(date, visit, colour=variable)) +
  geom_line()
print(p1)

#
top5_pref_toyama<-training_toyama_pref %>% group_by(variable) %>% summarise(total_visit=sum(visit)) %>% 
  arrange(desc(total_visit)) %>% head(5) %>% select(variable)
p2 <- ggplot(filter(training_toyama_pref, variable %in% as.character(top5_pref_toyama$variable)),
             aes(date, visit, colour=variable)) +
  geom_line()
print(p2)
# Kanazawa seems to have an increase in the number of visit from Tokyo, Saitama, Chiaba and Kanagawa.
# Such a level shift does not look very distinctive for Toyama.

#######################################################################################
## Our plan is to build a simple model for the aggregated data for Kanazawa and Toyama respectively.
## The first model will consider 
## treand + weekday + holiday 

# add week number
training_norm$wday <- as.POSIXlt(training_norm$date)$wday
library(rstan)
toyama <- filter(training_norm, variable=="16201_total")
T <- nrow(toyama)
data <- list(T=T, Y=toyama$visit)
#rstan_options(auto_write=TRUE)
#options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file='model_trend_season.stan')
fit1 <- sampling(
  stanmodel, data=data, pars=c('s_mu','s_s','s_y'),
  iter=10200, warmup=200, thin=10, chains=3,
  seed=123
)





colnames(training_kotsu) <- c("date", "toyama", "kanazawa")
p<-ggplot(training_kotsu, aes(date, toyama))+geom_line()

training_chiiki<- training[, c(1,5,6,9)] #14382:hakone, 14384:yugawara, 22205:atami
training_inbound<-training[, c(1,16:29)]

training_kotsu <- training[, c(1, grep("16201|17201", colnames(training)))]

?grep