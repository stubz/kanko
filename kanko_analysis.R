# 
setwd("/Users/okada/myWork/kaggle/datasciencelab/kanko")
library(dplyr);library(data.table);library(readr)
library(ggplot2)
library(reshape2)
training <- read_csv("target_train.csv")
# JIS code 
location <- read_tsv("target_location.tsv")
# number of items in each city by category (e.g. event, hotels, shopping)
category <- read_csv("category.csv")
category_master <- read_tsv("category_master.tsv")
geo_location_test <- read_csv("geo_location_test.csv")
geo_location_train <- read_csv("geo_location_train.csv")
# weather data by sensor
sensor_location <- read_tsv("sensor_location.tsv")
sensor_test <- read_csv("sensor_test.csv")
sensor_train <- read_csv("sensor_train.csv")
# number of posts about the city (e.g. twitter, blog, sns)
sns_test <- read_csv("sns_test.csv")
sns_train <- read_csv("sns_train.csv")
# weather data by the ministry
weather_locaton <- read_tsv("weather_location.tsv")
weather_test <- read_csv("weather_test.csv")
weather_train <- read_csv("weather_train.csv")
# exchange rate
ex_test <- read_csv("exchange_test.csv")
ex_train <- read_csv("exchange_train.csv")

prefecture_master <- read_tsv("prefecture_master.tsv", col_names=FALSE)
dim(training)
# holidays (self-made)
date_master <- data.frame(date=seq(as.Date("2013-01-01"), as.Date("2016-12-31"), 1))
holidays <- read_csv("holidays.csv")
holidays$holiday <- 1
date_master <- merge(date_master, holidays, by.x="date", by.y="date", all.x=TRUE)
date_master$holiday[is.na(date_master$holiday)] <- 0
# Add 1/2, 1/3 and 12/31 as holidays
date_master$holiday[date_master$date %in% c(
  as.Date("2013-01-02"),
  as.Date("2013-01-03"),
  as.Date("2013-12-31"),
  as.Date("2014-01-02"),
  as.Date("2014-01-03"),
  as.Date("2014-12-31"),
  as.Date("2015-01-02"),
  as.Date("2015-01-03"),
  as.Date("2015-12-31"),
  as.Date("2016-01-02"),
  as.Date("2016-01-03"),
  as.Date("2016-12-31"))] <- 1

date_master$period_name_mth <- strftime(date_master$date, "%Y-%m")
date_master$period_name_week<- strftime(date_master$date, "%Y-%U")
# flag to indicate if the next day is a holiday
date_master$before_holiday <- c(date_master$holiday[-1], 1)
date_master$wday <- as.POSIXlt(date_master$date)$wday
date_master <- date_master %>% mutate(D1=ifelse(wday>=1 & wday<=6 & holiday==1, 1, 0),
                                      D2=ifelse(wday>=1 & wday<=5 & before_holiday==1, 1, 0)) 
date_master %>% filter(D1==1) %>% select(date, D1, D2) %>% head(10)
date_master %>% filter(D2==1) %>% select(date, D1, D2)%>% head(10)
date_master$saturday <- ifelse(date_master$wday==6, 1, 0)
date_master$sunday   <- ifelse(date_master$wday==0, 1, 0)
date_master$friday   <- ifelse(date_master$wday==5, 1, 0)
date_master$monday   <- ifelse(date_master$wday==1, 1, 0)
date_master$tuesday   <- ifelse(date_master$wday==2, 1, 0)
date_master$wednesday   <- ifelse(date_master$wday==3, 1, 0)
date_master$thursday   <- ifelse(date_master$wday==4, 1, 0)
date_master$shinkansen <- ifelse(date_master$date>=as.Date("2015-03-14"), 1, 0)
save(date_master, file="date_master.RData")
######################################################################
## data manipulation 
######################################################################
training_norm <- melt(training, id.var=c("date"), value.name="visit")
training_norm$jis <- substr(training_norm$variable, 1, 5)
training_norm <- merge(training_norm, location, by.x="jis", by.y="code", all.x = TRUE)
training_norm <- merge(training_norm, date_master, by.x="date", by.y="date", all.x = TRUE)
# week effect
# http://heartruptcy.blog.fc2.com/blog-entry-90.html
# D1: 1 if holidays on weekdays, 0 otherwise
# D2: 1 if non-holiday between Mon-Thu, and the next day is a holiday. 0 otherwise
training_norm <- training_norm %>% mutate(D1=ifelse(wday>=1 & wday<=6 & holiday==1, 1, 0),
                         D2=ifelse(wday>=1 & wday<=5 & before_holiday==1, 1, 0)) 
save(training_norm, file="training_norm.RData")
load("training_norm.RData")
######################################################################
## visualisation
######################################################################
plot_stay <- function(dat, var1){
  dat1 <- filter(dat, variable==var1)
  p <- ggplot(dat1, aes(date, visit)) + geom_line() + 
    geom_vline(xintercept=as.numeric(as.Date("2015-03-14")), linetype="longdash", colour="blue")
  print(p)  
}
plot_stay(training_norm, "16201_total")
plot_stay(training_norm, "17201_total")

plot_stay(training_norm, "01202_total") # hakodate
plot_stay(training_norm, "04100_total") # sendai
plot_stay(training_norm, "13102_total") # tokyo
plot_stay(training_norm, "14382_total") # hakone
plot_stay(training_norm, "14384_total") # yugawara
plot_stay(training_norm, "22205_total") # atami
plot_stay(training_norm, "24203_total") # ise
plot_stay(training_norm, "26100_total") # kyoto
plot_stay(training_norm, "32203_total") # izumo
plot_stay(training_norm, "34100_total") # hiroshima
plot_stay(training_norm, "42201_total") # nagasaki
plot_stay(training_norm, "47207_total") # ishigaki

training_norm %>% filter(variable %in% paste(location$code, "_total", sep="")) %>% 
  group_by(area) %>% 
  summarise(total_stay=sum(visit)) %>% 
  arrange(desc(total_stay))
#
#1      京都府京都市   17348847
#2    神奈川県箱根町    7842064
#3      宮城県仙台市    7615789
#4      広島県広島市    5843986
#5      東京都中央区    5435195
#6      静岡県熱海市    5092010
#7      北海道函館市    4206405
#8      石川県金沢市    3043957
#9      長崎県長崎市    2593003
#10     沖縄県石垣市    2222869
#11     三重県伊勢市    1386409
#12     富山県富山市    1141943
#13 神奈川県湯河原町     815496
#14     島根県出雲市     286154

## toyama & Kanazawa

toyama <- filter(training_norm, variable=="16201_total")
kanazawa <- filter(training_norm, variable=="17201_total")
toyama_mth <- toyama %>% group_by(period_name_mth) %>% 
  summarise(visit=sum(visit))
kanazawa <- filter(training_norm, variable=="17201_total")
kanazawa_mth <- kanazawa %>% group_by(period_name_mth) %>% 
  summarise(visit=sum(visit))

ggplot(toyama_mth, aes(x=period_name_mth, y=visit, group=1)) + geom_line() +
  geom_vline(xintercept=10, linetype=2)
ggplot(kanazawa_mth, aes(x=period_name_mth, y=visit, group=1)) + geom_line() +
  geom_vline(xintercept=10, linetype=2)

ggplot(subset(kanazawa,date>="2015-02-01"&date<="2015-03-31"), 
       aes(x=date, y=visit, group=1)) + 
  geom_line()
ggplot(subset(toyama,date>="2015-02-01"&date<="2015-03-31"), 
       aes(x=date, y=visit, group=1)) + 
  geom_line()

toyama %>% filter(date>="2015-02-01"&date<="2015-03-31") %>% select(date, wday, visit)
# 2015/2/24(Tue) hit the highest 10744, another peak on 2015/2/17 at 7714, and 6485 on 2/18.
training_norm %>% filter(jis==16201 & date == "2015-02-24") %>% 
  select(date, variable, visit, area, wday) %>% arrange(variable)
# 16201_15(Niigata) and 16201_20(Nagano) have 1514 visits, 16201_10(Gunma) has 1278.
# Most are 中年層 (7006)
training_norm %>% filter(jis==16201 & date == "2015-02-17") %>% 
  select(date, variable, visit, area, wday) %>% arrange(variable)
# 13(tokyo), 14(kanagawa) have 1380 and 1047 respectively.
training_norm %>% filter(jis==16201 & date == "2015-02-18") %>% 
  select(date, variable, visit, area, wday) %>% arrange(variable)


toyama_week <- toyama %>% group_by(period_name_week) %>% 
  summarise(visit=sum(visit)) %>% arrange(period_name_week)
kanazawa_week <- kanazawa %>% group_by(period_name_week) %>% 
  summarise(visit=sum(visit)) %>% arrange(period_name_week)
ggplot(toyama_week, aes(x=period_name_week, y=visit, group=1)) + geom_line() +
  geom_vline(xintercept=43, linetype=2)
ggplot(kanazawa_week, aes(x=period_name_week, y=visit, group=1)) + geom_line() +
  geom_vline(xintercept=43, linetype=2)




dat <- filter(training_norm, variable=="17201_total")
p <- ggplot(dat, aes(date, visit)) + geom_line() + 
  geom_vline(xintercept=as.numeric(as.Date("2015-03-14")), linetype="longdash", colour="blue")
print(p)


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
training_norm <- melt(training, id.var=c("date"), value.name="visit")
training_norm$wday <- as.POSIXlt(training_norm$date)$wday
library(rstan)
toyama <- filter(training_norm, variable=="16201_total")
T <- nrow(toyama)
data <- list(T=T, Y=toyama$visit, D1=toyama$D1, D2=toyama$D2, wday=toyama$wday,
             shinkansen=toyama$shinkansen)
#rstan_options(auto_write=TRUE)
#options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file='model_trend_season.stan')
fit1 <- sampling(
  stanmodel, data=data,
  iter=102000, warmup=2000, thin=1, chains=1,seed=123
)
#plot(fit1)
traceplot(fit1, pars=c("s_ar","s_mu","s_s", "s_r"))
print(fit1, digits=3)



colnames(training_kotsu) <- c("date", "toyama", "kanazawa")
p<-ggplot(training_kotsu, aes(date, toyama))+geom_line()

training_chiiki<- training[, c(1,5,6,9)] #14382:hakone, 14384:yugawara, 22205:atami
training_inbound<-training[, c(1,16:29)]

training_kotsu <- training[, c(1, grep("16201|17201", colnames(training)))]

?grep