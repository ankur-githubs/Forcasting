getwd()
setwd("E:/R_class/5. Time Series Forecasting/0. Case Study - Sales Forecasting - Class Exercise/Time Series in R")
mydata <- read.csv("TS.csv")
myts <- ts(mydata$Sales,start = c(2008,1),end = c(2011,4),frequency = 4)
#by the use ets model 
require(cluster)
require(forecast)
fit <- ets(myts)
accuracy(fit)
forecast(fit,h=4)
plot(forecast(fit,h=8))
#------------------------------------------------------------------
#use Arima model p,d,q
#-------------------------------
# 1. cal d :
require(tseries)
adf.test(myts,k=0)
# look t-val 
adf.test(diff(myts,differences = 1),k = 0)
plot(diff(myts,differences = 1))
# 2. look for ACF And PACF for acf-q val ,pacf - pval
acf(diff(myts,differences = 1)) # 1
pacf(diff(myts,differences = 1)) # 3

fit <- arima(myts,order = c(3,1,1))
summary(fit)
forecast(fit,h=8)
plot(forecast(fit,h=8))
#-------------------------------------------------------
# use auto arima
fit1 <- auto.arima(myts)
accuracy(fit1)
forecast(fit1,h=8)
plot(forecast(fit1,h=8))
