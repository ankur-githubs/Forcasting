getwd()
setwd("E:\\R_class\\5. Time Series Forecasting\\final case study project")
library(readxl)
data <- read_excel("E:/R_class/5. Time Series Forecasting/final case study project/data.xlsx")
data <- ts(data$Total,start = c(1996,1),end = c(2005,4),frequency = 4)
plot(data)# we data havetrend and sesonality
#we decompose data for 
fit <- decompose(data,type = c("multiplicative"))
plot(fit)
ls(fit)
# plot(fit$figure)#figure
# plot(fit$seasonal)#seasonal
# plot(fit$trend)#trend
# plot(fit$type)#type
# plot(fit$x)#x
# plot(fit$random)#random
fit <- stl(data,s.window = "period")
plot(fit)
ls(fit)
plot(fit$win)
require(forecast)
forecast(fit,h = 4)
sm = ma(data,order = 4,centre = T)
plot(sm,col="red")
forecast(fit,h=2)
#-----------------------------------------------------------------
#Exponential meathod for use HOLTWINTER
fit1 <- HoltWinters(data,beta =F,gamma = F)
accuracy(fit1$fitted,data)
#ACF1 = -0.02855096
#Mape = 18.10978 < not less then 10 %
#so we do double exponential 
fit2 = HoltWinters(data,beta = F)
accuracy(fit2$fitted,data)
#ACF1 = -0.1043916
#MAPE =  3.483573
fit3 = HoltWinters(data)
accuracy(fit3$fitted,data)
#ACF1 <- 0.002905161
#MAPE <- 2.917488
#select which fit give best accuracy 
plot(forecast(fit2,h = 4))
plot(forecast(fit3,h = 8))
forecast(fit2,h = 8)
#here we manualy select best fit but for automated we use 
#ecnometric timeseries model 
fit4 <- ets(data)
accuracy(fit4$fitted,data)
forecast(fit4,h = 2)
plot(forecast(fit4,h=8))
#--------------------------------------------------------------------------------------------------------------------------
# NOW WE APPROCH WITH ARIMA
#first we know series is stationary or not
require(tseries)
adf.test(data,k = 0)
plot(data)
adf.test(diff(data,differences = 1),k=0)
plot(diff(data,differences = 1))
acf(diff(data,differences = 1)) # q = 1

pacf(diff(data,differences = 1)) # p = 3
adf.test(data,k = 0) # d = 1

fit5 <- auto.arima(data)
summary(fit5)
library(forecast)
forecast(fit5,h = 4)
plot(forecast(fit5,h = 8))



