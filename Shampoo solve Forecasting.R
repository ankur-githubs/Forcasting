getwd()
setwd("E:\\R_class\\5. Time Series Forecasting\\2. Case Study - Shampo Sales Forecasting")
mydata <- read.csv("shampoo-sales.csv")
myts <- ts(mydata$sales,start = c(2001,1),end = c(2003,12),frequency = 12)
#observ sesonal and trend
plot(myts)
plot(mydata)
#decompose
fit <- decompose(myts,type = c("multiplicative"))
fit

fit <- stl(myts,s.window = "period")
fit

require(forecast)
forecast(fit,h = 12+6)
plot(forecast(fit,h = 12))
#by the holtwinter
fit <- HoltWinters(myts,beta = F, gamma = F)
accuracy(fit$fitted,myts)
fit1 <- HoltWinters(myts,beta = F)
accuracy(fit1$fitted,myts)
fit2 <- HoltWinters(myts)
accuracy(fit2$fitted,myts)
fit3 <- ets(log(myts))
accuracy(fit3$fitted,log(myts))

plot(forecast(fit2,h = 12))
#-------------------------------------------------------------------------------------------
fit <- auto.arima(ma(myts))
accuracy(fit)

ts <- ma(myts,order = 4)
fit <- auto.arima(ts)
accuracy(fit)

