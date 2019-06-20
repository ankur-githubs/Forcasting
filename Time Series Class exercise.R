# save a numeric vector containing 48 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
getwd()

setwd("E:/R_class/5. Time Series Forecasting/0. Case Study - Sales Forecasting - Class Exercise/Time Series in R")
tsData <- read.table("TS.csv", header=TRUE, sep=",")
View(tsData)

myts   <- ts(tsData$Sales, start=c(2008, 1), end=c(2011, 4), frequency=4) 

?ts()

# plot series
plot(tsData)
plot(myts)

# Seasonal Decomposition

#decompose() #both additive & multiplicative decomposition- for understanding only
#stl()  #loess -to calculate trend, multiplicative decomposition, for understanding as well as forecasting


#A time series with additive trend, seasonal, and irregular components can be decomposed using the stl() function. 
#Note that a series with multiplicative effects can often by transformed into series with additive effects through a 
#log transformation (i.e., newts <- log(myts))
fit<-decompose(myts, type = c("multiplicative"))
fit
?decompose()
plot(fit)
fit

fit <- stl(myts, s.window="period")
?stl()
plot(fit)
fit
ls(fit)
print(fit$time.series)
fit$win

#install.packages("forecast")
require(forecast)
forecast(fit, h=4)

#-------------------simple moving average

sm <- ma(myts, order=4, centre = T) # 4 quarters moving average

?ma()
plot(myts)
plot(sm, col="red") # plot

plot(fit$trend)

#############################################################

#Exponential Models
#Both the HoltWinters() function in the base installation, and the ets() function in the forecast package, can be used to fit exponential models
#Forecast package
# simple exponential - models level
fit1 <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
ls(fit1)
require(forecast)
accuracy(fit1$fitted, myts)

# double exponential - models level and trend
fit2 <- HoltWinters(myts, beta=FALSE)
accuracy(fit2$fitted, myts)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(myts)
accuracy(fit3$fitted, myts)

forecast(fit3, h=4)

fit<-ets(myts)
accuracy(fit$fitted, myts)
forecast(fit, h=4)

# predictive accuracy
library(forecast)
accuracy(fit)

fit<-ets(myts)
accuracy(fit$fitted, myts)
summary(fit)

# predict next three future values
library(forecast)
forecast(fit, 4)
plot(forecast(fit, 4))


#ARIMA

#The arima() function can be used to fit an autoregressive integrated moving averages model. Other useful functions include:

#lag(ts, k) : lagged version of time series, shifted back k observations
#diff(ts, differences=d)	: difference the time series d times
#ndiffs(ts)	: Number of differences required to achieve stationarity (from the forecast package)
#acf(ts)	: autocorrelation function
#pacf(ts)	: partial autocorrelation function
#adf.test(ts)	: Augemented Dickey-Fuller test. Rejecting the null hypothesis suggests that a time series is stationary (from the tseries package)
#Box.test(x, type="Ljung-Box")	: Pormanteau test that observations in vector or time series x are independent

#Note that the forecast package has somewhat nicer versions of acf() and pacf() called Acf() and Pacf() respectively.

?adf.test
myts
require(tseries)
#adf.test(myts)
#myts1=diff(myts,differences = 2)
#adf.test(myts1)
#myts1
#?ndiffs
#ndiffs(myts, alpha=0.01,test=c('adf'))
acf(myts)
pacf(myts)

pacf(diff(myts,differences = 2))

adf.test(myts, k=0)
ndiffs(myts)

diff(myts,differences = 2)

test = adf.test(diff(myts,differences = 1), k=0)


acf(diff(myts,differences = 1))
pacf(diff(myts,differences = 1))


# fit an ARIMA model of order P, D, Q
fit <- arima(myts, order=c(3, 1, 1))
summary(fit)

?auto.arima

fit <-auto.arima(myts)
ls(fit)
fit$model
fit$series
summary(fit)
             
# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 4 observations
library(forecast)
forecast(fit, h=4)
plot(forecast(fit, 4))

fit = arima(myts, order=c(3,1,1))
summary(fit)
forecast(fit, h=8)
plot(forecast(fit, h=8))
fit= arima(myts, order = c(2,1,1), seasonal=list(order(1,1,2),period=4),
      xreg = X)

#Automated Forecasting
#The forecast package provides functions for the automatic selection of exponential and ARIMA models. 
#The ets() function supports both additive and multiplicative models. 
#The auto.arima() function can handle both seasonal and nonseasonal ARIMA models. Models are chosen to maximize one of several fit criteria.

library(forecast)
# Automated forecasting using an exponential model
fit <- ets(myts)
ls(fit)
summary(fit)
accuracy(fit$fitted, myts)
forecast(fit,4)

# Automated forecasting using an ARIMA model
fit <- auto.arima(myts)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 4 observations
library(forecast)
forecast(fit, 4)
plot(forecast(fit, 4))

