getwd()
setwd("E:/R_class/5. Time Series Forecasting/0. Case Study - Sales Forecasting - Class Exercise/Time Series in R")
mydata <- read.csv("TS.csv")
myts <- ts(mydata$Sales,start = c(2008,1),end = c(2011,4),frequency = 4)
#check data have sesonality and trend:-
plot(myts)
#here we decompose the data 
fit <- decompose(myts,type ="multiplicative")
plot(fit)
#or here we use stl
fit <- stl(myts,s.window ="period")
plot(fit)
#NOW decompose we forcast the data 
fit
accuracy(fit$time.series,myts)
forecast(fit,h = 4)
#cal sma
sm <- ma(myts,order = 4 , centre = T)
plot(sm,col="red")
#Exponetiol Model 
fit1 <- HoltWinters(myts,beta = F,gamma = F)
accuracy(fit1$fitted,myts)
#double exponential
fit2 <- HoltWinters(myts,beta = F)
accuracy(fit2$fitted,myts)
plot(forecast(fit2,h=8))

#triple exponential
Fit3 <- HoltWinters(myts)
accuracy(Fit3$fitted,myts)
plot(forecast(Fit3,h=8))
