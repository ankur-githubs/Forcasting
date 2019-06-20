
#ARIMAX EXAMPLE
#TV advertising and insurance quotations
insurance<-read.csv("E:/0. HP Laptop/DSSR - BA Classes/5. Time Series Forecasting/4. Case Study - ARIMAX Example/insurance.csv")
insurance<-ts(data=insurance, start=c(2012,1), end=c(2015,4), frequency=12)

plot(insurance, main="Insurance advertising and quotations", xlab="Year")

insurance[,2]

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
View(Advert)
colnames(Advert) <- paste("AdLag",0:3,sep="")

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

summary(fit4)

# Best model fitted to all data (based on AICc)
# Refit using all data
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
?auto.arima()
fit
accuracy(fit)

fc8 <- forecast(fit, xreg=cbind(rep(8,4), c(Advert[40,1],rep(8,3))), h=4)

par(mfrow = c(1,1))
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")


