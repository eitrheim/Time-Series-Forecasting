#install.packages("forecast")
library("forecast")
library("fracdiff")
ts.2 <-  read_csv("C:/Users/Wklus/Dropbox/U of Chicago/Time Series/Time Series/ts 2.full.csv")
View(ts.2)

period<-ts.2$period
unemp<-ts.2$unemp
gdp<-ts.2$gdp

plot(period,unemp,xlab="period",ylab="unemp")
plot(period,gdp,xlab="period",ylab="gdp")

##ARIMA model
##Unemployment

un.arima<-auto.arima(unemp)
summary(un.arima)

##ARIMA model 
##GDP
gdp.arima<-auto.arima(gdp)
summary(gdp.arima)


##Forecast - Unemployment
fit <- Arima(unemp,c(1,0,1))
plot(forecast(fit))

fit <- arfima(unemp)
plot(forecast(fit,h=10))

predict(arima(unemp, order=c(1,0,1)),n.ahead = 12)
plot(od)


arima_model.un<-forecast.Arima(un.arima, h=4)
b<-data.frame(arima_model.un)
arima_model.un
plot(arima_model.un, xlab="period", ylab="Unemployment Forecast")
lines(ts.2.full$unemp) 


##Forecast - gdp
a<-data.frame(arima_model.gdp<-forecast.Arima(gdp.arima, h=4))
arima_model.gdp
plot(arima_model.gdp, xlab="period", ylab="GDP Forecast")
lines(ts.2.full$gdp) 

##Add predicted values to actual

##GDP
ts.2.test$pred<-c(a$Point.Forecast)  ##GDP predicted added to actual
ts.2.test$gdp.diff<-ts.2.test$pred-ts.2.test$gdp ##Take the difference
plot(ts.2.test$gdp.diff, type="l", xlab="Period", ylab="GDP forecast error")
ts.2.test$gdp.sq<-(ts.2.test$gdp.diff)^2
gdp.error<-sum(ts.2.test$gdp.sq)
gdp.error

##Unemployment
ts.2.test$pred.un<-c(b$Point.Forecast)  ##Unemployment predicted added to actual
ts.2.test$un.diff<-ts.2.test$pred.un-ts.2.test$unemp  ##Take the difference
plot(ts.2.test$un.diff, type="l", xlab="Period", ylab="Unemp forecast error")
ts.2.test$un.sq<-(ts.2.test$un.diff)^2
un.error<-sum(ts.2.test$un.sq)
un.error


##Regress GDP on Unemp

GDP<-lm(gdp~unemp)
summary(GDP)

un.new<-data.frame(unemp=ts.2.test$unemp)
p<-data.frame(gdp.pred<-predict(GDP, newdata=un.new, interval="confidence"))
plot(gdp.pred)

ts.2.test$pred.lm<-c(p$fit)  ##GDP predicted added to actual
ts.2.test$gdp.lm.diff<-ts.2.test$pred.lm-ts.2.test$gdp ##Take the difference
plot(ts.2.test$gdp.lm.diff, type="l", xlab="period", ylab="Error for GDP")
ts.2.test$gdp.lm.sq<-(ts.2.test$gdp.lm.diff)^2
gdp.lm.error<-sum(ts.2.test$gdp.lm.sq)
gdp.lm.error


##Regress Unemp on GDP

Unemp<-lm(unemp~gdp)
summary(Unemp)

gdp.new<-data.frame(gdp=ts.2.test$gdp)
q<-data.frame(un.pred<-predict(Unemp, newdata=gdp.new, interval="confidence"))
plot(un.pred)

ts.2.test$pred.un.lm<-c(q$fit)  ##un predicted added to actual
ts.2.test$un.lm.diff<-ts.2.test$pred.un.lm-ts.2.test$unemp ##Take the difference
plot(ts.2.test$un.lm.diff, type="l", xlab="period", ylab="Error for Unemployment")
ts.2.test$un.lm.sq<-(ts.2.test$un.lm.diff)^2
un.lm.error<-sum(ts.2.test$un.lm.sq)
un.lm.error


