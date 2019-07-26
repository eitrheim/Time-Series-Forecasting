#Arnabs version of assignment 6

furnace<- data.frame(Gas.Furnace.Dataset <- read.csv("~/Gas Furnace Dataset.csv"))
#data <- read.csv("Project 4.csv")
gas <- furnace$Input
co2 <- furnace$Outlet
model.ols <- lm(co2 ~ gas)
plot(gas, co2, pch=20, col="blue", main="Gas Furnace Data", xlab="Input Gas Rate", ylab="Output CO2 %")
abline(model.ols, col="orange")
residual.ols <- residuals(model.ols)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(residual.ols, main="Residuals of OLS", xlab="Time Index", ylab="Residual")
acf(residual.ols, main="ACF")
pacf(residual.ols, main="PACF")

library(lmtest)
dwtest(model.ols, alternative="two.sided")  #.13023
Box.test(residual.ols, lag=20, type="Ljung-Box", fitdf=2) #x2=576.22

############################################
x<-1:100
e<-arima.sim(model=list(ar=.3, ma=.9),n=100)
y<-1+2*x+e
fit1<-lm(y~x)
summary(fit1)
plot(fit1)
acf(fit1$residuals,lag=100)
par(mfrow=c(1,2))
qqnorm(fit1$residuals)
qqline(fit1$residuals)

fit2<-auto.arima(y,xreg=x)
summary(fit2)
qqnorm(fit2$residuals)
qqline(fit2$residuals)
acf(fit2$residuals, lag=100)
fc.fit2 <- forecast(fit2, xreg=x,h=24)
plot(fc.fit2)

fit3<-Arima(y,order=c(5,1,1))
summary(fit3)
qqnorm(fit3$residuals)
qqline(fit3$residuals)
acf(fit3$residuals, lag=100)

#Forecasting
(USAccDeaths)
fit <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths, 5), seasonal=FALSE)
fc <- forecast(fit, xreg=fourier(USAccDeaths, 5, 24))
plot(fc)






