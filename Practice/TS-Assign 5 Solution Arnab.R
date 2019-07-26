data(beersales)
#1A
train <- window(beersales, start=c(1975,1), end=c(1989,12))
trainXts <- as.xts(train)
model <- auto.arima(train, seasonal=T)
directForecasts <- forecast(model, h=12)
directForecasts

#1B
train <- window(beersales, start=c(1975,1), end=c(1989,12))
trainXts <- as.xts(train)
for (month in 1:12) {
  model <- auto.arima(train, seasonal=T)
  estimate <- forecast(model, h=1)
  estimate$mean
  trainXts <- rbind(trainXts, estimate$mean)
  train <- ts(trainXts, frequency=12, start=c(1975,1)) 
}
train
beer<-as.xts(beersales)
plot(trainXts,type="l")
lines(beer,type="l", col="red")
plot(trainXts[168:192],type="l")
lines(beer[168:192],type="l", col="red")
#1C
actuals <- window(beersales, start=c(1990,1), end=c(1990,12))
estimate1A <- directForecasts$mean
estimate1B <- window(train, start=c(1990,1), end=c(1990,12))
rmse1A <- mean((actuals - estimate1A)^2);rmse1A  #.565
rmse1B <- mean((actuals - estimate1B)^2);rmse1B  #.568

#2
train <- window(beersales, start=c(1975,1), end=c(1989,12))
seasonalModel <- auto.arima(train)
seasonalForecasts <- forecast(seasonalModel, h=12)

#3
estimate2 <- seasonalForecasts$mean
rmse2 <- mean((actuals - estimate2)^2) #.5650021

