library(readxl)
June16 <- read_excel("~/June16.xls")
June17 <- read_excel("~/June17.xls")
June18 <- read_excel("~/June18.xls")
June19 <- read_excel("~/June19.xls")
June20 <- read_excel("~/June20.xls")
June21 <- read_excel("~/June21.xls")
June22 <- read_excel("~/June22.xls")
June23 <- read_excel("~/June23.xls")
June24 <- read_excel("~/June24.xls")
June25 <- read_excel("~/June25.xls")
June26 <- read_excel("~/June26.xls")
June27 <- read_excel("~/June27.xls")
June28 <- read_excel("~/June28.xls")
June29 <- read_excel("~/June29.xls")
June30 <- read_excel("~/June30.xls")
July1 <- read_excel("~/July1.xls")

June_16<-June16[c(5:28),5]
June_17<-June17[c(5:28),5]
June_18<-June18[c(5:28),5]
June_19<-June19[c(5:28),5]
June_20<-June20[c(5:28),5]
June_21<-June21[c(5:28),5]
June_22<-June22[c(5:28),5]
June_23<-June23[c(5:28),5]
June_24<-June24[c(5:28),5]
June_25<-June25[c(5:28),5]
June_26<-June26[c(5:28),5]
June_27<-June27[c(5:28),5]
June_28<-June28[c(5:28),5]
June_29<-June29[c(5:28),5]
June_30<-June30[c(5:28),5]
July_1<-July1[c(5:28),5]

traffic<-rbind(June_16,June_17,June_18,June_19,June_20,June_21,June_22,June_23,June_24,June_25,June_26,June_27,June_28,June_29,June_30,July_1)
names(traffic)<-("Count")

############# PART 1 ##############################
par(mfrow=c(1,1))
traffic<-ts(traffic, start=1, frequency=24)
plot(traffic, type="o", main="Hourly Traffic", xlab="Day", ylab="Vehicle Count")
traffic1<-ts(traffic, start=1, frequency=1)
june1<-window(traffic1, end=c(15,24))
t1<-auto.arima(traffic1)

june<-window(traffic, end=c(15,24))
july<-window(traffic, start=c(16,1))
plot(diff(june), main="Traffic Pulse") #to show a pulse in TA session 8

library(forecast)
library(stats)
library(TSA)
t.a<-auto.arima(june, seasonal=F)  ;t.a  #(2,0,3) AICc=4455
t.h<-forecast(t.a, 24)

plot(forecast(t.a, 24), main="Hourly Traffic", xlab="Day", ylab="Vehicle Count")
lines(as.vector(time(july)), july,col="red")

t.a.max<-auto.arima(june, seasonal = FALSE, max.order = 20, max.p=10, max.q=10, stepwise=FALSE, approximation = FALSE, trace=TRUE) #(10,0,2) AICc=4425 NUTS
t.a.max
eacf(june)

max.p<-6
max.q<-6
aic.c<-matrix(NA,max.p,max.q)
bic<-matrix(NA,max.p,max.q)
for( p in seq_len(max.p)){
  for (q in seq_len(max.q)){
    model<-Arima(june, order=c(p,1,q))
    aic.c[p,q]<-model$aicc
    bic[p,q]<-model$bic
  }
}


data.frame(aic.c)
data.frame(bic)
min(aic.c)
which(aic.c==min(aic.c), arr.ind = T)
which(bic==min(bic), arr.ind = T)

std.A<-Arima(june, order=c(4,0,3));std.A #4409
std.B<-Arima(june, order=c(3,1,2));std.B #4441

################### PART 2 ###############################
## Day of the Week ##
trafficW<-ts(traffic, frequency = 168)
juneW<-window(trafficW, end=c(3,24))
t.w<-auto.arima(juneW, stepwise=FALSE,trace=TRUE)
t.w.A<-Arima(juneW, order=c(0,1,2), seasonal=c(0,1,0))
t.w.f<-forecast(t.w.A, 24) #ARIMA(0,1,2)(0,1,0)[168]
plot(t.w.f, main="Weekly Forecast", ylab="Vehicle Count", xlab="Weeks")
lines(as.vector(time(july)-1)/7+1, july,col="red")
plot(resid(t.w), main="Weekly Data")
Box.test(resid(t.w), lag = 14, type="Ljung-Box") #2m for seasonal data
forecast<-as.vector(t.w$mean)
actual<-as.vector(july)
h<-cbind(actual,forecast)
h<-as.data.frame(h)
h$diff<-(h$actual-h$forecast)

################# PART 3 ######################
hour<-c(8,9,17,18)  #Hourly
t.h<-auto.arima(june, stepwise=FALSE, approximation=FALSE)
t.h.f<-forecast(t.h,24) #(2,0,1)(2,1,0)
t.h.f$mean[hour]
error<-(july - as.vector(t.h.f$mean))[hour]#479.178066 294.134109  -5.076941  78.208794
sqrt(mean(error^2)) #283.8
plot(t.h.f, main="Daily Forecast")
lines(july, col="red")
plot(resid(t.h.f), main="Hourly Data")
Box.test(resid(t.h.f), lag = 10, type="Ljung-Box") #.8454
##See the seasonalilty

################## PART 4 #####################
hour<-c(8,9,17,18)  #Weekly
errorW<-(july - as.vector(t.w.f$mean))[hour]#  27.021445  29.021445 -54.978555 3.021445
sqrt(mean(errorW^2)) #33.93
plot(resid(t.w), main="Weekly Data")
Box.test(resid(t.w), lag = 10, type="Ljung-Box") #.573
pacf(resid(t.h),50)

a<-as.vector(july)[hour]
w<-as.vector(t.w.f$mean)[hour]
h<-as.vector(t.h.f$mean)[hour]

plot(a,type="l", ylim=c(750,1250))
lines(w, col="red")
lines(h, col="green")
  