### Time Series Assignment 3 #############
### ARIMA model #######
library("forecast")

#Unemp_GDP_UK <-ts(read_excel("Unemp_GDP_UK.xlsx"))
#View(Unemp_GDP_UK)
UNGDP<-(Unemp_GDP_UK)
#UNGDP$Year<-rep(1955:1969, each=4)
unemp<-ts(Unemp_GDP_UK, start=1955, frequency = 4)

un<-unemp[,1]
gdp<-unemp[,2]
pre.un<-window(un,end=c(1968,4))
pre.gdp<-window(gdp, end=c(1968,4))

plot(unemp, main="UN and GDP")
plot(diff(pre.un))
plot(diff(pre.gdp))
un.arima<-auto.arima(pre.un); un.arima #(1,1,0)
gdp.arima<-auto.arima(pre.gdp); gdp.arima #(0,1,0) with drift

c(ndiffs(pre.un), ndiffs(pre.gdp)) #check for d differences 1 for both

un.forecast<-forecast(un.arima,h=4)
gdp.forecast<-forecast(gdp.arima, h=4)

un.fc<-(un.forecast$mean)
un.actual<-(window(un, start=c(1969,1)))
d<-cbind(un.fc,un.actual)
d<-data.frame(d)
d$un.error<-d$un.fc - d$un.actual

gdp.fc<-(gdp.forecast$mean)
gdp.actual<-(window(gdp, start=c(1969,1)))
c<-cbind(gdp.fc,gdp.actual)
c<-data.frame(c)
c$gdp.error<-c$gdp.fc - c$gdp.actual

par(mfrow=c(1,2))
plot(d$un.error, type="b", main="UN Forecast Error")
plot(c$gdp.error, type="b", main="GDP Forecast Error")

c(sum(d$un.error^2), sum(c$gdp.error^2))  #UN-2718 (s/b 1537) GDP-5.86

############Linear Regression#############
gdp.lm<-lm(pre.gdp~pre.un)
summary(gdp.lm)

un.lm<-lm(pre.un ~ pre.gdp)
summary(un.lm)

gdp.error<-((predict(gdp.lm, data.frame(pre.un=window(un, start=c(1969,1))))-window(gdp,start=c(1969,1)))/window(gdp,start=c(1969,1)))*100

mape.gdp<-mean(abs(predict(gdp.lm, data.frame(pre.un=window(un, start=c(1969,1))))-window(gdp,start=c(1969,1)))/window(gdp,start=c(1969,1)))

un.error<-((predict(un.lm, data.frame(pre.gdp=window(gdp, start=c(1969,1))))-window(un,start=c(1969,1)))/window(un,start=c(1969,1)))*100

mape.un<-mean(abs(predict(un.lm, data.frame(pre.gdp=window(gdp, start=c(1969,1))))-window(un,start=c(1969,1)))/window(un,start=c(1969,1)))

par(mfrow=c(1,2))
plot(gdp.error, main="GDP Errors", type="b")
plot(un.error, main="UN Errors", type="b")


c(sum(un.error^2),sum(gdp.error)^2)  #UN-16,681, GDP-528








