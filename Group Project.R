---
title: "Time Series Group Project"
author: "Ann Eitrheim, Alvin Haryanto, Dan Tallarico"
output: rmarkdown::github_document
---

https://www.kaggle.com/c/web-traffic-time-series-forecasting/data

Each of these time series represent a number of daily views of a different Wikipedia article. The page/article names contain the Wikipedia project (e.g. en.wikipedia.org), type of access/traffic (e.g. desktop) and type of agent (e.g. spid). In other words, each article name has the following format: 'name_project_access_agent' (e.g. 'AKB48_zh.wikipedia.org_all-access_spid'). Unfortunately, the data source for this dataset does not distinguish between traffic values of zero and missing values. A missing value may mean the traffic was zero or that the data is not available for that day.
```{r}
train_2 <- read.csv('train_2.csv') #The second stage will use training data up until September 1st, 2017.
```


```{r, echo = FALSE}
#train_1 <- read.csv('train_1.csv') #Views starting from July, 1st, 2015 up until December 31st, 2016.
#key_1 <- read.csv('key_1.csv') #gives the mapping between the page names and the shortened Id column used for prediction
#key_2 <- read.csv('key_2.csv')
#sample_submission_1 <- read.csv('sample_submission_1.csv') #The leaderboard during the training stage is based on traffic from January, 1st, 2017 up until March 1st, 2017.
#sample_submission_2 <- read.csv('sample_submission_2.csv') #The final ranking of the competition will be based on predictions of daily views between September 13th, 2017 and November 13th, 2017 for each article in the dataset. 
```


```{r}
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

train <- train_2 #[complete.cases(train_2), ]
tdates <- train %>% select(-Page) #df with no page names

#separating the mediawiki, wikimedia, and wikipedia data
foo <- train %>% select(Page) %>% rownames_to_column()
mediawiki <- foo %>% filter(str_detect(Page, "mediawiki"))
wikimedia <- foo %>% filter(str_detect(Page, "wikimedia"))
wikipedia <- foo %>% filter(str_detect(Page, "wikipedia")) %>% 
  filter(!str_detect(Page, "wikimedia")) %>% 
  filter(!str_detect(Page, "mediawiki"))

#separating the page name into topic, location, access, and agent
wikipedia <- wikipedia %>%
  separate(Page, into = c("foo", "bar"), sep = ".wikipedia.org_") %>%
  separate(foo, into = c("article", "locale"), sep = -3) %>%
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  mutate(locale = str_sub(locale,2,3))
wikimedia <- wikimedia %>%
  separate(Page, into = c("article", "bar"), sep = "_commons.wikimedia.org_") %>%
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  add_column(locale = "wikmed")
mediawiki <- mediawiki %>%
  separate(Page, into = c("article", "bar"), sep = "_www.mediawiki.org_") %>%
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  add_column(locale = "medwik")

#rejoining mediawiki, wikimedia, and wikipedia into one df
tpages <- wikipedia %>%
  full_join(wikimedia, by = c("rowname", "article", "locale", "access", "agent")) %>%
  full_join(mediawiki, by = c("rowname", "article", "locale", "access", "agent"))

sample_n(tpages, 5)
```


```{r, echo = FALSE}
#searching for pages
#tpages %>% filter(article == "Bill_Gates")
  #The_Last_Guardian
  #Barack_Obama
  #Bill_Gates
  #Bitcoin
  #Coca-Cola
  #Internet
  #Meghan_Markle
  #Meryl_Streep
  #Michelle_Obama
  #The_Beatles
  #Triathlon
  #WhatsApp
  #William_Shakespeare

#tpages %>%  group_by(article) %>% dplyr::summarise(count = n()) %>% filter(count > 9)
```
t(tdates[6279,773:800])
```{r}
bg.fr.desk <- ts(t(tdates[6279,]),frequency = 7)
bg.fr.mobl <- ts(t(tdates[53362,]), frequency = 7)
bg.fr.spid <- ts(t(tdates[129691,]), frequency = 7)
bg.fr.all <- bg.fr.desk + bg.fr.mobl + bg.fr.spid

bg.en.desk <- ts(t(tdates[11057,]), frequency = 7)
bg.en.mobl <- ts(t(tdates[73051,]), frequency = 7)
bg.en.spid <- ts(t(tdates[34899,]), frequency = 7)
bg.en.all <- bg.en.desk + bg.en.mobl + bg.en.spid

bg.de.desk <- ts(t(tdates[67313,]), frequency = 7)
bg.de.mobl <- ts(t(tdates[116365,]), frequency = 7)
bg.de.spid <- ts(t(tdates[48744,]), frequency = 7)
bg.de.all <- bg.de.desk + bg.de.mobl + bg.de.spid

bg.es.desk <- ts(t(tdates[70902,]), frequency = 7)
bg.es.mobl <- ts(t(tdates[95498,]), frequency = 7)
bg.es.spid <- ts(t(tdates[143106,]), frequency = 7)
bg.es.all <- bg.es.desk + bg.es.mobl + bg.es.spid

bg.all <- bg.en.all + bg.de.all + bg.fr.all + bg.es.all
```

```{r}
plot(bg.en.all, ylim = c(0,45000), main = "Bill Gates - English Wikipedia")
legend("topright", legend = c("All","Desktop", "Mobile", "Spider"), col = c(1,2,3,4), lty=1, cex=0.8,box.lty=0)
lines(bg.en.desk, col = 2)
lines(bg.en.mobl, col = 3)
lines(bg.en.spid, col = 4)

plot(bg.de.all, ylim = c(0,8000), main = "Bill Gates - German Wikipedia")
lines(bg.de.desk, col = 2)
lines(bg.de.mobl, col = 3)
lines(bg.de.spid, col = 4)

plot(bg.fr.all, ylim = c(0,27000), main = "Bill Gates - French Wikipedia")
lines(bg.fr.desk, col = 2)
lines(bg.fr.mobl, col = 3)
lines(bg.fr.spid, col = 4)

plot(bg.es.all, ylim = c(0,18000), main = "Bill Gates - Spanish Wikipedia")
lines(bg.es.desk, col = 2)
lines(bg.es.mobl, col = 3)
lines(bg.es.spid, col = 4)


plot(bg.all, ylim = c(0,72000), main = "Bill Gates - All Wikipedia")
legend("topright", legend = c("All","English", "German", "French", "Spanish"), col = c(1,2,3,4,5), lty=1, cex=0.8,
       box.lty=0)
lines(bg.en.all, col = 2)
lines(bg.de.all, col = 3)
lines(bg.fr.all, col = 4)
lines(bg.es.all, col = 5)
```

```{r, echo = FALSE}
#to help with plotting
plot_rownr <- function(rownr){
  art <- tpages %>% filter(rowname == rownr) %>% .$article
  loc <- tpages %>% filter(rowname == rownr) %>% .$locale
  acc <- tpages %>% filter(rowname == rownr) %>% .$access
  extract_ts(rownr) %>%
    ggplot(aes(dates, views)) +
    geom_line() +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = str_c(art, " - ", loc, " - ", acc))
}

plot_rownr_log <- function(rownr){
  art <- tpages %>% filter(rowname == rownr) %>% .$article
  loc <- tpages %>% filter(rowname == rownr) %>% .$locale
  acc <- tpages %>% filter(rowname == rownr) %>% .$access
  extract_ts_nrm(rownr) %>%
    ggplot(aes(dates, views)) +
    geom_line() +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = str_c(art, " - ", loc, " - ", acc)) +
    scale_y_log10() + labs(y = "log views")
}

plot_rownr_zoom <- function(rownr, start, end){
  art <- tpages %>% filter(rowname == rownr) %>% .$article
  loc <- tpages %>% filter(rowname == rownr) %>% .$locale
  acc <- tpages %>% filter(rowname == rownr) %>% .$access
  extract_ts(rownr) %>%
    filter(dates > ymd(start) & dates <= ymd(end)) %>%
    ggplot(aes(dates, views)) +
    geom_line() +
    #geom_smooth(method = "loess", color = "blue", span = 1/5) +
    #coord_cartesian(xlim = ymd(c(start,end))) +  
    labs(title = str_c(art, " - ", loc, " - ", acc))
}
```

```{r nonseasonal arima}
train <- window(bg.all, end = c(111,3))
test <- window(bg.all, start = c(111,4))

bg.auto.arima <- auto.arima(train, seasonal = F, stepwise = F)
bg.auto.arima #ARIMA(4,1,1)  #AIC=15,029.9
bg.auto.arima.forecast <- forecast(bg.auto.arima, h=30)
plot(bg.auto.arima.forecast)
plot(bg.auto.arima.forecast$residuals, main = 'Residuals of Nonseasonal Arima Model')
acf(bg.auto.arima.forecast$residuals, 50)
accuracy(bg.auto.arima.forecast, test)
```


```{r seasonal arima}
train <- window(bg.all, end = c(111,3))
test <- window(bg.all, start = c(111,4))

bg.auto.sarima <- auto.arima(train, stepwise = F)
bg.auto.sarima #ARIMA(1,1,1)(2,0,0)[7]  #AIC=15,003.24
bg.auto.sarima.forecast <- forecast(bg.auto.sarima, h=30)
plot(bg.auto.sarima.forecast)
plot(bg.auto.sarima.forecast$residuals, main = 'Residuals of Seasonal Arima Model')
acf(bg.auto.sarima.forecast$residuals, 50)
accuracy(bg.auto.sarima.forecast, test)

#the forecast is less volatile/tame
plot(test, main = 'Forecasted (blue) versus Actual (black)'); lines(bg.auto.sarima.forecast$mean,col=4)
```

```{r hierarchical}
library(forecast)
library("hts") 
#nodes <- list(4, c(3, 3, 3, 3))
#group into language, then into total
abc <- cbind(bg.en.desk, bg.en.mobl, bg.en.spid,
             bg.de.desk, bg.de.mobl, bg.de.spid,
             bg.fr.desk, bg.fr.mobl, bg.fr.spid,
             bg.es.desk, bg.es.mobl, bg.es.spid)

train <- hts(window(abc, end = c(111,3)), characters = c(6,4))
test <- hts(window(abc, start = c(111,4)), characters = c(6,4))

smatrix(train)
plot(train) #aggregates some of them in the middle, then all at the to
bg.hts.fcst <- forecast(train, method="bu", fmethod = "arima", h=30, keep.resid = T, weights = 'ols')
    #Forecasts are distributed in the hierarchy using bottom-up, top-down, middle-out, and optimal combination methods.
    #"comb", "bu", "mo", "tdgsa", "tdgsf", "tdfp"
plot(bg.hts.fcst)
plot(rowSums(bg.hts.fcst$residuals), main = 'Residuals of Hierarchical Arima Model', type = 'l')
acf(rowSums(bg.hts.fcst$residuals),50)
accuracy.gts(bg.hts.fcst, test, level = 0)
```

```{r arimax}
library(TSA)
train <- window(bg.all, end = c(111,3))
test <- window(bg.all, start = c(111,4))

which(bg.auto.sarima.forecast$residuals >= 11000)

pulse1 <- 1*(seq(train) == 39)
pulse2 <- 1*(seq(train) == 120)
pulse3 <- 1*(seq(train) == 155)
pulse4 <- 1*(seq(train) == 182)
pulse5 <- 1*(seq(train) == 218)
pulse6 <- 1*(seq(train) == 238)
pulse7 <- 1*(seq(train) == 362)
pulse8 <- 1*(seq(train) == 364)
pulse9 <- 1*(seq(train) == 486)
pulse10 <- 1*(seq(train) == 566)
pulse11 <- 1*(seq(train) == 614)
pulse12 <- 1*(seq(train) == 629)
pulse13 <- 1*(seq(train) == 758)

xtransf.dataframe = data.frame(pulse1, pulse2, pulse3, pulse4, pulse5, pulse6, pulse7,
                               pulse8, pulse9, pulse10, pulse11, pulse12, pulse13)
transfer.list = list(c(1,0), c(1,0), c(1,0), c(1,0), c(1,0), c(1,0), c(1,0),
                     c(1,0), c(1,0), c(1,0), c(1,0), c(1,0), c(1,0))

bg.pulse <- arimax(log(train),
                   order=c(1,1,1),
                   seasonal=list(order=c(2,0,0), period=7),
                   transfer=transfer.list,
                   xtransf=xtransf.dataframe,
                   method='ML')
#bg.pulse
tf1<-stats::filter(1*(seq(1:(length(train)+30))==39),filter=bg.pulse$coef[5], method='recursive',side=1) * (bg.pulse$coef[6])
tf2<-stats::filter(1*(seq(1:(length(train)+30))==120),filter=bg.pulse$coef[7], method='recursive',side=1) * (bg.pulse$coef[8])
tf3<-stats::filter(1*(seq(1:(length(train)+30))==155),filter=bg.pulse$coef[9], method='recursive',side=1) * (bg.pulse$coef[10])
tf4<-stats::filter(1*(seq(1:(length(train)+30))==182),filter=bg.pulse$coef[11], method='recursive',side=1) * (bg.pulse$coef[12])
tf5<-stats::filter(1*(seq(1:(length(train)+30))==218),filter=bg.pulse$coef[13], method='recursive',side=1) * (bg.pulse$coef[14])
tf6<-stats::filter(1*(seq(1:(length(train)+30))==238),filter=bg.pulse$coef[15], method='recursive',side=1) * (bg.pulse$coef[16])
tf7<-stats::filter(1*(seq(1:(length(train)+30))==362),filter=bg.pulse$coef[17], method='recursive',side=1) * (bg.pulse$coef[18])
tf8<-stats::filter(1*(seq(1:(length(train)+30))==364),filter=bg.pulse$coef[19], method='recursive',side=1) * (bg.pulse$coef[20])
tf9<-stats::filter(1*(seq(1:(length(train)+30))==486),filter=bg.pulse$coef[21], method='recursive',side=1) * (bg.pulse$coef[22])
tf10<-stats::filter(1*(seq(1:(length(train)+30))==566),filter=bg.pulse$coef[23], method='recursive',side=1) * (bg.pulse$coef[24])
tf11<-stats::filter(1*(seq(1:(length(train)+30))==614),filter=bg.pulse$coef[25], method='recursive',side=1) * (bg.pulse$coef[26])
tf12<-stats::filter(1*(seq(1:(length(train)+30))==629),filter=bg.pulse$coef[27], method='recursive',side=1) * (bg.pulse$coef[28])
tf13<-stats::filter(1*(seq(1:(length(train)+30))==758),filter=bg.pulse$coef[29], method='recursive',side=1) * (bg.pulse$coef[30])

tf <- tf1 + tf2 + tf3 + tf4 + tf5 + tf6 + tf7 + tf8 + tf9 + tf10 + tf11 + tf12 + tf13
plot(exp(tf), col = 4, main = "Effect of Pulses")

forecast.arima<-Arima(log(train), order=c(1,1,1),
                      seasonal = list(order=c(2,0,0), period=7),
                      xreg=tf[1:(length(tf)-30)])

bg.arimax.predict <- predict(forecast.arima, n.ahead = 30, newxreg=tf[774:length(tf)])
bg.arimax.predict$pred <- exp(bg.arimax.predict$pred)

plot(c(train,bg.arimax.predict$pred), col =2, type = 'l'); lines(ts(bg.all), col = 1)

plot(bg.arimax.predict$pred-test, main = 'Residuals of Seasonal Arima Model')
#MAE 
mean(abs(bg.arimax.predict$pred-test))
#MPE 
sum((test-bg.arimax.predict$pred)/test)*100/(length(test)-1)
#RMSE
sqrt(mean((bg.arimax.predict$pred-test)^2))

pulsesx <- c(39, 120, 155, 182, 218, 239, 362, 364, 486, 566, 614, 630, 759)
ts.plot(ts(c(train),frequency = 7), main = "Bill Gates Time Series with Pulses Highlighted"); points(pulsesx/7+.9,train[pulsesx],col = 'blue')

#like the seasonal arima, it doesn't have as extreme swings in the data
lines(bg.arimax.predict$pred,col=2)
```








