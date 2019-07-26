##Assignment 7 Arnab's Solution##
library(xts)
library(stats)
cme <- read_csv("cmeS.csv", col_types = cols(DateOfSale = col_date(format = "%m/%d/%Y")))
imm <- read_csv("immS.csv", col_types = cols(DateOfSale = col_date(format = "%m/%d/%Y")))
iom <- read_csv("iomS.csv", col_types = cols(DateOfSale = col_date(format = "%m/%d/%Y")))
#View(iomS)
#View(immS)
#View(cmeS)


#CME Seat Monthly Price
spline.monthly.avg <- function(x) {
  x.spline <- smooth.spline(index(x), x)
  temp <- predict(x.spline, min(index(x)):max(index(x)))
  x.daily <- xts(temp$y, order.by=as.Date(temp$x))
  return(apply.monthly(x.daily, function(x) mean(x)))
}

imm.xts <- xts(cme$price, order.by=as.Date(cme$DateOfSale, "%m/%d/%Y"))

plot(index(cme.xts), cme.xts, pch=20, col="darkgrey",
     main="CME Seat Price + (Possibly Splined) Monthly Average", xlab="Date",
     ylab="Seat Price")

cme.raw.avg <- apply.monthly(cme.xts, function(x) mean(x)) #daily average
cme.spline.avg <- round(spline.monthly.avg(cme.xts), 0)

cme.monthly <- numeric(length(cme.spline.avg))
t = 1
for (y in "2001":"2013") {
  for (m in seq_len(12)) {
    raw <- cme.raw.avg[paste0(y,m)]
    cme.monthly[t] <- ifelse(length(raw) == 1, raw, cme.spline.avg[paste0(y,m)])
    t <- t + 1
  }
}
cme.monthly <- xts(cme.monthly, order.by=seq(as.Date("2001-01-01"),
                                             as.Date("2013-12-01"), by="month"))
points(index(cme.monthly), cme.monthly, pch=20, col="blue")

#############################################################
#IMM Seat Monthly Price

imm.xts <- xts(imm$price, order.by=as.Date(imm$DateOfSale, "%m/%d/%Y"))
plot(index(imm.xts), imm.xts, pch=20, col="darkgrey",
     main="IMM Seat Price + (Possibly Splined) Monthly Average", xlab="Date",
     ylab="Seat Price")

imm.raw.avg <- apply.monthly(imm.xts, function(x) mean(x))
imm.spline.avg <- round(spline.monthly.avg(imm.xts), 0)

imm.monthly <- numeric(length(imm.spline.avg))
t = 1
for (y in "2001":"2013") {
  for (m in seq_len(12)) {
    raw <- imm.raw.avg[paste0(y,m)]
    imm.monthly[t] <- ifelse(length(raw) == 1, raw, imm.spline.avg[paste0(y,m)])
    t <- t + 1
  }
}
imm.monthly <- xts(imm.monthly, order.by=seq(as.Date("2001-01-01"),
                                             as.Date("2013-12-01"), by="month"))
points(index(imm.monthly), imm.monthly, pch=20, col="blue")

#IOM Seat Monthly Price

iom.xts <- xts(iom$price, order.by=as.Date(iom$DateOfSale, "%m/%d/%Y"))
plot(index(iom.xts), iom.xts, pch=20, col="darkgrey",
     main="IOM Seat Price + (Possibly Splined) Monthly Average", xlab="Date",
     ylab="Seat Price")

iom.raw.avg <- apply.monthly(iom.xts, function(x) mean(x))
iom.spline.avg <- round(spline.monthly.avg(iom.xts), 0)

iom.monthly <- numeric(length(iom.spline.avg))
t = 1
for (y in "2001":"2013") {
  for (m in seq_len(12)) {
    raw <- iom.raw.avg[paste0(y,m)]
    iom.monthly[t] <- ifelse(length(raw) == 1, raw, iom.spline.avg[paste0(y,m)])
    t <- t + 1
  }
}
iom.monthly <- xts(iom.monthly, order.by=seq(as.Date("2001-01-01"),
                                             as.Date("2013-12-01"), by="month"))
points(index(iom.monthly), iom.monthly, pch=20, col="blue")

write.csv(data.frame(Date=index(cme.monthly), CME=cme.monthly, IMM=imm.monthly,
                     IOM=iom.monthly), file="seat_price.csv", row.names=FALSE)




