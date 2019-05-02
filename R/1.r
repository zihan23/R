library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(quantmod)

getwd()
sp500<-new.env()
options("getSymbols.warning4.0"=FALSE)
getSymbols("AET",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("BSX",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("CELG",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("CVS",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("JNJ",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("MYL",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("MTD",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("HSIC",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("XRAY",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))
getSymbols("GILD",env = sp500, src = "yahoo",from = as.Date("2016-01-01"), to = as.Date("2018-11-02"))

S1_open<-sp500$AET$AET.Open
S1_close<-sp500$AET$AET.Close
S2_open<-sp500$BSX$BSX.Open
S2_close<-sp500$BSX$BSX.Close
S3_open<-sp500$CELG$CELG.Open
S3_close<-sp500$CELG$CELG.Close
S4_open<-sp500$CVS$CVS.Open
S4_close<-sp500$CVS$CVS.Close
S5_open<-sp500$JNJ$JNJ.Open
S5_close<-sp500$JNJ$JNJ.Close
S6_open<-sp500$MYL$MYL.Open
S6_close<-sp500$MYL$MYL.Close
S7_open<-sp500$MTD$MTD.Open
S7_close<-sp500$MTD$MTD.Close
S8_open<-sp500$HSIC$HSIC.Open
S8_close<-sp500$HSIC$HSIC.Close
S9_open<-sp500$XRAY$XRAY.Open
S9_close<-sp500$XRAY$XRAY.Close
S10_open<-sp500$GILD$GILD.Open
S10_close<-sp500$GILD$GILD.Close

StockData<-data.frame(log(S1_open/S1_close),log(S2_open/S2_close),log(S3_open/S3_close),log(S4_open/S4_close),log(S5_open/S5_close),log(S6_open/S6_close),log(S7_open/S7_close),log(S8_open/S8_close),log(S9_open/S9_close),log(S10_open/S10_close))
headings_<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
names(log_return_data_frame)<-headings_

#question1
data<-StockData[,1]
#parta
active_trading_weekly<-function(data)
{
  n_daily<-length(data)
  n_weekly<-floor(length(data)/5)
  weekly<-rep(0,n_weekly)
  for(i in (1:n_weekly))
  {
    weekly[i]<-sum(data[((i-1)*5+1):(i*5)])
  }
  return(weekly)
}
weekly_INTC<-active_trading_weekly(data)
#draw histogram
bin<-round(sqrt(length(weekly_INTC)))
hist(weekly_INTC,breaks = bin,main="weekly AET Histogam", xlab = "StockData", ylab = "frequency")

#partB
#split data into 2
n1<-floor(length(data)/2)
data_1<-data[1:n1]
data_2<-data[(n1+1):length(data)]
#first half
x_bar<-mean(data_1)
first_half<-sqrt(5)*(data_1-x_bar)
bin_1<-round(sqrt(n1))
hist(first_half, breaks = bin_1,main = "Histogram of first_half",col = "blue", xlab="StockData",ylab="frequency")
#second half
weekly_data<-active_trading_weekly(data_2)
y_bar<-mean(weekly_data)
second_half<-weekly_data-y_bar
bin_2<-round(sqrt(length(second_half)))
hist(second_half, breaks = bin_2,main = "Histogram of second_half",col = "blue", xlab="StockData",ylab="frequency")

#partC
#confidence interval for unknown mean and variance
qnorm(0.95)
qt(0.95,df=length(data)-1)

x_bar_<-mean(data)
s<-sd(data)
t<-qt(0.975,df=length(data)-1)
upper_bound<-x_bar_+t*s/sqrt(length(data))
lower_bound<-x_bar_-t*s/sqrt(length(data))
confidence_interval_mean<-data.frame(lower_bound,upper_bound)
headings<-c('lower bound','upper bound')
names(confidence_interval_mean)<-headings
confidence_interval_mean

#confidence interval for unknown variance
df<-length(data)-1
first_value<-qchisq(0.025,df)
second_value<-qchisq(0.975,df)
upper_bound2<-df*s^2/first_value
lower_bound2<-df*s^2/second_value
confidence_interval_variance<-data.frame(lower_bound2,upper_bound2)
headings2<-c('lower bound','upper bound')
names(confidence_interval_variance)<-headings2
confidence_interval_variance


