library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(quantmod)
install.packages("corrplot")
library(corrplot)


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
S1_volume<-sp500$AET$AET.Volume
S2_open<-sp500$BSX$BSX.Open
S2_close<-sp500$BSX$BSX.Close
S2_volume<-sp500$BSX$BSX.Volume
S3_open<-sp500$CELG$CELG.Open
S3_close<-sp500$CELG$CELG.Close
S3_volume<-sp500$CELG$CELG.Volume
S4_open<-sp500$CVS$CVS.Open
S4_close<-sp500$CVS$CVS.Close
S4_volume<-sp500$CVS$CVS.Volume
S5_open<-sp500$JNJ$JNJ.Open
S5_close<-sp500$JNJ$JNJ.Close
S5_volume<-sp500$JNJ$JNJ.Volume
S6_open<-sp500$MYL$MYL.Open
S6_close<-sp500$MYL$MYL.Close
S6_volume<-sp500$MYL$MYL.Volume
S7_open<-sp500$MTD$MTD.Open
S7_close<-sp500$MTD$MTD.Close
S7_volume<-sp500$MTD$MTD.Volume
S8_open<-sp500$HSIC$HSIC.Open
S8_close<-sp500$HSIC$HSIC.Close
S8_volume<-sp500$HSIC$HSIC.Volume
S9_open<-sp500$XRAY$XRAY.Open
S9_close<-sp500$XRAY$XRAY.Close
S9_volume<-sp500$XRAY$XRAY.Volume
S10_open<-sp500$GILD$GILD.Open
S10_close<-sp500$GILD$GILD.Close
S10_volume<-sp500$GILD$GILD.Volume


ReturnData<-data.frame(log(S1_open/S1_close),log(S2_open/S2_close),log(S3_open/S3_close),log(S4_open/S4_close),log(S5_open/S5_close),log(S6_open/S6_close),log(S7_open/S7_close),log(S8_open/S8_close),log(S9_open/S9_close),log(S10_open/S10_close))
headings1<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
names(ReturnData)<-headings1

VolumeData<-data.frame(S1_volume/100000000,S2_volume/100000000,S3_volume/100000000,S4_volume/100000000,S5_volume/100000000,S6_volume/100000000,
                       S7_volume/100000000,S8_volume/100000000,S9_volume/100000000,S10_volume/100000000)
headings2<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
names(VolumeData)<-headings2

cor_data<-cor(StockData)
corrplot.mixed(cor_data,lower.col = "black", number.cex = .7)

output=c()
for (i in 1:10) {
  y<-ReturnData[,i]
  x<-VolumeData[,i]
  reg<-lm(y~x)
  reg_result<-summary(reg)
  out<-data.frame(reg_result$coefficients)
  output<-rbind(output,out)
  print(reg_result)
}
write.csv(output,file="/Users/yashi/desktop/reg2.csv",row.names = FALSE)
