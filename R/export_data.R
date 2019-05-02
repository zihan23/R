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


StockData<-data.frame(log(S1_open/S1_close),log(S2_open/S2_close),log(S3_open/S3_close),log(S4_open/S4_close),log(S5_open/S5_close),log(S6_open/S6_close),log(S7_open/S7_close),log(S8_open/S8_close),log(S9_open/S9_close),log(S10_open/S10_close),S1_volume/100000000,S2_volume/100000000,S3_volume/100000000,S4_volume/100000000,S5_volume/100000000,S6_volume/100000000,S7_volume/100000000,S8_volume/100000000,S9_volume/100000000,S10_volume/100000000)
headings<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD','AET_vol','BSX_vol','CELG_vol','CVS_vol','JNJ_vol','MYL_vol','MTD_vol','HSIC_vol','XRAY_vol','GILD_vol')
names(StockData)<-headings
write.csv(StockData,file="/Users/yashi/desktop/StockData.csv",row.names = FALSE)