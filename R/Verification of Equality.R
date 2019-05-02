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

Date<-time(sp500$AET)
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

StockData<-data.frame(Date,log(S1_open/S1_close),log(S2_open/S2_close),log(S3_open/S3_close),log(S4_open/S4_close),log(S5_open/S5_close),log(S6_open/S6_close),log(S7_open/S7_close),log(S8_open/S8_close),log(S9_open/S9_close),log(S10_open/S10_close))
headings_<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
StockData$NormDate <- seq.int(nrow(StockData))
t.test(StockData$AET.Open, StockData$BSX.Open, alternative = "two.sided", var.equal = FALSE)