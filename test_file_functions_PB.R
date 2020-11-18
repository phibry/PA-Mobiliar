load("data/data_mobi") 
n=20

n1=30
n2 =200
k=2



mobidat= data[,4]
head(mobidat)
tail(mobidat)



start="2015-10-30"
end="2020-04-30"


horizon=paste(start,"::",end,sep = "")


sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)


signal <-rep(0,length(sma1))

signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1

signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1

signal=reclass(signal,sma1)



chartSeries(mobidat,subset=horizon,theme=chartTheme("white"),name= "sna",type="")

#addSMA(n=n1,on=1,col = "blue")


addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")


trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade

mean(ret,na.rm=T)
names(ret)="filter"

chart.Bar(ret,main="returns daily")
chart.CumReturns(ret, main="Naive Rule: Cum Returns")
chart.Drawdown(ret,main="Naive Rule: Percentage Drawdown")
charts.PerformanceSummary(ret, main="-ç Naive Buy Rule")

