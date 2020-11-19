load("data/data_mobi") 
source("data/libraries.R")
#daten####

mobidat= data[,4]
head(mobidat)
tail(mobidat)

start="2015-10-30"
end="2020-04-30"


horizon=paste(start,"::",end,sep = "")


#MA crossings##### ----------------------------

sn1=30
n2 =200

ma1 <-SMA(mobidat,n=n1)
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

# bollinger bands#### -------------------------------------------------------------

source("data/generate_signal_bb_function.R")

mobisub=mobidat[horizon]
head(mobisub)
k=2
n= 20

class(Cl(GOOGL))
class(mobisub)

bb <-BBands(mobisub,n=20, sd=2)
bb= myBBands(mobisub,n,k)
bb= myBBands(Cl(GOOGL),n,k)

bb
signal=generate_signal_bb(bb,mobisub)

head(bb)

class(signal)

length(signal)

length(mobisub)

chartSeries(mobisub,theme=chartTheme("white"),name= "bollinger",type="")
addBBands(n=n,sd=k)
addTA(signal)







