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

bb= myBBands(mobisub,n,k)

signal=generate_signal_bb(bb,mobisub)
signal<-reclass(signal,bb$up)
 


length(signal)

length(mobisub)

chartSeries(mobisub,theme=chartTheme("white"),name= "bollinger",type="")
addBBands(n=n,sd=k)
addTA(signal,type="S",col="red")


trade <- Lag(signal[from_to],1)
return <- diff(log(mobisub))
ret<-(return)*trade
names(ret)<-"filter"


charts.PerformanceSummary(ret, main="Classic BB-rule")
# macd####-------------------------------------------------------------------------------


# analyzing data####-------------------------
load("data/data_mobi")

ind <- data[,1:4]
int <- data[,5:12]

par(mfrow=c(2,1))

plot(ind, main="Indexes 1-4", col=2:5, lwd=1,legend.loc="topleft")
plot(log_ind,main="LOG_Indexes 1-4",col=2:5, lwd=1,legend.loc="topleft")

log_ind <-    log(ind)


par(mfrow=c(4,1))
log_ret_ind<- diff(log_ind)

plot(log_ret_ind[,1],main="Log_returns index 1",col = 2)
plot(log_ret_ind[,2],main="Log_returns index 2",col = 3)
plot(log_ret_ind[,3],main="Log_returns index 3",col = 4)
plot(log_ret_ind[,4],main="Log_returns index 4",col = 5)

x<-as.data.frame(log_ind)

pairs(x)
cor(x)

c=as.data.frame(cor(x))
c



