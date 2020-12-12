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
start_date="2010-01-01"
in_sample ="2010-01-01"
  
x<-log_ind[paste(start_date,"/",sep="")]

# code from methology

install.packages(tidyr)
library(tidyr)
#test1= as.data.frame(test1)


#################################################saving as xts files ########
test1[,1]=as.Date(test1[,1])
test1=test1 %>%separate(`S-Filter`,c("Sharpe_S","Sharpe_L")," / ")
test1=test1%>%separate(`D-Filter`,c("Drawdown_S","Drawdown_L")," / ")
test1[,2:7]=test1[,2:7] %>% mutate_if(is.character,as.numeric)
qxts <- xts(test1[,-1], test1[,1])
optim_ma_cross_obj_1_xts=qxts
save(optim_ma_cross_obj_1_xts, file="data/R_Files/optim_ma_cross_obj_1_xts.RData")


test2[,1]=as.Date(test2[,1])
test2=test2 %>%separate(`S-Filter`,c("Sharpe_S","Sharpe_L")," / ")
test2=test2%>%separate(`D-Filter`,c("Drawdown_S","Drawdown_L")," / ")
test2[,2:7]=test2[,2:7] %>% mutate_if(is.character,as.numeric)
qxts <- xts(test2[,-1], test2[,1])
optim_ma_cross_obj_2_xts=qxts
save(optim_ma_cross_obj_2_xts, file="data/R_Files/optim_ma_cross_obj_2_xts.RData")



test3[,1]=as.Date(test3[,1])
test3=test3 %>%separate(`S-Filter`,c("Sharpe_S","Sharpe_L")," / ")
test3=test3%>%separate(`D-Filter`,c("Drawdown_S","Drawdown_L")," / ")
test3[,2:7]=test3[,2:7] %>% mutate_if(is.character,as.numeric)
qxts <- xts(test3[,-1], test3[,1])
optim_ma_cross_obj_3_xts=qxts
save(optim_ma_cross_obj_3_xts, file="data/R_Files/optim_ma_cross_obj_3_xts.RData")

test4[,1]=as.Date(test4[,1])
test4=test4 %>%separate(`S-Filter`,c("Sharpe_S","Sharpe_L")," / ")
test4=test4%>%separate(`D-Filter`,c("Drawdown_S","Drawdown_L")," / ")
test4[,2:7]=test4[,2:7] %>% mutate_if(is.character,as.numeric)
qxts <- xts(test4[,-1], test4[,1])
optim_ma_cross_obj_4_xts=qxts
save(optim_ma_cross_obj_4_xts, file="data/R_Files/optim_ma_cross_obj_4_xts.RData")


#######################################plotting the data######### from optimizaiton

##################sharpe maxdrow
plot(optim_ma_cross_obj_1_xts[,1],ylim= c(-2,4),main= "Max Sharpe/Drawdown Hyperoptimization Index 1",col="black")
lines(optim_ma_cross_obj_1_xts[,4]*100,lwd=2,col="green")

  
  
  
addLegend("topleft", on=1, legend.names = c("Sharpe ", "MaxDrawdown * 100 "), lty=c(1, 1), lwd=c(2, 2),col=c("black","green"))
################## lengths
plot(optim_ma_cross_obj_1_xts[,2:3],type="l",col= "red",main= "Filterlengths Hyperoptimization Index 1 ",)
lines(optim_ma_cross_obj_1_xts[,5:6], col=c("blue"),lwd=2)
addLegend("topleft", on=1, legend.names = c("Sharpe long filter", "Drawdown long filter"), lty=c(1, 1), lwd=c(1, 1),col=c("red","blue"))
addLegend("bottomleft", on=1, legend.names = c("Sharpe short filter", "Drawdown short filter"), lty=c(1, 1), lwd=c(1, 1),col=c("red","blue"))

##################data for kable

test1[,1]=as.Date(test1[,1])
test1[,c(2,4)]=test1[,c(2,4)] %>% mutate_if(is.character,as.numeric)
test1[,c(2,4)]=round(test1[,c(2,4)],3)
##################### data for adding straight line in plot

ind1_opt$best_drawdown
ind1_opt$best_sharpe

x=t(c("2015-10-30",1.225487,1,106,-0.006546676,18,102))

df=as.data.frame(x)
colnames(df)=c("date","Sharpe","Sharpe_S","Sharpe_L","Drawdown","Drawdown_S","Drawdown_L")
df[,1]=as.Date(df[,1])
qtxs <- xts(df[,-1], df[,1])

index_1_onebest_year_with15=rbind(index_1_onebest_year,qtxs)

save(index_1_onebest_year_with15, file="data/R_Files/index_1_onebest_yearwith15.RData")



################dont change #####  for appendix
loop_func_v3 <- function(data, minyear=3, maxyear=18,returnmax=10){
  #This function uses the cross_optim_easy_v2 function an loops the function through the insample timespan and returns a data frame 
  
  year_list <- NULL
  for (year in minyear:maxyear) {
    year_list <- c(year_list, paste(2000+year,"-01-01", sep=""))
  }
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  startdate=as.Date(paste(2000+minyear,"-01-01", sep=""))
  
  datevec <- as.data.frame(c(rep(startdate,returnmax)))
  colnames(datevec)="startdate"
  iteration<- cross_optim_easy_v3(x=data, start=startdate,returnmax=returnmax)
  iteration_withdate=cbind(datevec,iteration)
  
  # Loop through years
  for (i in 2:length(year_list)) {
    startdate <- year_list[i]
    iteration<- cross_optim_easy_v3(x=data, start=startdate,returnmax=returnmax)
    
    datevec <- as.data.frame(c(rep(as.Date(startdate),returnmax)))
    colnames(datevec)="startdate"
    
    iteration_withdate_loop=cbind(datevec,iteration)
    iteration_withdate=rbind(iteration_withdate,iteration_withdate_loop)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  iteration_withdate <- xts(iteration_withdate[,-1], iteration_withdate[,1])
  return(iteration_withdate)
}
cross_optim_easy_v3 <- function(x, start, end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250,returnmax=10){ 
  ###This function optimizes the ma crossings with the given filterlength in the given timespan and returns a dataframe with the  "returnmax"
  # sharpe and drawdownfilter which where found by common trading rules of ma crossings.
  horizon <- paste(start,"::",end,sep = "")
  
  sharpmat<-matrix(NA,L1max,L2max)  ## craeting empty matrizes for sharpe and maxdrawdown
  drawmat <-matrix(NA,L1max,L2max)
  
  pb = txtProgressBar(min = L1min, max = L1max, style = 3)
  for (k in L1min:L1max)
  {
    
    for (j in L2min:L2max)
    {
      
      # Simple Moving Averages
      sma1 <- SMA(x,k)
      sma2 <- SMA(x,j)
      
      # Signals
      signal <- rep(0,length(sma1))
      signal[which(sma1>sma2&lag(sma1)<lag(sma2))] <- 1
      signal[which(sma1<sma2&lag(sma1)>lag(sma2))]< - -1
      signal[which(sma1>sma2)] <- 1
      signal[which(sma1<sma2)] <- -1
      signal <- reclass(signal,sma1)
      
      # Trading
      trade   <-   Lag(signal[horizon],1)
      return  <-   diff(log(x))
      ret <- return*trade
      ret <- na.exclude(ret)
      
      # Sharpe
      sharpmat[k,j] <- sqrt(250)*mean(ret)/sqrt(var(ret))
      
      # Drawdown
      drawmat[k,j] <- -max(abs(Drawdowns(ret, geometric = F)))
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
  
  #finding the [returmax] best sharpevalues an thei indexes =l1 / l2
  sharpmax <- which(sharpmat>=sort(sharpmat, decreasing = T)[returnmax], arr.ind = T)
  sharpmax=head(sharpmax,returnmax)  
  #finding the [returmax] best drawevalues an thei indexes =l1 / l2
  drawmax <- which(drawmat>=sort(drawmat, decreasing = T)[returnmax], arr.ind = T)
  drawmax=head(drawmax,returnmax)  
  
  maximus=cbind(sharpmat[sharpmax],sharpmax,drawmat[drawmax],drawmax)    #combining the vector to return each value
  colnames(maximus)=c("sharpe","sharpe_l1","sharpe_l2","drawdown","drawdown_l1","drawdown_l2")
  return(as.data.frame(maximus))
}
################################################################################tenbest


index_1_tenbest_year=loop_func_v3(data=data[,1], minyear=3, maxyear=18,returnmax=10)
save(index_1_tenbest_year, file="data/R_Files/index_1_tenbest_year.RData")

index_2_tenbest_year=loop_func_v3(data=data[,2],  minyear=3, maxyear=18,returnmax=10)
save(index_2_tenbest_year, file="data/R_Files/index_2_tenbest_year.RData")

index_3_tenbest_year=loop_func_v3(data=data[,3],  minyear=3, maxyear=18,returnmax=10)
save(index_3_tenbest_year, file="data/R_Files/index_3_tenbest_year.RData")

index_4_tenbest_year=loop_func_v3(data=data[,4],  minyear=3, maxyear=18,returnmax=10)
save(index_4_tenbest_year, file="data/R_Files/index_4_tenbest_year.RData")

###################################################onebest#####################################
index_1_onebest_year=loop_func_v3(data=data[,1], minyear=3, maxyear=18,returnmax=1)
save(index_1_onebest_year, file="data/R_Files/index_1_onebest_year.RData")

index_2_onebest_year=loop_func_v3(data=data[,2], minyear=3, maxyear=18,returnmax=1)
save(index_2_onebest_year, file="data/R_Files/index_2_onebest_year.RData")

index_3_onebest_year=loop_func_v3(data=data[,3], minyear=3, maxyear=18,returnmax=1)
save(index_3_onebest_year, file="data/R_Files/index_3_onebest_year.RData")

index_4_onebest_year=loop_func_v3(data=data[,4], minyear=3, maxyear=18,returnmax=1)
save(index_4_onebest_year, file="data/R_Files/index_4_onebest_year.RData")
######################################################################################################
load("data/R_Files/index_1_tenbest_year.RData")

timevec_year=as.Date(  c( "2003-01-01","2002-01-01","2004-01-01","2005-01-01","2006-01-01","2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01"))
######################################################################################plottiplotti shiftings
plot(index_1_tenbest_year[,1],ylim= c(-2,4),main= "Max Sharpe/Drawdown Hyperoptimization Index 1",col="black",type="p",major.ticks="auto",format.labels="%b-%Y")
lines(index_1_tenbest_year[,4]*100,lwd=2,col="green",type="p")
addLegend("topleft", on=1, legend.names = c("Sharpe ", "MaxDrawdown * 100 "), lty=c(1, 1), lwd=c(2, 2),col=c("black","green"))
events <- xts("Figure 20/21/22", as.Date("2015-10-01"))

plot(index_1_tenbest_year[,2],type="p",col= "red",main= "Filterlengths Hyperoptimization Index 1 ",ylim=c(-10,260),format.labels="%b-%Y")
lines(index_1_tenbest_year[,3], col=c("coral"),lwd=2,type="p")
lines(index_1_tenbest_year[,5], col=c("blue"),lwd=2,type="p")
lines(index_1_tenbest_year[,6], col=c("cornflowerblue"),lwd=2,type="p")

lines(index_1_onebest_year[,c(2,3,5,6)],type="p",pch=3,lwd= 3,cex=2 ,col= c("red","coral","blue","cornflowerblue"))

addLegend("left", on=1, legend.names = c("long filter sharpe", "long filter drawdown "), lty=c(1, 1), lwd=c(1, 1),col=c("coral","cornflowerblue"))
addLegend("bottomleft", on=1, legend.names = c("short filter sharpe", "short filter Drawdown "), lty=c(1, 1), lwd=c(1, 1),col=c("red","blue"))
addLegend("topright", on=1, legend.names = c("maximum values"), pch=3,box=T)

################################################################################################
return_buy_and_hold_1  =   diff(log(data[,1]))##### buy and hold
return_buy_and_hold_2  =   diff(log(data[,2]))
return_buy_and_hold_3  =   diff(log(data[,3]))
return_buy_and_hold_4  =   diff(log(data[,4]))
##############################index 1############################
n1=49
n2 =246
mobidat= data[,1] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"
horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
names(ret)="filter"
cumsu
plot(Cumsum(ret))
lines(return_buy_and_hold_1,col="red")
#SharpeRatio(ret,FUN="StdDev")
chart.Bar(ret,main="returns daily")
chart.CumReturns(ret, main="Naive Rule: Cum Returns")
addCumReturns(return_buy_and_hold_4, main = "", on = NA, type = "l", col = NULL, lty = 1,lwd = 1, pch = 0)
#chart.Drawdown(ret,main="Naive Rule: Percentage Drawdown")
charts.PerformanceSummary(ret, main="Naive Buy Rule")

#SharpeRatio(data,FUN="StdDev")
#ames(ret)="filter"



##best for 1
 2017 37  189  dd   2017 1 171
 
##best for 2

s2016   17 111 dd 2014  17 106

##best for 3
2016  18 103     dd 19 101

##best for 4
2016 12 134    dd 2016 12 100

#############1##############
n1=19
n2  = 101
mobidat= data[,4] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"
horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade


head(ret)

ret=return[horizon]

names(ret)="filter"

sqrt(255)*SharpeRatio(ret,FUN="StdDev")

chart.Bar(ret,main="returns daily")
chart.CumReturns(ret, main="Naive Rule: Cum Returns")

#chart.Drawdown(ret,main="Naive Rule: Percentage Drawdown")
chart.CumReturns(ret, main="Naive Rule: Cum Returns")
addTA(ret)
charts.PerformanceSummary(ret, main="Naive Buy Rule")
#SharpeRatio(data,FUN="StdDev")
#ames(ret)="filter"


##########################################


