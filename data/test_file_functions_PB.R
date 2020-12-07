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

x=t(c("2015-10-01",1.225487,1,106,-0.006546676,18,102))

df=as.data.frame(x)
colnames(df)=c("date","Sharpe","Sharpe_S","Sharpe_L","Drawdown","Drawdown_S","Drawdown_L")
df[,1]=as.Date(df[,1])
qtxs <- xts(df[,-1], df[,1])

optim_ma_cross_obj_1_with_xts=rbind(optim_ma_cross_obj_1_xts,qtxs)

save(optim_ma_cross_obj_1_with_xts, file="data/R_Files/optim_ma_cross_obj_1_with_xts.RData")








############################################## do not change ###################################
loop_func_v2 <- function(data, insamp="2019-01-01", minyear=3, maxyear=18,returnmax=10){
  #This function uses the cross_optim_easy_v2 function an loops the function through the insample timespan and returns a data frame 
  
  year_list <- NULL
  for (year in minyear:maxyear) {
    year_list <- c(year_list, paste(2000+year,"-01-01", sep=""))
  }
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  startdate=as.Date(paste(2000+minyear,"-01-01", sep=""))
  
  datevec <- as.data.frame(c(rep(startdate,returnmax)))
  colnames(datevec)="startdate"
  iteration<- cross_optim_easy_v2(x=data, start=startdate,returnmax=returnmax)
  iteration_withdate=cbind(datevec,iteration)
  
  # Loop through years
  for (i in 2:length(year_list)) {
    startdate <- year_list[i]
    iteration<- cross_optim_easy_v2(x=data, start=startdate,returnmax=returnmax)
    
    datevec <- as.data.frame(c(rep(as.Date(startdate),returnmax)))
    colnames(datevec)="startdate"
    
    iteration_withdate_loop=cbind(datevec,iteration)
    iteration_withdate=rbind(iteration_withdate,iteration_withdate_loop)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(iteration_withdate)
}
cross_optim_easy_v2 <- function(x, start, end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250,returnmax=5){ 
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
  
  #searching for the 5 maximum sharpes/drawdowns with the filterlenghts
  l1_vec_sharp=c(rep(0,returnmax))
  l2_vec_sharp=c(rep(0,returnmax))
  vecsharp=tail(sort(sharpmat),returnmax)   #taking the 5 maximum sharpe ratios last one is the max of 5
  
  l1_vec_drawdown=c(rep(0,returnmax))
  l2_vec_drawdown=c(rep(0,returnmax))
  vecdrawdown=tail(sort(drawmat),returnmax) #taking the 5 maximum drawdowne ratios last one is the max of 5
  
  for(l in returnmax:1)
   {
   l1_vec_sharp[l]=(which(sharpmat==vecsharp[l],arr.ind=TRUE))[1]# adding the rows = L1 and columns = L2 to the 5 max sharpes
   l2_vec_sharp[l]=(which(sharpmat==vecsharp[l],arr.ind=TRUE))[2]#
   l1_vec_drawdown[l]=(which(drawmat==vecdrawdown[l],arr.ind=TRUE))[1]# adding the rows = L1 and columns = L2 to the 5 max drawdownes
   l2_vec_drawdown[l]=(which(drawmat==vecdrawdown[l],arr.ind=TRUE))[2]#
   
   }
  
  maximus=cbind(vecsharp,l1_vec_sharp,l2_vec_sharp,vecdrawdown,l1_vec_drawdown,l2_vec_drawdown)    #combining the vector to return each value
  colnames(maximus)=c("sharpe","sharpe_l1","sharpe_l2","drawdown","drawdown_l1","drawdown_l2")
  return(maximus)
}
###################################################################################################




m=cross_optim_easy(data[,1], start, end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250,returnmax=10)

index_1_tenbest_year=loop_func_v2(data[,1])

View(index_1_tenbest_year)






x=data[,1]
start="2012-01-01"


cross_optim_easy_v2 <- function(x, start, end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250,returnmax=5){ 
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
  
  #searching for the 5 maximum sharpes/drawdowns with the filterlenghts
  l1_vec_sharp=c(rep(0,returnmax))
  l2_vec_sharp=c(rep(0,returnmax))
  vecsharp=tail(sort(sharpmat),returnmax)   #taking the 5 maximum sharpe ratios last one is the max of 5
  
  View(sharpmat)
  
  l1_vec_drawdown=c(rep(0,returnmax))
  l2_vec_drawdown=c(rep(0,returnmax))
  vecdrawdown=tail(sort(drawmat),returnmax) #taking the 5 maximum drawdowne ratios last one is the max of 5
  
  for(l in returnmax:1)
  {
    l1_vec_sharp[l]=(which(sharpmat==vecsharp[l],arr.ind=TRUE))[1]# adding the rows = L1 and columns = L2 to the 5 max sharpes
    l2_vec_sharp[l]=(which(sharpmat==vecsharp[l],arr.ind=TRUE))[2]#
    l1_vec_drawdown[l]=(which(drawmat==vecdrawdown[l],arr.ind=TRUE))[1]# adding the rows = L1 and columns = L2 to the 5 max drawdownes
    l2_vec_drawdown[l]=(which(drawmat==vecdrawdown[l],arr.ind=TRUE))[2]#
    
  }
  
  maximus=cbind(vecsharp,l1_vec_sharp,l2_vec_sharp,vecdrawdown,l1_vec_drawdown,l2_vec_drawdown)    #combining the vector to return each value
  colnames(maximus)=c("sharpe","sharpe_l1","sharpe_l2","drawdown","drawdown_l1","drawdown_l2")
  return(maximus)
}