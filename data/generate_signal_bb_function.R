

# function for generating bb
# input price = series of a price can be xts
# n = filterlength of MA ,note: in this function we use a SMA
# sd = factor how bollinger bands should be scaled ussually = 2
# return upper lower mavg and pcTB = 101% price is 1% over upper band, -1% price is exactly under lower band 1%


myBBands <- function (price,n,sd){
  mavg <- SMA(price,n)
  sdev <- rep(0,n)
  N <- nrow(price)
  for (i in (n+1):N){
    sdev[i]<- sd(price[(i-n+1):i])
  }
  sdev <- sqrt((n-1)/n)*sdev
  up <- mavg + sd*sdev
  dn <- mavg - sd*sdev
  pctB <- (price - dn)/(up - dn)
  output <- cbind(dn, mavg, up, pctB)
  colnames(output) <- c("dn", "mavg", "up", 
                        "pctB")
  return(output)
}

# generating a signal for addta in bollinger bands
# input bb = bollinger bands from Mybbfunction 
# input data = an xts times series 
# needs libraries qauntmod and performance analytics


generate_signal_bb<-function(bb,data){
  
  signal <-rep(0,length(bb$up)) # first date has no signal
  # Detect crossings of upper band  
  signal[which(data>bb$up&lag(data)<lag(bb$up))]=1
  # Detect crossings of lower band  
  signal[which(data<bb$dn&lag(data)>lag(bb$dn))]=-1
  # The problem is: we might observe several upper crossings before the next lower crossing occurs
  # A buy signal goes from the smallest upper crossing (greater than lower crossing) to the smallest lower crossing (greater than this upper crossing): see example below
  new_sig<-signal
  # Specify trading signal
  up_cross<-which(data>bb$up&lag(data)<lag(bb$up)) 
  down_cross<-which(data<bb$dn&lag(data)>lag(bb$dn))
  end_sig=min(up_cross,down_cross)
  start_sig=1
  for (i in 1:max(length(up_cross),length(down_cross)))#i<-1
  {
    # Check end of sample (NA)    
    if (!is.na(end_sig))
    {
      # If next signal (at end_sig) is buy    
      if (signal[end_sig]>0)
      {
        # Then sell during previous period from start_sig to end_sig (end_sig is enclosed because new trade starts next day)
        new_sig[start_sig:(end_sig)]<--1
      } else
      {
        # Else buy
        new_sig[start_sig:end_sig]<-1
      }
      # Define new start and ends: new start=last end+1      
      start_sig<-end_sig+1
      if (signal[end_sig]==1)
      {
        # If current is buy: search smallest down_cross >= current buy        
        end_sig<-down_cross[which(down_cross>end_sig)[1]]
      } else
      {
        # If current is sell: search smallest up_cross >= current sell       
        end_sig<-up_cross[which(up_cross>end_sig)[1]]
      }
      # At end: fill till the end
      #  Note that we have to invert signs (in comparison to above) because we use if (signal[start_sig-1]>0) (instead of if (signal[end_sig]>0))     
      if (is.na(end_sig))
      {
        if (signal[start_sig-1]>0)
        {
          # Sell        
          new_sig[start_sig:length(bb$up)]<-1
        } else
        {
          # Buy
          new_sig[start_sig:length(bb$up)]<--1
        }
        
      }
    }
  }
  # Check: the following line should be compared to up_cross and down_cross
  which(new_sig[1:(length(new_sig)-1)]<new_sig[2:length(new_sig)])
  return(new_sig)
}


optimize_simple_MA_func<-function(x_filt,min_L,max_L,x_trade,weight_criterion,L_forecast,in_samp)
{
  # Ensure that weights are positive and sum to one  
  weight_criterion<-abs(weight_criterion)/sum(abs(weight_criterion))
  # Intialize criterion: small value (we want to maximize it)  
  criterion_opt<--9.e+99
  pb <- txtProgressBar(min = min_L, max = max_L, style = 3)
  perf_mat<-NULL
  for (L in min_L:max_L)
  {
    # Use new filtering from TTR package    
    yhat_full <-SMA(x_filt,n=L)
    # Select in-sample span
    #    yhat_in_h<-yhat_full[paste("/",in_samp,sep="")]
    # Skip all initializations up to length of longest possible filter: so that all filter outputs have identical length
    # This is much faster than aligning all filter outputs in a matrix...
    yhat<-yhat_full[max_L:length(yhat_full)]
    # Use x-trade for trading: note that xts will automtically synchronize dates even if x_trade has out-of-sample data in it    
    perf<-lag(sign(yhat))*x_trade
    # perf has a NA at strat because we lag yhat_in    
    perf<-na.exclude(perf)
    # Ideally we want a filter that maximizes the Sharpe: large aboslute perf and small vola
    sharpe<-sqrt(250)*mean(perf)/sqrt(var(perf))
    {
      # we could use SharpeRatio function instead (which gives the same result)    
      sharpe<-sqrt(250)*SharpeRatio(perf, Rf=0, FUN="StdDev")
    }
    
    # Ideally we want a filter that maximizes the (negative of the) worst absolute loss
    maxdraw<--max(abs(Drawdowns(na.exclude(perf), geometric = F)))
    # Compute mean of future returns: if L_forecast=1 then this is simply tomorrows return
    # We have to shift the data into the future by L_forecast     
    future_returns<-lag(SMA(x_trade,n=L_forecast),k=-L_forecast)
    # Compute MSE of forecast error: 
    # We use negative MSE because we maximize
    # We scale by 1/var(x_trade,na.rm=T) in order that all three criteria have comparable scales
    MSE<--as.double(mean((future_returns-yhat)^2,na.rm=T))/var(x_trade,na.rm=T)
    # The criterion combines sharpe, drawdown and MSE
    criterion<-weight_criterion[1]*sharpe+weight_criterion[2]*maxdraw+weight_criterion[3]*MSE
    #    findDrawdowns(x, geometric = F)
    # Add the new results to perf_mat    
    perf_mat<-rbind(perf_mat,c(sharpe,maxdraw,MSE,criterion))
    # Detect maximum    
    if (criterion>criterion_opt)
    {
      criterion_opt<-criterion
      L_opt<-L
    }
    setTxtProgressBar(pb, L)
  }
  colnames(perf_mat)<-c("Sharpe","Drawdown","Negative MSE forecast error","Criterion")
  close(pb)
  rownames(perf_mat)<-paste("filter length ",min_L:max_L)
  if (min_L>1)
    perf_mat<-rbind(matrix(ncol=ncol(perf_mat),nrow=min_L-1),perf_mat)
  
  return(list(L_opt=L_opt,criterion_opt=criterion_opt,perf_mat=perf_mat))
}



opt_trade_func<-function(weight_criterion,min_L,max_L,x_filt,x_trade,in_samp,L_forecast)
  
{
  # if x_trade  is not specified, then we identify it with x_filt
  # Note that we assume that x_filt are log-returns (not prices)  
  if (is.null(x_trade))
    x_trade<-x_filt
  
  # New optimization
  # Uses filtering provided by TTR: much faster than previous filtering
  x_in<-x_filt[paste("/",in_samp,sep="")]
  
  opt_obj<-optimize_simple_MA_func(x_in,min_L,max_L,x_trade,weight_criterion,L_forecast,in_samp)
  
  perf_mat<-opt_obj$perf_mat
  L_opt<-opt_obj$L
  L_opt
  criterion_opt<-opt_obj$criterion_opt
  criterion_opt
  
  mplot<-(perf_mat)
  
  ts.plot(mplot[,1],ylim=c(min(na.exclude(mplot)),max(na.exclude(mplot))),col="blue",main=paste("Sharpe (blue), drawdown (red), Negative MSE (green) and  criterion (cyan): weight_sharpe=",paste(weight_criterion,collapse=","),sep=""))
  lines(mplot[,2],col="red")
  lines(mplot[,3],col="green")
  lines(mplot[,4],col="cyan")
  
  
  # In sample  
  if (L_opt<length(x_in))
    ret<-plot_perf(x_in,L_opt,x_trade,max_L)$ret
  # For perf calculation we use the same span as the longest possible filter (which is what the optimization is based on...)
  # Full span
  ret<-plot_perf(x_filt,L_opt,x_trade,max_L)
  return(list(L_opt=L_opt,perf_mat=perf_mat,criterion_opt=criterion_opt))
}




# New performance charts based on charts.PerformanceSummary
plot_perf<-function(x_filt,L,x_trade,max_L)
{
  if (is.null(x_trade))
    x_trade<-x_filt
  # Use x_filt for filtering  
  signal_MA<-sign(SMA(x_filt,n=L))
  # Remove initializations of longest possible filter (to make results comparable)  
  signal_MA<-signal_MA[max_L:length(signal_MA)]
  # Trade x_trade
  ret<-lag(signal_MA)*x_trade
  if (F)
  {
    #  print(chart.Bar(ret,main="Daily return"))
    #  print(chart.CumReturns(ret,main="Cum Returns"))
    #  print(chart.Drawdown(ret,main="Percentage Drawdown"))
  }
  # Finally, we can see the summary of performance using charts.PerformanceSummary() to evaluate the performance.
  # Set geometric = F to see linear perfs (when working with log-transformed data)
  charts.PerformanceSummary(ret,main=paste(names(x_filt),": EqMA(",L,")",sep=""), geometric = F)
  return(list(ret=ret))
}