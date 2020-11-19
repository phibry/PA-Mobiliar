# generating a signal for addta in bollinger bands
# input bb = bollinger bands from Mybbfunction 
# input AAPL = an xts times series 
# needs libraries qauntmod and performance analytics

generate_signal_bb<-function(bb,AAPL)
{
  signal <-rep(0,length(bb$up)) # first date has no signal
  # Detect crossings of upper band  
  signal[which(Cl(AAPL)>bb$up&lag(Cl(AAPL))<lag(bb$up))]<-1
  # Detect crossings of lower band  
  signal[which(Cl(AAPL)<bb$dn&lag(Cl(AAPL))>lag(bb$dn))]<--1
  # The problem is: we might observe several upper crossings before the next lower crossing occurs
  # A buy signal goes from the smallest upper crossing (greater than lower crossing) to the smallest lower crossing (greater than this upper crossing): see example below
  new_sig<-signal
  # Specify trading signal
  up_cross<-which(Cl(AAPL)>bb$up&lag(Cl(AAPL))<lag(bb$up)) 
  down_cross<-which(Cl(AAPL)<bb$dn&lag(Cl(AAPL))>lag(bb$dn))
  end_sig<-min(up_cross,down_cross)
  start_sig<-1
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
