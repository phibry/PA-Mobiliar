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


