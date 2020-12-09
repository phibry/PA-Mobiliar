# Ljung-Box-Plots####
ljungplot <- function(x, lag=10) {
  testerino <- rep(NA, lag)
  for (i in 1:lag) {
    testerino[i] <- Box.test(x, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
  }
  
  plot(x=1:lag, y=testerino, main="Ljung-Box statistic",
       xlab = "lag", ylab = "p value", axes = FALSE, ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="#2297E6")
}
ljungplotGarch <- function(resid, sig) {
  p1 <- rep(NA, 20)
  p2 <- rep(NA, 20)
  for (i in 1:20) {
    p1[i] <- Box.test(resid/sig, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
    p2[i] <- Box.test((resid/sig)^2, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
  }
  
  par(mfrow=c(1,2))
  plot(x=1:20, y=p1, main=expression(paste("Ljung-Box statistic: ", u)),
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="#2297E6")
  points(x=10, y=p1[10], pch=16, col="#DF536B")
  points(x=15, y=p1[15], pch=16, col="#DF536B")
  points(x=20, y=p1[20], pch=16, col="#DF536B")
  
  plot(x=1:20, y=p2, main=expression(paste("Ljung-Box statistic: ", u^2)),
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="#2297E6")
  points(x=10, y=p2[10], pch=16, col="#DF536B")
  points(x=15, y=p2[15], pch=16, col="#DF536B")
  points(x=20, y=p2[20], pch=16, col="#DF536B")
}


#.####
# AR####
perfplot_ar <- function(optim_ar_obj, lr_series, inx, insamp="2019-01-01", plotter=TRUE, returner=TRUE) {
  # Max Optim-Element
  max_obj <- optim_ar_obj[which.max(optim_ar_obj[,2]),]
  
  # Startdate
  start_date <- max_obj[1]
  
  xall <- lr_series[paste(start_date,"/",sep="")][,inx]
  xin <- xall[paste("/",insamp,sep="")]
  xout <- xall[paste(insamp,"/",sep="")]
  
  
  # sharpe
  sharpe_bnh <- as.double(sqrt(250) * mean(xout) / sqrt(var(xout)))
  sharpe_ar <- as.numeric(max_obj[2])
  
  # Model Order AR(p)
  p <- as.numeric(max_obj[3])
  
  # AR-Fit, in-sample
  arma_obj <- arima(xin, order=c(p,0,0))
  
  # AR-Pred, out-of-sample
  mat_out <- cbind(xout)
  for (k in 1:(p)) {
    mat_out <- cbind(mat_out, lag(xout, k=k))
  }
  ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
    as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]
  
  # AR-Returns
  signal <- sign(ar_pred)
  ret_arima <- signal*xout
  
  # Plot
    perf_bnh <- cumsum(xout)
    perf_ar <- cumsum(na.exclude(ret_arima))
  if (plotter) {
    ymin <- min(c(min(perf_bnh), min(perf_ar)))
    ymax <- max(c(max(perf_bnh), max(perf_ar)))
    
    plot(perf_bnh, main=paste("Startdate: ",start_date,"| Out-of-sample-Index: ",inx), lwd=1.5,
         ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
    lines(perf_ar, lty=2, lwd=1, col="red")
    print(addLegend("topleft", legend.names = c(paste("Buy & Hold:", round(sharpe_bnh, 2)),
                                                paste("AR(",p,"):", round(sharpe_ar, 2))),
                    lty=c(1, 2),
                    lwd=c(1.5, 1),
                    col=c("#000000", "red"),
                    cex=0.8))
  }
  
  if (returner) {
    return(list(signal=sign(signal), perf_bnh=perf_bnh, perf_ar=perf_ar))
  }
}
ar_plotter <- function(perf_bnh, perf_ar, start_date, inx, p, sharpe_bnh, sharpe_ar, signal) {
  
  ymin <- min(c(min(perf_bnh), min(perf_ar)))
  ymax <- max(c(max(perf_bnh), max(perf_ar)))
  signal <- reclass(signal, perf_bnh)
  
  plot(perf_bnh, main=paste("Startdate: ",start_date,"| Out-of-sample-Index: ",inx), lwd=1.5,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(perf_ar, lty=2, lwd=1, col="#DF536B")
  addLegend("topleft", legend.names = c(paste("Buy & Hold:", round(sharpe_bnh, 2)),
                                              paste("AR(",p,"):", round(sharpe_ar, 2))),
                  lty=c(1, 2), lwd=c(1.5, 1), col=c("#000000", "#DF536B"), cex=1)
  
  print(lines(signal, on=NA, ylim=c(-1.3, 1.3), lwd=2))
  # axis(side=2, at=c(-1, 1), labels=c("Short", "Long"), las=2, tick=FALSE)
  # axis(side=4, at=c(-1, 1), las=2, tick=FALSE)
}

#.####
# SMA####
perfplot_sma_opt <- function(xall, insamp, res_obj, inx) {
  xall <- xall[,inx]
  
  # Optima
  sharpe_opt <- res_obj$res_df[which.max(res_obj$res_df[,2]),1:3]
  drawdown_opt <- res_obj$res_df[which.max(res_obj$res_df[,4]),c(1,4:5)]
  mse_opt <- res_obj$res_df[which.max(res_obj$res_df[,6]),c(1,6:7)]
  
  # Filterlengths
  sharpe_L <- as.numeric(sharpe_opt[3])
  drawdown_L <- as.numeric(drawdown_opt[3])
  MSE_L <- as.numeric(mse_opt[3])
  
  # Startdates
  sharpe_startdate <- sharpe_opt[1]
  
  # Trading Signal
  sharpe_signal <- sign(SMA(xall, n=sharpe_L))
  drawdown_signal <- sign(SMA(xall, n=drawdown_L))
  MSE_signal <- sign(SMA(xall, n=MSE_L))
  
  # Returns
  sharpe_ret <- lag(sharpe_signal)*xall
  drawdown_ret <- lag(drawdown_signal)*xall
  MSE_ret <- lag(MSE_signal)*xall
  
  bnh_perf <- cumsum(na.exclude(xall[paste(insamp, "/", sep=""),]))
  sharpe_perf <- cumsum(na.exclude(sharpe_ret[paste(insamp, "/", sep=""),]))
  drawdown_perf <- cumsum(na.exclude(drawdown_ret[paste(insamp, "/", sep=""),]))
  MSE_perf <- cumsum(na.exclude(MSE_ret[paste(insamp, "/", sep=""),]))
  
  ymin <- min(c(min(bnh_perf), min(sharpe_perf), min(drawdown_perf), min(MSE_perf)))
  ymax <- max(c(max(bnh_perf), max(sharpe_perf), max(drawdown_perf), max(MSE_perf)))
  
  
  plot(bnh_perf, main=paste("Out-of-sample-Index: ",inx), lwd=2,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(sharpe_perf, col="#DF536B", lty=2, lwd=2)
  lines(drawdown_perf, col="#2297E6", lty=3, lwd=2)
  lines(MSE_perf, col="#61D04F", lty=4, lwd=2)
  
  addLegend("topleft", legend.names = c("Buy & Hold",
                                        paste("StartDate: ", sharpe_opt[1], "| Sharpe | L: ",sharpe_L),
                                        paste("StartDate: ", drawdown_opt[1], "| Drawdown | L: ",drawdown_L),
                                        paste("StartDate: ", mse_opt[1], "| MSE | L: ",MSE_L)),
            lty=c(1, 2, 3, 4),
            lwd=c(2, 2, 2, 2),
            col=c("#000000", "#DF536B", "#2297E6", "#61D04F"),
            cex=0.8)
  
  lines(sharpe_signal, on=NA, ylim=c(-1.5, 1.5), col="#DF536B", lwd=2)
  lines(drawdown_signal, on=NA, ylim=c(-1.5, 1.5), col="#2297E6", lwd=2)
  print(lines(MSE_signal, on=NA, ylim=c(-1.5, 1.5), col="#61D04F", lwd=2))
}

#.####
# Lesser functions####
trading_counter <- function(trade_sig) {
  trade_count <- 1
  for (i in 1:length(trade_sig)) {
    
    if (is.na(trade_sig[i+1])) {
      break
      
    } else {
      if (trade_sig[i] != trade_sig[i+1]) {
        trade_count <- trade_count + 1
      }
    }
  }
  return(trade_count)
}
sharpe_fun <- function(x) {
  as.double(sqrt(250) * mean(x) / sqrt(var(x)))
}

#.####
# Unused####
# compute_sigma_t_out_of_sample_func <- function(x_out, y.garch_11, x_in)
# {
#   # Vola based on in-sample parameters
#   sigma_t <- rep(NA,length(x_out))
#   a <- y.garch_11@fit$coef["beta1"]
#   b <- y.garch_11@fit$coef["alpha1"]
#   d <- y.garch_11@fit$coef["omega"]
#   
#   # First data point: based on last in-sample data
#   # Formula: variance-equation
#   sigma_t[1] <- sqrt(d + a*y.garch_11@sigma.t[length(y.garch_11@sigma.t)]^2 +
#                        b*x_in[length(x_in)]^2)
#   
#   # On out-of-sample span
#   for (i in 2:length(x_out))
#   {
#     sigma_t[i] <- sqrt(d+a*sigma_t[i-1]^2+b*x_out[i-1]^2)
#   }
#   # Transform sigma_t into xts object
#   names(sigma_t) <- index(x_out)
#   sigma_t<-as.xts(sigma_t)
#   index(sigma_t) <- index(x_out)
#   return(list(sigma_t=sigma_t))
# }


# performante <- function(xin, xout, threshold_p=0.7, main="") {
#   
#   ## AR-Model-Order
#   testerino <- rep(0, 10)
#   j <- 1
#   while (!all(testerino > threshold_p)) {
#     # fit an ar-model
#     arma_obj <- arima(xin, order=c(j,0,0))
#     
#     # estiamte the Ljung-Box statistics
#     for (i in 1:10) {
#       testerino[i] <- Box.test(arma_obj$residuals, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
#     }
#     j <- j + 1
#   }
#   ##
#   ##
#   ##
#   ##
#   ## AR-Prediction
#   mat_out <- cbind(xout)
#   for (k in 1:(j-1)) {
#     mat_out <- cbind(mat_out, lag(xout, k=k))
#   }
#   ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
#     as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]
#   ##
#   ##
#   ##
#   ##
#   ## 
#   ## MGARCH
#   garch_11 <- garchFit(~garch(1,1), data=xin, delta=2,
#                        include.delta=F, include.mean=F, trace=F)
#   ##
#   ##
#   ##
#   ##
#   ## 
#   # Sigma_t in-sample
#   sigma_t_in <- garch_11@sigma.t
#   names(sigma_t_in) <- index(xin)
#   sigma_t_in <- as.xts(sigma_t_in)
#   index(sigma_t_in) <- index(xin)
#   ##
#   ##
#   ##
#   ##
#   ## 
#   # Regression
#   lm_obj <- lm(xin ~ sigma_t_in)
#   ##
#   ##
#   ##
#   ##
#   ## 
#   # Sigma_t out of sample
#   sigma_t_out <- compute_sigma_t_out_of_sample_func(xout, garch_11, xin)$sigma_t
#   ##
#   ##
#   ##
#   ##
#   ## 
#   # MGARCH-Prediction
#   mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t_out
#   ##
#   ##
#   ##
#   ##
#   ## 
#   ## Trading: Signum
#   ret_arima <- sign(ar_pred)*xout
#   returns_mgarch <- sign(mgarch_predict)*xout
#   ##
#   ##
#   ##
#   ##
#   ##   
#   # Sharpe
#   sharpe_bnh <- as.double(sqrt(250) * mean(xout) / sqrt(var(xout)))
#   sharpe_ar <- as.double(sqrt(250) * mean(ret_arima, na.rm=T) / sqrt(var(ret_arima, na.rm=T)))
#   sharpe_mgarch <- as.double(sqrt(250) * mean(returns_mgarch, na.rm=T) / sqrt(var(returns_mgarch, na.rm=T)))
#   ##
#   ##
#   ##
#   ##
#   ## 
#   # Plot
#   #par(mfrow=c(1,1))
#   (plot(cumsum(xout), main=main, lwd=1.5))
#   (lines(cumsum(na.exclude(ret_arima)), lty=2, lwd=1, col="red"))
#   (lines(cumsum(na.exclude(returns_mgarch)), lty=3, col="blue"))
#   
#   
#   print(addLegend("topleft", legend.names = c(paste("Buy & Hold:", round(sharpe_bnh, 2)),
#                                               paste("AR(",j-1,"):", round(sharpe_ar, 2)),
#                                               paste("MGARCH(1,1):", round(sharpe_mgarch, 2))),
#                   lty=c(1, 2, 3),
#                   lwd=c(1.5, 1, 1),
#                   col=c("black", "red", "blue"),
#                   cex=0.8))
# }


# optimize_simple_MA_func<-function(x,in_samp,min_L,max_L,x_trade)
# {
#   if (is.null(x_trade))
#     x_trade<-x
# 
#   sharpe_opt<--9.e+99
#   #in_s<-which(index(x)>in_samp)[1]
#   
#   pb <- txtProgressBar(min = min_L, max = max_L, style = 3)
#   perf_vec<-NULL
#   
#   for (L in min_L:max_L)
#   {
#     yhat_full <- SMA(x, n=L)
#     yhat <- yhat_full[max_L:length(yhat_full)]
# 
#     # Use x-trade for trading
#     #perf <- sign(yhat)[1:(in_s-1)]*x_trade[2:(in_s)]
#     perf<-lag(sign(yhat))*x_trade
#     
#     sharpe<-sqrt(250)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))
#     perf_vec<-c(perf_vec,sharpe)
#     if (sharpe>sharpe_opt)
#     {
#       sharpe_opt<-sharpe
#       L_opt<-L
#     }
#     setTxtProgressBar(pb, L)
#   }
#   close(pb)
#   names(perf_vec)<-paste("filter length ",min_L:max_L)
# 
#   return(list(L_opt=L_opt,sharpe_opt=sharpe_opt,perf_vec=perf_vec))
# }



# filt_func<-function(x,b)
# {
#   L<-nrow(b)
#   if (is.matrix(x))
#   {  
#     length_time_series<-nrow(x)
#   } else
#   {
#     if (is.vector(x))
#     {
#       length_time_series<-length(x)
#     } else
#     {
#       print("Error: x is neither a matrix nor a vector!!!!")
#     }
#   }
#   if (is.xts(x))
#   {
#     yhat<-x[,1]
#   } else
#   {
#     yhat<-rep(NA,length_time_series)
#   }
#   for (i in L:length_time_series)#i<-L
#   {
#     # If x is an xts object then we cannot reorder x in desceding time i.e. x[i:(i-L+1)] is the same as  x[(i-L+1):i]
#     #   Therefore, in this case, we have to revert the ordering of the b coefficients.    
#     if (is.xts(x))
#     {
#       if (ncol(b)>1)
#       {
#         yhat[i]<-as.double(sum(apply(b[L:1,]*x[i:(i-L+1),],1,sum)))
#       } else
#       {
#         yhat[i]<-as.double(b[L:1,]%*%x[i:(i-L+1)])#tail(x) x[(i-L+1):i]
#       }
#     } else
#     {
#       if (ncol(b)>1)
#       {
#         yhat[i]<-as.double(sum(apply(b[1:L,]*x[i:(i-L+1),],1,sum)))
#       } else
#       {
#         yhat[i]<-as.double(as.vector(b)%*%x[i:(i-L+1)])#tail(x) x[(i-L+1):i]
#       }
#     }
#   }
#   #  names(yhat)<-index(x)#index(yhat)  index(x)
#   #  yhat<-as.xts(yhat,tz="GMT")
#   return(list(yhat=yhat))
# }


# perf_plot_func<-function(x,L,in_samp,x_trade, ploterino=2)
# {
#   if (is.null(x_trade))
#     x_trade<-x
#   
#   b<-as.matrix(rep(1/L,L),ncol=1)
#   
#   yhat<-filt_func(x,b)$yhat
#   
#   is.xts(yhat)
#   
#   # Use x-trade for trading  
#   
#   # Lag k=1 mit signume
#   # Man filtert also logReturns
#   perf<-lag(sign(yhat),k=1)*x_trade
#   
#   par(mfrow=(c(ploterino,1)))
#   perf_in<-perf[paste("/",in_samp,sep="")]
#   sharpe<-sqrt(250)*mean(perf_in,na.rm=T)/sqrt(var(perf_in,na.rm=T))
#   print(plot(cumsum(na.exclude(perf_in)),main=paste("In-sample: Sharpe=",round(sharpe,2),sep="")))
#   perf_out<-perf[paste(in_samp,"/",sep="")]
#   sharpe<-sqrt(250)*mean(perf_out,na.rm=T)/sqrt(var(perf_out,na.rm=T))
#   print(plot(cumsum(na.exclude(perf_out)),main=paste("Out-sample: Sharpe=",round(sharpe,2),sep="")))
#   
# }





# optimize_sma <- function(x, inx, min_L=5, max_L=500, L_forecast=1, start = "2015-10-30", insamp = "2019-01-01", plot_T=TRUE) {
#   
#   xall <- x[paste(start,"/",sep="")][,inx]
#   xin <- xall[paste("/",insamp,sep="")]
#   xout <- xall[paste(insamp,"/",sep="")]
#   
#   sharpe_opt<- -9.e+99
#   draw_opt <- -9.e+99
#   mse_opt <- -9.e+99
#   
#   filler <- 1
#   res_mat <- matrix(1:((length(min_L:max_L))*4), ncol=4,
#                     dimnames = list(1:(length(min_L:max_L)), c("L", "Sharpe", "Drawdown", "MSE")))
#   
#   
#   for (L in min_L:max_L) {
#     # Moving Average
#     yhat_full <- SMA(xin, n=L)
#     # Cut init
#     yhat <- yhat_full[max_L:length(yhat_full)]
#     
#     # Perf (daily)
#     perf <- lag(sign(yhat))*xin
#     
#     sharpe <- sqrt(250)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))
#     
#     maxdraw<- -max(abs(Drawdowns(na.exclude(perf), geometric = F)))
#     
#     future_returns <- lag(SMA(xin, n=L_forecast), k=-L_forecast)
#     MSE <- -as.double(mean((future_returns-yhat)^2, na.rm=T))/var(xin, na.rm=T)
#     
#     res_mat[filler,] <- c(L, sharpe, maxdraw, MSE)
#     
#     filler <- filler + 1
#     
#     # Best
#     if (sharpe > sharpe_opt) {
#       sharpe <- as.data.frame(sharpe)
#       sharpe_opt <- sharpe
#       rownames(sharpe_opt) <- paste(L)
#       colnames(sharpe_opt) <- "Sharpe"
#     }
#     
#     if (maxdraw > draw_opt) {
#       maxdraw <- as.data.frame(maxdraw)
#       draw_opt <- maxdraw
#       rownames(draw_opt) <- paste(L)
#       colnames(draw_opt) <- "Drawdown"
#     }
#     
#     if (MSE > mse_opt) {
#       MSE <- as.data.frame(MSE)
#       mse_opt <- MSE
#       rownames(mse_opt) <- paste(L)
#       colnames(mse_opt) <- "MSE"
#     }
#   }
#   
#   listerino <- list(sharpe_opt=sharpe_opt, draw_opt=draw_opt, mse_opt=mse_opt, res_mat=res_mat)
#   
#   if (plot_T) {
#     perfplot_sma(xall, insamp, start, listerino, max_L, inx)
#   }
#   
#   return(listerino)
# }

# perfplot_sma <- function(xall, insamp, start, opt_obj, max_L, inx) {
#   sharpe_L <- as.numeric(rownames(opt_obj$sharpe_opt))
#   drawdown_L <- as.numeric(rownames(opt_obj$draw_opt))
#   MSE_L <- as.numeric(rownames(opt_obj$mse_opt))
#   
#   # Trading Signal
#   sharpe_signal <- sign(SMA(xall, n=sharpe_L))
#   drawdown_signal <- sign(SMA(xall, n=drawdown_L))
#   MSE_signal <- sign(SMA(xall, n=MSE_L))
#   
#   # Returns
#   sharpe_ret <- lag(sharpe_signal)*xall
#   drawdown_ret <- lag(drawdown_signal)*xall
#   MSE_ret <- lag(MSE_signal)*xall
#   
#   bnh_perf <- cumsum(na.exclude(xall[paste(insamp, "/", sep=""),]))
#   sharpe_perf <- cumsum(na.exclude(sharpe_ret[paste(insamp, "/", sep=""),]))
#   drawdown_perf <- cumsum(na.exclude(drawdown_ret[paste(insamp, "/", sep=""),]))
#   MSE_perf <- cumsum(na.exclude(MSE_ret[paste(insamp, "/", sep=""),]))
#   
#   ymin <- min(c(min(bnh_perf), min(sharpe_perf), min(drawdown_perf), min(MSE_perf)))
#   ymax <- max(c(max(bnh_perf), max(sharpe_perf), max(drawdown_perf), max(MSE_perf)))
#   
#   
#   plot(sharpe_perf, main=paste("Startdate: ",start,"| Out-of-sample-Index: ",inx), lwd=2,
#        ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
#   lines(sharpe_perf, col="#DF536B", lty=2, lwd=2)
#   lines(drawdown_perf, col="#2297E6", lty=3, lwd=2)
#   lines(MSE_perf, col="#61D04F", lty=4, lwd=2)
#   
#   addLegend("topleft", legend.names = c("Buy & Hold",
#                                         paste("Sharpe, L: ",sharpe_L),
#                                         paste("Drawdown, L: ",drawdown_L),
#                                         paste("MSE, L: ",MSE_L)),
#             lty=c(1, 2, 3, 4),
#             lwd=c(2, 2, 2, 2),
#             col=c("#000000", "#DF536B", "#2297E6", "#61D04F"),
#             cex=0.8)
#   
#   lines(sharpe_signal, on=NA, ylim=c(-1.5, 1.5), col="#DF536B", lwd=2)
#   lines(drawdown_signal, on=NA, ylim=c(-1.5, 1.5), col="#2297E6", lwd=2)
#   print(lines(MSE_signal, on=NA, ylim=c(-1.5, 1.5), col="#61D04F", lwd=3))
# }