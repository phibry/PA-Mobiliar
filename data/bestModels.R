# Buy and Hold, 2019-01-01 Out-Of-Sample####
source("add/libraries.R")
source("add/functions_PA.R")
load("data/data_mobi")

ind <- na.exclude(data[,1:4])
ind.lr <- na.exclude(diff(log(ind)))

ind.lr.out <- ind.lr[paste("2019-01-01","/",sep="")]

cumsum(ind.lr.out)

sharpe_bnh_1 <- as.double(sqrt(250) * mean(ind.lr.out[,1]) / sqrt(var(ind.lr.out[,1])))
sharpe_bnh_2 <- as.double(sqrt(250) * mean(ind.lr.out[,2]) / sqrt(var(ind.lr.out[,2])))
sharpe_bnh_3 <- as.double(sqrt(250) * mean(ind.lr.out[,3]) / sqrt(var(ind.lr.out[,3])))
sharpe_bnh_4 <- as.double(sqrt(250) * mean(ind.lr.out[,4]) / sqrt(var(ind.lr.out[,4])))

# AR-Optim####
performante_ar <- function(xin, xout) {
  # AR-Model Order p -> 1:8
  # Find best in-sample ar_obj
  # Compute out-of-of sample daily trading returns
  # Find best Sharpe
  best <- 0
  for (j in 1:8) {
    arma_obj <- arima(xin, order=c(j,0,0))
    mat_out <- cbind(xout)
    for (k in 1:(j)) {
      mat_out <- cbind(mat_out, lag(xout, k=k))
    }
    ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
      as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]
    
    ret_arima <- na.exclude(sign(ar_pred)*xout)
    
    sharpe_ar <- as.double(sqrt(250) * mean(ret_arima, na.rm=T) / sqrt(var(ret_arima, na.rm=T)))
    
    if (sharpe_ar > best) {
      best <- as.data.frame(sharpe_ar)
      rownames(best) <- paste(j)
    }
  }
  return(list(sharpe_ar=best, ar_p=rownames(best)))
}
perfplot_ar <- function(optim_ar_obj, lr_series, inx, insamp="2019-01-01") {
  # Max Optim-Element
  max_obj <- optim_ar_obj[which.max(optim_ar_obj[,2]),]
  
  
  # Startdate
  start_date <- optim_ar_obj[which.max(optim_ar_obj[,2]),][1]
  
  xall <- lr_series[paste(start_date,"/",sep="")][,inx]
  xin <- xall[paste("/",insamp,sep="")]
  xout <- xall[paste(insamp,"/",sep="")]
  
  
  # sharpe
  sharpe_bnh <- as.double(sqrt(250) * mean(xout) / sqrt(var(xout)))
  sharpe_ar <- as.numeric(optim_ar_obj[which.max(optim_ar_obj[,2]),][2])
  
  # Model Order AR(p)
  p <- as.numeric(optim_ar_obj[which.max(optim_ar_obj[,2]),][3])
  
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
  ret_arima <- sign(ar_pred)*xout
  
  # Plot
  perf_bnh <- cumsum(xout)
  perf_ar <- cumsum(na.exclude(ret_arima))
  ymin <- min(c(min(perf_bnh), min(perf_ar)))
  ymax <- max(c(max(perf_bnh), max(perf_ar)))

  plot(perf_bnh, main=paste("Startdate: ",start_date,"| Out-of-sample-Index: ",inx), lwd=1.5,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(perf_ar, lty=2, lwd=1, col="red")
  print(addLegend("topleft", legend.names = c(paste("Buy & Hold:", round(sharpe_bnh, 2)),
                                              paste("AR(",p,"):", round(sharpe_ar, 2))),
                  lty=c(1, 2),
                  lwd=c(1.5, 1),
                  col=c("black", "red"),
                  cex=0.8))
}
optim_ar <- function(x.lr, inx, insamp="2019-01-01", minyear=3, maxyear=18) {
  ind.lr <- x.lr
  year_list <- NULL
  for (yx in minyear:maxyear) {
    year_list <- c(year_list, paste(2000+yx,"-01-01", sep=""))
  }
  
  # Create result_matrix
  res_mat <- matrix(1:(3*length(year_list)), ncol=3,
                    dimnames = list(1:length(year_list), c("StartDate", "AR-Sharpe", "AR-Order p")))
  
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
  # Loop through years
  for (i in 1:length(year_list)) {
    startdate <- year_list[i]
    
    ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]
    ind.lr.in <- ind.lr.all[paste("/",insamp,sep="")]
    ind.lr.out <- ind.lr.all[paste(insamp,"/",sep="")]
    
    yolo1 <- performante_ar(xin=ind.lr.in[,inx], xout=ind.lr.out[,inx])
    
    res_mat[i,1] <- year_list[i]
    res_mat[i,2:3] <- c(as.numeric(yolo1$sharpe_ar), round(as.numeric(yolo1$ar_p)))
    
    setTxtProgressBar(pb, i)
  }
  res_df <- as.data.frame(res_mat)
  close(pb)
  
  perfplot_ar(res_df, ind.lr.all, inx)
  
  return(res_df)
}

## Index1####
par(mfrow=c(1,1))
opt_ar_1 <- optim_ar(x.lr=ind.lr, inx=1)
opt_ar_1
# StartDate        AR-Sharpe AR-Order p
# 1  2003-01-01 3.29595353087806          1
# 2  2004-01-01 3.03663080384028          1
# 3  2005-01-01 3.14595370330181          1
# 4  2006-01-01 2.95696375410652          1
# 5  2007-01-01 2.72157066213196          1
# 6  2008-01-01 1.86730160376776          1
# 7  2009-01-01 2.47288363622484          1
# 8  2010-01-01 2.65097314751304          1
# 9  2011-01-01  2.0037895032277          3
# 10 2012-01-01 1.85182920854677          3
# 11 2013-01-01 2.03321635232673          4
# 12 2014-01-01 1.95391519328146          3
# 13 2015-01-01 1.93205919008196          5
# 14 2016-01-01 3.44862913565847          2
# 15 2017-01-01 2.16327238600849          4
# 16 2018-01-01 3.76531914186755          2

#save(opt_ar_1, file="data/R_Files/optim_ar_1.RData")

# Best Model
opt_ar_1[which.max(opt_ar_1[,2]),]







## Index2####
opt_ar_2 <- optim_ar(x.lr=ind.lr, inx=2)
opt_ar_2
# StartDate        AR-Sharpe AR-Order p
# 1  2003-01-01 3.03303677020712          1
# 2  2004-01-01 3.10004245587814          1
# 3  2005-01-01  3.0056303577096          1
# 4  2006-01-01 2.65609741507539          1
# 5  2007-01-01  2.6127467113483          1
# 6  2008-01-01 2.74519371281584          1
# 7  2009-01-01 1.81872615773264          1
# 8  2010-01-01 2.68081106945987          1
# 9  2011-01-01 1.76322872598847          1
# 10 2012-01-01  1.6509536698659          1
# 11 2013-01-01 1.75483021028372          1
# 12 2014-01-01 1.61422632652208          1
# 13 2015-01-01 1.32746265592463          1
# 14 2016-01-01 1.75483021028372          1
# 15 2017-01-01 1.61493950303439          3
# 16 2018-01-01 1.96081233421565          2
save(opt_ar_2, file="data/R_Files/optim_ar_2.RData")


# Best Model
opt_ar_2[which.max(opt_ar_2[,2]),]







## Index3####
opt_ar_3 <- optim_ar(x.lr=ind.lr, inx=3)
opt_ar_3
# StartDate        AR-Sharpe AR-Order p
# 1  2003-01-01 2.39846008834075          1
# 2  2004-01-01 2.39846008834075          1
# 3  2005-01-01 2.52837972310466          1
# 4  2006-01-01 2.52837972310466          1
# 5  2007-01-01 2.30122652365081          1
# 6  2008-01-01 2.32596914576708          1
# 7  2009-01-01 1.91689384883742          3
# 8  2010-01-01 2.38928203790148          1
# 9  2011-01-01 2.22518046632104          4
# 10 2012-01-01 1.54296433840858          2
# 11 2013-01-01 1.15519602735763          1
# 12 2014-01-01 1.46833520572479          2
# 13 2015-01-01 1.18440395331472          3
# 14 2016-01-01 1.57488862612892          4
# 15 2017-01-01 1.23709869095954          4
# 16 2018-01-01 1.08560127991841          3
save(opt_ar_3, file="data/R_Files/optim_ar_3.RData")


# Best Model
opt_ar_3[which.max(opt_ar_3[,2]),]







## Index4####
opt_ar_4 <- optim_ar(x.lr=ind.lr, inx=4)
opt_ar_4
# Best Model
# StartDate         AR-Sharpe AR-Order p
# 1  2003-01-01  2.21814552894437          1
# 2  2004-01-01  2.16858264074697          1
# 3  2005-01-01  2.15917453873627          1
# 4  2006-01-01  2.12250460120158          1
# 5  2007-01-01  2.18062931729536          1
# 6  2008-01-01  2.07452095501032          1
# 7  2009-01-01  1.50820360010582          1
# 8  2010-01-01   2.1819618081639          1
# 9  2011-01-01  2.03065748663533          1
# 10 2012-01-01   1.5346114273831          2
# 11 2013-01-01 0.884246741008454          3
# 12 2014-01-01  1.41217255946765          2
# 13 2015-01-01  1.11676847529102          4
# 14 2016-01-01   1.1206519698519          7
# 15 2017-01-01 0.810093667134683          8
# 16 2018-01-01  1.21982741090185          7
save(opt_ar_4, file="data/R_Files/optim_ar_4.RData")


opt_ar_4[which.max(opt_ar_4[,2]),]











#.####
# MGARCH-Optim####
compute_sigma_t_out_of_sample_func <- function(x_out, y.garch_11, x_in)
{
  # Vola based on in-sample parameters
  sigma_t <- rep(NA,length(x_out))
  a <- y.garch_11@fit$coef["beta1"]
  b <- y.garch_11@fit$coef["alpha1"]
  d <- y.garch_11@fit$coef["omega"]
  
  # First data point: based on last in-sample data
  # Formula: variance-equation
  sigma_t[1] <- sqrt(d + a*y.garch_11@sigma.t[length(y.garch_11@sigma.t)]^2 +
                       b*x_in[length(x_in)]^2)
  
  # On out-of-sample span
  for (i in 2:length(x_out))
  {
    sigma_t[i] <- sqrt(d+a*sigma_t[i-1]^2+b*x_out[i-1]^2)
  }
  # Transform sigma_t into xts object
  names(sigma_t) <- index(x_out)
  sigma_t<-as.xts(sigma_t)
  index(sigma_t) <- index(x_out)
  return(list(sigma_t=sigma_t))
}
performante_mgarch <- function(xin, xout) {
  ## MGARCH
  garch_11 <- garchFit(~garch(1,1), data=xin, delta=2,
                       include.delta=F, include.mean=F, trace=F)
  
  # Sigma_t in-sample
  sigma_t_in <- garch_11@sigma.t
  names(sigma_t_in) <- index(xin)
  sigma_t_in <- as.xts(sigma_t_in)
  index(sigma_t_in) <- index(xin)
  
  # Regression
  lm_obj <- lm(xin ~ sigma_t_in)
  
  # Sigma_t out of sample
  sigma_t_out <- compute_sigma_t_out_of_sample_func(xout, garch_11, xin)$sigma_t
  
  # MGARCH-Prediction
  mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t_out
  
  ## Trading: Signum
  returns_mgarch <- na.exclude(sign(mgarch_predict)*xout)
  
  # Sharpe
  sharpe_mgarch <- as.double(sqrt(250) * mean(returns_mgarch) / sqrt(var(returns_mgarch)))
  
  return(sharpe_mgarch)
}
optim_mgarch <- function(x.lr, inx, insamp="2019-01-01") {
  ind.lr <- x.lr
  year_list <- NULL
  for (yx in 3:18) {
    year_list <- c(year_list, paste(2000+yx,"-01-01", sep=""))
  }
  
  # Create result_matrix
  res_mat <- matrix(1:(2*length(year_list)), ncol=2,
                    dimnames = list(1:length(year_list), c("StartDate", "MGARCH-Sharpe")))
  
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
  for (i in 1:length(year_list)) {
    startdate <- year_list[i]
    
    ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]
    ind.lr.in <- ind.lr.all[paste("/",insamp,sep="")]
    ind.lr.out <- ind.lr.all[paste(insamp,"/",sep="")]
    
    yolo1 <- performante_mgarch(xin=ind.lr.in[,inx], xout=ind.lr.out[,inx])
    
    res_mat[i,1] <- year_list[i]
    res_mat[i,2] <- yolo1
    
    setTxtProgressBar(pb, i)
  }
  res_df <- as.data.frame(res_mat)
  close(pb)
  return(res_df)
}

# Index1

opt_mgarch_1 <- optim_mgarch(x.lr=ind.lr, inx=1)
opt_mgarch_1

# Index2
opt_mgarch_2 <- optim_mgarch(x.lr=ind.lr, inx=2)
opt_mgarch_2

# Index3
opt_mgarch_3 <- optim_mgarch(x.lr=ind.lr, inx=3)
opt_mgarch_3

# Index4
opt_mgarch_4 <- optim_mgarch(x.lr=ind.lr, inx=4)
opt_mgarch_4





#.####
# MA-Filter####
source("add/libraries.R")
source("add/functions_PA.R")
load("data/data_mobi")

ind <- na.exclude(data[,1:4])
ind.lr <- na.exclude(diff(log(ind)))

optimize_sma <- function(x, min_L=5, max_L=500, L_forecast=1, start = "2015-10-30", insamp = "2019-01-01", plot_T=TRUE) {
  
  xall <- x[paste(start,"/",sep="")]
  xin <- xall[paste("/",insamp,sep="")]
  xout <- xall[paste(insamp,"/",sep="")]
  
  sharpe_opt<- -9.e+99
  draw_opt <- -9.e+99
  mse_opt <- -9.e+99
  
  filler <- 1
  res_mat <- matrix(1:((length(min_L:max_L))*4), ncol=4,
                    dimnames = list(1:(length(min_L:max_L)), c("L", "Sharpe", "Drawdown", "MSE")))
  
  pb <- txtProgressBar(min = min_L, max = max_L, style = 3)
  
  for (L in min_L:max_L) {
    # Moving Average
    yhat_full <- SMA(xin, n=L)
    # Cut init
    yhat <- yhat_full[max_L:length(yhat_full)]
    
    # Perf (daily)
    perf <- lag(sign(yhat))*xin
    
    sharpe <- sqrt(250)*mean(perf,na.rm=T)/sqrt(var(perf,na.rm=T))
    
    maxdraw<- -max(abs(Drawdowns(na.exclude(perf), geometric = F)))
    
    future_returns <- lag(SMA(xin, n=L_forecast), k=-L_forecast)
    MSE <- -as.double(mean((future_returns-yhat)^2, na.rm=T))/var(xin, na.rm=T)
    
    res_mat[filler,] <- c(L, sharpe, maxdraw, MSE)
    
    filler <- filler + 1
    
    # Best
    if (sharpe > sharpe_opt) {
      sharpe <- as.data.frame(sharpe)
      sharpe_opt <- sharpe
      rownames(sharpe_opt) <- paste(L)
      colnames(sharpe_opt) <- "Sharpe"
    }
    
    if (maxdraw > draw_opt) {
      maxdraw <- as.data.frame(maxdraw)
      draw_opt <- maxdraw
      rownames(draw_opt) <- paste(L)
      colnames(draw_opt) <- "Drawdown"
    }
    
    if (MSE > mse_opt) {
      MSE <- as.data.frame(MSE)
      mse_opt <- MSE
      rownames(mse_opt) <- paste(L)
      colnames(mse_opt) <- "MSE"
    }

    setTxtProgressBar(pb, L)
  }
  close(pb)
  
  listerino <- list(sharpe_opt=sharpe_opt, draw_opt=draw_opt, mse_opt=mse_opt, res_mat=res_mat)
  
  if (plot_T) {
    perfplot_sma(xall, insamp, listerino, max_L)
  }

  return(listerino)
}
perfplot_sma <- function(xall, insamp, opt_obj, max_L) {
  sharpe_L <- as.numeric(rownames(opt_obj$sharpe_opt))
  drawdown_L <- as.numeric(rownames(opt_obj$draw_opt))
  MSE_L <- as.numeric(rownames(opt_obj$mse_opt))
  
  # Trading Signal
  sharpe_signal <- sign(SMA(xall, n=sharpe_L))
  drawdown_signal <- sign(SMA(xall, n=drawdown_L))
  MSE_signal <- sign(SMA(xall, n=MSE_L))
  
  # Returns
  sharpe_ret <- lag(sharpe_signal)*xall
  drawdown_ret <- lag(drawdown_signal)*xall
  MSE_ret <- lag(MSE_signal)*xall
  
  # Plot
  # par(mfrow=c(2,1))
  # chartSeries(cumsum(na.exclude(xall[paste(insamp, "/", sep=""),])), theme=chartTheme("white", bg.col="#FFFFFF"),name= "SMA",type="")
  # addSMA(n=sharpe_L, on=1, col="red")
  # addSMA(n=drawdown_L, on=1, col="blue")
  # addSMA(n=MSE_L, on=1, col="green")
  # 
  # plot(cumsum(na.exclude(xall[paste(insamp, "/", sep=""),])), main="Comparison")
  # plot(cumsum(na.exclude(sharpe_ret[paste(insamp, "/", sep=""),])), col="red")
  
  bnh_perf <- cumsum(na.exclude(xall[paste(insamp, "/", sep=""),]))
  sharpe_perf <- cumsum(na.exclude(sharpe_ret[paste(insamp, "/", sep=""),]))
  drawdown_perf <- cumsum(na.exclude(drawdown_ret[paste(insamp, "/", sep=""),]))
  MSE_perf <- cumsum(na.exclude(MSE_ret[paste(insamp, "/", sep=""),]))
  
  ymin <- min(c(min(bnh_perf), min(sharpe_perf), min(drawdown_perf), min(MSE_perf)))
  ymax <- max(c(max(bnh_perf), max(sharpe_perf), max(drawdown_perf), max(MSE_perf)))
  
  
  plot(sharpe_perf, main="Comparison", lwd=2,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(sharpe_perf, col="red", lty=2, lwd=2)
  lines(drawdown_perf, col="blue", lty=3, lwd=2)
  lines(MSE_perf, col="green", lty=4, lwd=2)
  
  print(addLegend("topleft", legend.names = c("Buy & Hold",
                                              paste("Sharpe, L: ",sharpe_L),
                                              paste("Drawdown, L: ",drawdown_L),
                                              paste("MSE, L: ",MSE_L)),
                  lty=c(1, 2, 3, 4),
                  lwd=c(2, 2, 2, 2),
                  col=c("black", "red", "blue", "green"),
                  cex=0.8))
}

# Ind1####
optim_sma1 <- optimize_sma(x=ind.lr[,1])
opt_obj <- optim_sma1

head(optim_sma1$res_mat)
optim_sma1$mse_opt

par(mfrow=c(3,1))
plot(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,2], col="red", type="l",
     xlab="L", ylab="", main="Sharpe: In-Sample")
points(x=as.numeric(rownames(optim_sma1$sharpe_opt)),
       y=as.numeric(optim_sma1$sharpe_opt), col="red", pch=19)

plot(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,3], col="blue", type="l",
     xlab="L", ylab="", main="Drawdown: In-Sample")
points(x=as.numeric(rownames(optim_sma1$draw_opt)),
       y=as.numeric(optim_sma1$draw_opt), col="blue", pch=19)

plot(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,4], col="green", type="l",
     xlab="L", ylab="", main="MSE: In-Sample")
points(x=as.numeric(rownames(optim_sma1$mse_opt)),
       y=as.numeric(optim_sma1$mse_opt), col="green", pch=19)


# in one
par(mfrow=c(1,1))
plot(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,2], col="red", type="l",
     xlab="L", ylab="", main="In-Sample")
points(x=as.numeric(rownames(optim_sma1$sharpe_opt)),
       y=as.numeric(optim_sma1$sharpe_opt), col="red", pch=19)
lines(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,3], col="blue", type="l")

points(x=as.numeric(rownames(optim_sma1$draw_opt)),
       y=as.numeric(optim_sma1$draw_opt), col="blue", pch=19)

lines(x=optim_sma1$res_mat[,1], y=optim_sma1$res_mat[,4], col="green", type="l")
points(x=as.numeric(rownames(optim_sma1$mse_opt)),
       y=as.numeric(optim_sma1$mse_opt), col="green", pch=19)



# Ind2####
optim_sma2 <- optimize_sma(x=ind.lr[,2])


# Ind3####
optim_sma3 <- optimize_sma(x=ind.lr[,3])


# Ind4####
optim_sma4 <- optimize_sma(x=ind.lr[,4])
