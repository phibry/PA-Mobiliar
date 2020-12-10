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
                    dimnames = list(1:length(year_list), c("StartDate", "AR-Sharpe", "p")))
  
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
  # Loop through years
  for (i in 1:length(year_list)) {
    startdate <- year_list[i]
    
    ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]
    ind.lr.in <- ind.lr.all[paste("/",insamp,sep="")]
    ind.lr.out <- ind.lr.all[paste(insamp,"/",sep="")]
    
    yolo1 <- performante_ar(xin=ind.lr.in[,inx], xout=ind.lr.out[,inx])
    
    res_mat[i,1] <- year_list[i]
    res_mat[i,2:3] <- c(round(as.numeric(yolo1$sharpe_ar), 3), round(as.numeric(yolo1$ar_p)))
    
    setTxtProgressBar(pb, i)
  }
  res_df <- as.data.frame(res_mat)
  close(pb)
  
  # print(head(ind.lr.all))
  # print(tail(ind.lr.all))
  perfplot_ar(res_df, ind.lr, inx)
  
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
#save(opt_ar_2, file="data/R_Files/optim_ar_2.RData")


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
#save(opt_ar_3, file="data/R_Files/optim_ar_3.RData")


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
#save(opt_ar_4, file="data/R_Files/optim_ar_4.RData")


opt_ar_4[which.max(opt_ar_4[,2]),]




trade_count <- 1
signalerino <- na.exclude(signalerino)

for (i in 1:length(signalerino)) {
  
  if (is.na(signalerino[i+1])) {
    break
    
  } else {
    if (signalerino[i] != signalerino[i+1]) {
      trade_count <- trade_count + 1
    }
  }
}





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

optimize_sma <- function(x, inx, min_L=5, max_L=500, L_forecast=1, start = "2015-10-30", insamp = "2019-01-01", plot_T=TRUE) {
  
  xall <- x[paste(start,"/",sep="")][,inx]
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
    perfplot_sma(xall, insamp, start, listerino, inx)
  }

  return(listerino)
}


perfplot_sma <- function(xall, insamp, start, opt_obj, inx) {
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
  
  bnh_perf <- cumsum(na.exclude(xall[paste(insamp, "/", sep=""),]))
  sharpe_perf <- cumsum(na.exclude(sharpe_ret[paste(insamp, "/", sep=""),]))
  drawdown_perf <- cumsum(na.exclude(drawdown_ret[paste(insamp, "/", sep=""),]))
  MSE_perf <- cumsum(na.exclude(MSE_ret[paste(insamp, "/", sep=""),]))
  
  ymin <- min(c(min(bnh_perf), min(sharpe_perf), min(drawdown_perf), min(MSE_perf)))
  ymax <- max(c(max(bnh_perf), max(sharpe_perf), max(drawdown_perf), max(MSE_perf)))
  
  
  plot(sharpe_perf, main=paste("Startdate: ",start,"| Out-of-sample-Index: ",inx), lwd=2,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(sharpe_perf, col="red", lty=2, lwd=2)
  lines(drawdown_perf, col="blue", lty=3, lwd=2)
  lines(MSE_perf, col="green", lty=4, lwd=2)
  
  addLegend("topleft", legend.names = c("Buy & Hold",
                                              paste("Sharpe, L: ",sharpe_L),
                                              paste("Drawdown, L: ",drawdown_L),
                                              paste("MSE, L: ",MSE_L)),
                  lty=c(1, 2, 3, 4),
                  lwd=c(2, 2, 2, 2),
                  col=c("black", "red", "blue", "green"),
                  cex=0.8)
  
  lines(sharpe_signal, on=NA, ylim=c(-1.5, 1.5), col="red", lwd=2)
  lines(drawdown_signal, on=NA, ylim=c(-1.5, 1.5), col="blue", lwd=2)
  print(lines(MSE_signal, on=NA, ylim=c(-1.5, 1.5), col="green"), lwd=2)
}
# Ind1 2015-10-30####
optim_sma1 <- optimize_sma(x=ind.lr, inx=1)

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



# Ind2 2015-10-30####
optim_sma2 <- optimize_sma(x=ind.lr, inx=2)


# Ind3 2015-10-30####
optim_sma3 <- optimize_sma(x=ind.lr, inx=3)


# Ind4 2015-10-30####
optim_sma4 <- optimize_sma(x=ind.lr, inx=4)



#.####
#.####
#.####
#.####
# Optim ALL####
loop_sma <- function(x=ind.lr, inx=1, insamp = "2019-01-01", minyear=3, maxyear=16) {
  
  year_list <- NULL
  for (yx in minyear:maxyear) {
    year_list <- c(year_list, paste(2000+yx,"-01-01", sep=""))
  }
  
  # Create result_matrix
  res_mat <- matrix(1:(7*length(year_list)), ncol=7,
                    dimnames = list(1:length(year_list), c("StartDate", "Sharpe", "L_Sharpe", "Drawdown", "L_Drawdown", "MSE", "L_MSE")))
  
  lirino <- NULL
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
  # Loop through years
  for (i in 1:length(year_list)) {
    startdate <- year_list[i]
    
    
    opt_obj <- optimize_sma(x, inx, min_L=5, max_L=500, L_forecast=1, start = startdate, insamp = "2019-01-01", plot_T=FALSE)
    res_mat[i,] <- c(year_list[i],
                     as.numeric(opt_obj$sharpe_opt), rownames(opt_obj$sharpe_opt),
                     as.numeric(opt_obj$draw_opt), rownames(opt_obj$draw_opt),
                     as.numeric(opt_obj$mse_opt), rownames(opt_obj$mse_opt))
    
    
    resulterino <- opt_obj$res_mat
    StartDate <- rep(year_list[i], dim(resulterino)[1])
    result <- as.data.frame(cbind(resulterino, StartDate))
    
    lirino <- rbind(lirino, result)
    
    
    setTxtProgressBar(pb, i)
  }
  res_df <- as.data.frame(res_mat)
  close(pb)
  
  return(list(res_df=res_df, lirino=lirino))  
}
optimize_sma_opt <- function(x, inx, min_L=5, max_L=500, L_forecast=1, start = "2015-10-30", insamp = "2019-01-01") {
  
  xall <- x[paste(start,"/",sep="")][,inx]
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
  return(listerino)
}

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
# Ind 1 ALL####
res1 <- loop_sma(x=ind.lr, inx=1, insamp = "2019-01-01", minyear=3, maxyear=16)
res1$res_df[,2] <- as.numeric(res1$res_df[,2])
res1$res_df[,3] <- as.numeric(res1$res_df[,3])
res1$res_df[,4] <- as.numeric(res1$res_df[,4])
res1$res_df[,5] <- as.numeric(res1$res_df[,5])
res1$res_df[,6] <- as.numeric(res1$res_df[,6])
res1$res_df[,7] <- as.numeric(res1$res_df[,7])

res1$lirino[,1] <- as.numeric(res1$lirino[,1])
res1$lirino[,2] <- as.numeric(res1$lirino[,2])
res1$lirino[,3] <- as.numeric(res1$lirino[,3])
res1$lirino[,4] <- as.numeric(res1$lirino[,4])
#save(res1, file="data/R_Files/optim_sma_1.RData")
res1$res_df

# Sharpe
res1$res_df[which.max(res1$res_df[,2]),1:3]

# Drawdown
res1$res_df[which.max(res1$res_df[,4]),c(1,4:5)]

# MSE
res1$res_df[which.max(res1$res_df[,6]),c(1,6:7)]



# Plot
seper <- (500-5)+1
x_s <- res1$res_df[,3] + 1 + (0:13*seper)
y_s <- res1$res_df[,2]

x_d <- res1$res_df[,5] + 1 + (0:13*seper)
y_d <- res1$res_df[,4]

x_m <- res1$res_df[,7] + 1 + (0:13*seper)
y_m <- res1$res_df[,6]

plot(x=1:length(res1$lirino[,2]), y=res1$lirino[,2], type="l", col="#DF536B", main="Performance Indicators | SMA | Index 1", xlab = "L", ylab="", xaxt = "n", ylim=c(-2, 3))
axis(1, at=c(1, seper, 1+2*seper, 2*seper+seper), labels=c("5", "500", "5", "500"))
axis(3, at=c(1+seper, seper+seper), labels=c("5", "500"))
axis(4, at=c(-0.5, 0.5, 1.5), labels=c("-0.10", "-0.05", "0"))
points(x_s, y_s, pch=19, col="#DF536B")

abline(v=1, lty=2, col="black")
abline(v=1+(500-5), lty=2, col="black")

abline(h=1.5, lty=2, col="black")
abline(h=0, lty=2, col="black")

lines((res1$lirino[,3])*18.23+1.5, type="l", col="#2297E6")
points(x_d, y_d*18.23+1.5, pch=19, col="#2297E6")

lines(res1$lirino[,4], type="l", col="#61D04F")
points(x_m, y_m, pch=19, col="#61D04F")

legend("topleft", legend=c("Sharpe", "MSE"),
       col=c("#DF536B", "#61D04F"), lty=c(1,1), cex=0.8,
       bty = "n")

legend("topright", legend="Drawdown",
       col="#2297E6", lty=1, cex=0.8,
       bty = "n")

for (i in 0:13) {
  text((1+500-5)/2 + seper*i, -2, paste(2003+i, "-01-01", sep=""), cex=0.6)
}

# Ind 1 Opt-Plot####
perfplot_sma_opt(xall=ind.lr, insamp="2019-01-01", res_obj=res1, inx=1)








#.####
# Ind 2####
res2 <- loop_sma(x=ind.lr, inx=2, insamp = "2019-01-01", minyear=3, maxyear=16)
res2$res_df[,2] <- as.numeric(res2$res_df[,2])
res2$res_df[,3] <- as.numeric(res2$res_df[,3])
res2$res_df[,4] <- as.numeric(res2$res_df[,4])
res2$res_df[,5] <- as.numeric(res2$res_df[,5])
res2$res_df[,6] <- as.numeric(res2$res_df[,6])
res2$res_df[,7] <- as.numeric(res2$res_df[,7])

res2$lirino[,1] <- as.numeric(res2$lirino[,1])
res2$lirino[,2] <- as.numeric(res2$lirino[,2])
res2$lirino[,3] <- as.numeric(res2$lirino[,3])
res2$lirino[,4] <- as.numeric(res2$lirino[,4])
#save(res2, file="data/R_Files/optim_sma_2.RData")

# Sharpe
res2$res_df[which.max(res2$res_df[,2]),1:3]

# Drawdown
res2$res_df[which.max(res2$res_df[,4]),c(1,4:5)]

# MSE
res2$res_df[which.max(res2$res_df[,6]),c(1,6:7)]

# Plot
seper <- (500-5)+1
x_s <- res2$res_df[,3] + 1 + (0:13*seper)
y_s <- res2$res_df[,2]

x_d <- res2$res_df[,5] + 1 + (0:13*seper)
y_d <- res2$res_df[,4]

x_m <- res2$res_df[,7] + 1 + (0:13*seper)
y_m <- res2$res_df[,6]

plot(x=1:length(res2$lirino[,2]), y=res2$lirino[,2], type="l", col="#DF536B", main="Performance Indicators | SMA | Index 2", xlab = "L", ylab="", xaxt = "n", ylim=c(-2, 2))
axis(1, at=c(1, seper, 1+2*seper, 2*seper+seper), labels=c("5", "500", "5", "500"))
axis(3, at=c(1+seper, seper+seper), labels=c("5", "500"))
axis(4, at=c(-1.5, -0.5, 0.5, 1.5), labels=c("-0.15", "-0.10", "-0.05", "0"))
points(x_s, y_s, pch=19, col="#DF536B")

abline(v=1, lty=2, col="black")
abline(v=1+(500-5), lty=2, col="black")

abline(h=1.5, lty=2, col="black")
abline(h=0, lty=2, col="black")

lines((res2$lirino[,3])*18.65+1.5, type="l", col="#2297E6", lty=1)
points(x_d, y_d*18.65+1.5, pch=19, col="#2297E6")

lines(res2$lirino[,4], type="l", col="#61D04F")
points(x_m, y_m, pch=19, col="#61D04F")

legend("topleft", legend=c("Sharpe", "MSE"),
       col=c("#DF536B", "#61D04F"), lty=c(1,1), cex=0.8,
       bty = "n")

legend("topright", legend="Drawdown",
       col="#2297E6", lty=1, cex=0.8,
       bty = "n")
for (i in 0:13) {
  text((1+500-5)/2 + seper*i, -2, paste(2003+i, "-01-01", sep=""), cex=0.6)
}

# Ind 2 Opt-Plot####
perfplot_sma_opt(xall=ind.lr, insamp="2019-01-01", res_obj=res2, inx=2)









#.####
# Ind 3####
res3 <- loop_sma(x=ind.lr, inx=3, insamp = "2019-01-01", minyear=3, maxyear=16)
res3$res_df[,2] <- as.numeric(res3$res_df[,2])
res3$res_df[,3] <- as.numeric(res3$res_df[,3])
res3$res_df[,4] <- as.numeric(res3$res_df[,4])
res3$res_df[,5] <- as.numeric(res3$res_df[,5])
res3$res_df[,6] <- as.numeric(res3$res_df[,6])
res3$res_df[,7] <- as.numeric(res3$res_df[,7])

res3$lirino[,1] <- as.numeric(res3$lirino[,1])
res3$lirino[,2] <- as.numeric(res3$lirino[,2])
res3$lirino[,3] <- as.numeric(res3$lirino[,3])
res3$lirino[,4] <- as.numeric(res3$lirino[,4])
#save(res3, file="data/R_Files/optim_sma_3.RData")

# Sharpe
res3$res_df[which.max(res3$res_df[,2]),1:3]

# Drawdown
res3$res_df[which.max(res3$res_df[,4]),c(1,4:5)]

# MSE
res3$res_df[which.max(res3$res_df[,6]),c(1,6:7)]

# Plot
seper <- (500-5)+1
x_s <- res3$res_df[,3] + 1 + (0:13*seper)
y_s <- res3$res_df[,2]

x_d <- res3$res_df[,5] + 1 + (0:13*seper)
y_d <- res3$res_df[,4]

x_m <- res3$res_df[,7] + 1 + (0:13*seper)
y_m <- res3$res_df[,6]

plot(x=1:length(res3$lirino[,2]), y=res3$lirino[,2], type="l", col="#DF536B", main="Performance Indicators | SMA | Index 3", xlab = "L", ylab="", xaxt = "n", ylim=c(-2.5, 2))
axis(1, at=c(1, seper, 1+2*seper, 2*seper+seper), labels=c("5", "500", "5", "500"))
axis(3, at=c(1+seper, seper+seper), labels=c("5", "500"))
axis(4, at=c(0.5, -0.25, -1), labels=c("0", "-0.15", "-0.3"))

points(x_s, y_s, pch=19, col="#DF536B")

abline(v=1, lty=2, col="black")
abline(v=1+(500-5), lty=2, col="black")

abline(h=0.5, lty=2, col="black")
abline(h=0, lty=2, col="black")

lines((res3$lirino[,3])*4.92+0.5, type="l", col="#2297E6", lty=1)
points(x_d, y_d*4.92+0.5, pch=19, col="#2297E6")

lines(res3$lirino[,4], type="l", col="#61D04F")
points(x_m, y_m, pch=19, col="#61D04F")

legend("topleft", legend=c("Sharpe", "MSE"),
       col=c("#DF536B", "#61D04F"), lty=c(1,1), cex=0.8,
       bty = "n")

legend("topright", legend="Drawdown",
       col="#2297E6", lty=1, cex=0.8,
       bty = "n")
for (i in 0:13) {
  text((1+500-5)/2 + seper*i, -2.5, paste(2003+i, "-01-01", sep=""), cex=0.6)
}

# Ind 3 Opt-Plot####
perfplot_sma_opt(xall=ind.lr, insamp="2019-01-01", res_obj=res3, inx=3)


#.####
# Ind 4####
res4 <- loop_sma(x=ind.lr, inx=4, insamp = "2019-01-01", minyear=3, maxyear=16)
res4$res_df[,2] <- as.numeric(res4$res_df[,2])
res4$res_df[,3] <- as.numeric(res4$res_df[,3])
res4$res_df[,4] <- as.numeric(res4$res_df[,4])
res4$res_df[,5] <- as.numeric(res4$res_df[,5])
res4$res_df[,6] <- as.numeric(res4$res_df[,6])
res4$res_df[,7] <- as.numeric(res4$res_df[,7])

res4$lirino[,1] <- as.numeric(res4$lirino[,1])
res4$lirino[,2] <- as.numeric(res4$lirino[,2])
res4$lirino[,3] <- as.numeric(res4$lirino[,3])
res4$lirino[,4] <- as.numeric(res4$lirino[,4])
#save(res4, file="data/R_Files/optim_sma_4.RData")

# Plot
seper <- (500-5)+1
x_s <- res4$res_df[,3] + 1 + (0:13*seper)
y_s <- res4$res_df[,2]

x_d <- res4$res_df[,5] + 1 + (0:13*seper)
y_d <- res4$res_df[,4]

x_m <- res4$res_df[,7] + 1 + (0:13*seper)
y_m <- res4$res_df[,6]

plot(x=1:length(res4$lirino[,2]), y=res4$lirino[,2], type="l", col="#DF536B", main="Performance Indicators | SMA | Index 4", xlab = "L", ylab="", xaxt = "n", ylim=c(-3, 2))
axis(1, at=c(1, seper, 1+2*seper, 2*seper+seper), labels=c("5", "500", "5", "500"))
axis(3, at=c(1+seper, seper+seper), labels=c("5", "500"))
axis(4, at=c(0.5, 0, -0.5, -1, -1.5), labels=c("0", "-0.1", "-0.2", "-0.3", "-0.4"))

points(x_s, y_s, pch=19, col="#DF536B")

abline(v=1, lty=2, col="black")
abline(v=1+(500-5), lty=2, col="black")

abline(h=0.5, lty=2, col="black")
abline(h=0, lty=2, col="black")


lines((res4$lirino[,3])*5+0.5, type="l", col="#2297E6", lty=1)
points(x_d, y_d*5+0.5, pch=19, col="#2297E6")

lines(res4$lirino[,4], type="l", col="#61D04F")
points(x_m, y_m, pch=19, col="#61D04F")

legend("topleft", legend=c("Sharpe", "MSE"),
       col=c("#DF536B", "#61D04F"), lty=c(1,1), cex=0.8,
       bty = "n")

legend("topright", legend="Drawdown",
       col="#2297E6", lty=1, cex=0.8,
       bty = "n")
for (i in 0:13) {
  text((1+500-5)/2 + seper*i, -3, paste(2003+i, "-01-01", sep=""), cex=0.6)
}

# Ind 4 Opt-Plot####
perfplot_sma_opt(xall=ind.lr, insamp="2019-01-01", res_obj=res4, inx=4)






#.####
# Trading with invest####
loop_sma_trading <- function(x, inx, min_L=5, max_L=500, insamp = "2018-12-31", invest_amount=1000000, tradingcosts=190) {
  
  
  filler <- 1
  res_mat <- matrix(1:((length(min_L:max_L))*4), ncol=4,
                    dimnames = list(1:(length(min_L:max_L)), c("L", "SMA-Return", "Trades", "Sharpe")))
  
  pb <- txtProgressBar(min = min_L, max = max_L, style = 3)
  
  for (L in min_L:max_L) {
    # SMA L<-328
    sma_full <- SMA(x[,inx], n=L)
    
    # Cut init
    sma_cut <- sma_full[paste(insamp, "/", sep="")]
    
    # Perf (daily)
    perf <- lag(sign(sma_cut))*x[,inx][paste(insamp, "/", sep="")]
    trade_sig <- lag(sign(sma_cut))
    
    # Count Trades
    sma_trade_count <- trading_counter(as.numeric(na.exclude(trade_sig)))
    
    # Sharpe
    sharpe <- sharpe_fun(na.exclude(perf))
    
    sma_ret <- tail((cumsum(na.exclude(perf)) * invest_amount) - (sma_trade_count * tradingcosts), 1)
    bnh_ret <- cumsum(x[,inx][paste(insamp, "/", sep="")]) * invest_amount
    
    
    res_mat[filler,] <- c(L, sma_ret, sma_trade_count, sharpe)
    
    filler <- filler + 1
    
    setTxtProgressBar(pb, L)
  }
  close(pb)
  res_df <- as.data.frame(res_mat)
  return(res_df)
}

plot_sma <- function(x, inx, L, insamp = "2018-12-31") {
  sma_full <- SMA(x[,inx], n=L)
  
  # Cut init
  sma_cut <- sma_full[paste(insamp, "/", sep="")]
  
  # Perf (daily)
  perf <- lag(sign(sma_cut))*x[,inx][paste(insamp, "/", sep="")]
  trade_sig <- lag(sign(sma_cut))
  
  # Trade Counter
  sma_trade_count <- trading_counter(as.numeric(na.exclude(trade_sig)))
  
  # Sharpe
  bnh_sharpe <- round(sharpe_fun(x[,inx]["2019-01-01/"]), 3)
  trade_sharpe <- round(sharpe_fun(na.exclude(perf)), 3)
  
  bnh_perf <- cumsum(x[,inx]["2019-01-01/"])
  trade_perf <- cumsum(na.exclude(perf))
  
  ymin <- min(c(min(bnh_perf), min(trade_perf)))
  ymax <- max(c(max(bnh_perf), max(trade_perf)))
  
  plot(bnh_perf, main=paste("Out-of-sample-Index: ",inx), lwd=2,
       ylim=c(ymin-0.1*abs(ymin), ymax+0.1*ymax))
  lines(trade_perf, col="#E413A3", lty=2, lwd=2)

  
  addLegend("topleft", legend.names = c(paste("Buy & Hold | Sharpe=", bnh_sharpe),
                                        paste("Trading-Opt.=",trade_sharpe, "| L=",L, "| Trades n=", sma_trade_count)),
            lty=c(1, 2),
            lwd=c(2, 2),
            col=c("#000000", "#E413A3"),
            cex=0.8)
  
  lines(trade_sig, on=NA, ylim=c(-1.5, 1.5), col="#E413A3", lwd=2)
}




# 1####
res_trade_1 <- loop_sma_trading(x=ind.lr, inx=1, min_L=5, max_L=500, insamp = "2018-12-31", invest_amount=1000000, tradingcosts=190)

sharpe_fun(ind.lr[, 1]["2019-01-01/"])
# 3.504957

bnh_trading_ret_1 <- as.numeric(tail(cumsum(ind.lr[, 1]["2019-01-01/"]) * 1000000, 1) - 190)
# Buy and Hold: 63642.12

sum(res_trade_1[,2] > bnh_trading_ret_1)
# 0
res_trade_1[,2] > bnh_trading_ret_1
res_trade_1[res_trade_1[,2] > bnh_trading_ret_1, ]



# 2####
res_trade_2 <- loop_sma_trading(x=ind.lr, inx=2, min_L=5, max_L=500, insamp = "2018-12-31", invest_amount=1000000, tradingcosts=190)

sharpe_fun(ind.lr[, 2]["2019-01-01/"])
# 2.475576

bnh_trading_ret_2 <- as.numeric(tail(cumsum(ind.lr[, 2]["2019-01-01/"]) * 1000000, 1) - 190)
# Buy and Hold: 105865.3

sum(res_trade_2[,2] > bnh_trading_ret_2)
# 5
res_trade_2[,2] > bnh_trading_ret_2

#   L    SMA-Return   Trades   Sharpe
#   40   106720.3     11       2.541531
#   42   109481.3     13       2.616882
#  353   107075.7      3       2.513635
#  354   110658.9      3       2.599536
#  357   106337.8      3       2.495972
ind2_outperf <- res_trade_2[res_trade_2[,2] > bnh_trading_ret_2, ]
plot(res_trade_2$Sharpe, type="l", main="SMA-Trading-Optimisation | Index 3", ylab="Sharpe Ratio", xlab="L", xaxt="n", col="#DF536B")
axis(1, at=c(1, 96, 196, 296, 396, 496), labels=c("5", "100", "200", "300", "400", "500"))
points(ind2_outperf$L-4, ind2_outperf$Sharpe, pch=19, col="#DF536B")



trade_opt_2 <- as.numeric(res_trade_2[res_trade_2[,2] > bnh_trading_ret_2, ][,1])
for (L in trade_opt_2) {
  print(plot_sma(x=ind.lr, inx=2, L=L, insamp = "2018-12-31"))
}

# plot_sma(x=ind.lr, inx=2, L=40, insamp = "2018-12-31")
# plot_sma(x=ind.lr, inx=2, L=42, insamp = "2019-01-01")
# plot_sma(x=ind.lr, inx=2, L=353, insamp = "2019-01-01")
# plot_sma(x=ind.lr, inx=2, L=354, insamp = "2019-01-01")
# plot_sma(x=ind.lr, inx=2, L=357, insamp = "2019-01-01")



# 3####
res_trade_3 <- loop_sma_trading(x=ind.lr, inx=3, min_L=5, max_L=500, insamp = "2019-01-01", invest_amount=1000000, tradingcosts=190)

sharpe_fun(ind.lr[, 3]["2019-01-01/"])
# 2.155228

bnh_trading_ret_3 <- as.numeric(tail(cumsum(ind.lr[, 3]["2019-01-01/"]) * 1000000, 1) - 190)
# Buy and Hold: 143100.6

sum(res_trade_3[,2] > bnh_trading_ret_3)
# 10
res_trade_3[,2] > bnh_trading_ret_3
res_trade_3[res_trade_3[,2] > bnh_trading_ret_3, ]
#   L    SMA-Return   Trades   Sharpe
#   39   150456.0     13       2.306516
#   40   159343.2     13       2.443678
#   41   150723.3     11       2.304779
#   42   155602.6     11       2.380016
#  347   143347.6      3       2.168001
#  348   143830.0      4       2.178323
#  352   147161.7      3       2.226586
#  383   144498.8      3       2.185674
#  384   146086.0      3       2.210054
#  385   144307.9      3       2.182743

ind3_outperf <- res_trade_3[res_trade_3[,2] > bnh_trading_ret_3, ]
plot(res_trade_3$Sharpe, type="l", main="SMA-Trading-Optimisation | Index 3", ylab="Sharpe Ratio", xlab="L", xaxt="n", col="#DF536B")
axis(1, at=c(1, 96, 196, 296, 396, 496), labels=c("5", "100", "200", "300", "400", "500"))
points(ind3_outperf$L-4, ind3_outperf$Sharpe, pch=19, col="#DF536B")


trade_opt_3 <- as.numeric(res_trade_3[res_trade_3[,2] > bnh_trading_ret_3, ][,1])
for (L in trade_opt_3) {
  print(plot_sma(x=ind.lr, inx=3, L=L, insamp = "2019-01-01"))
}

# 4####
res_trade_4 <- loop_sma_trading(x=ind.lr, inx=4, min_L=5, max_L=500, insamp = "2019-01-01", invest_amount=1000000, tradingcosts=190)

sharpe_fun(ind.lr[, 4]["2019-01-01/"])
# 1.944616

bnh_trading_ret_4 <- as.numeric(tail(cumsum(ind.lr[, 4]["2019-01-01/"]) * 1000000, 1) - 190)
# Buy and Hold: 184467.3

sum(res_trade_4[,2] > bnh_trading_ret_4)
# 8
res_trade_4[,2] > bnh_trading_ret_4
res_trade_4[res_trade_4[,2] > bnh_trading_ret_4, ]
#   L    SMA-Return   Trades   Sharpe
#   40   189309.3     17       2.031925
#   42   195937.3     11       2.090842
#   45   185488.6     11       1.978739
#  347   185816.7      4       1.968006
#  402   185573.5      3       1.963366
#  407   185703.7      9       1.976972
#  408   185543.1      5       1.967111
#  414   188192.7      3       1.991428


ind4_outperf <- res_trade_4[res_trade_4[,2] > bnh_trading_ret_4, ]
plot(res_trade_4$Sharpe, type="l", main="SMA-Trading-Optimisation | Index 4", ylab="Sharpe Ratio", xlab="L", xaxt="n", col="#DF536B")
axis(1, at=c(1, 96, 196, 296, 396, 496), labels=c("5", "100", "200", "300", "400", "500"))
points(ind4_outperf$L-4, ind4_outperf$Sharpe, pch=19, col="#DF536B")

trade_opt_4 <- as.numeric(res_trade_4[res_trade_4[,2] > bnh_trading_ret_4, ][,1])
for (L in trade_opt_4) {
  print(plot_sma(x=ind.lr, inx=4, L=L, insamp = "2019-01-01"))
}
