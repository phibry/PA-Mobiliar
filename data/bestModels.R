# Buy and Hold, 2019-01-01 Out-Of-Sample####
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

  plot(perf_bnh, main=paste("Startdate: ",start_date," Index: ",inx), lwd=1.5,
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
# Best Model
opt_ar_1[which.max(opt_ar_1[,2]),]







## Index2####
opt_ar_2 <- optim_ar(x.lr=ind.lr, inx=2)
opt_ar_2
# Best Model
opt_ar_2[which.max(opt_ar_2[,2]),]







## Index3####
opt_ar_3 <- optim_ar(x.lr=ind.lr, inx=3)
opt_ar_3
# Best Model
opt_ar_3[which.max(opt_ar_3[,2]),]







## Index4####
opt_ar_4 <- optim_ar(x.lr=ind.lr, inx=4)
opt_ar_4
# Best Model
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
