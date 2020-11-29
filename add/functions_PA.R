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
  abline(h=0.05, lty=2, col="blue")
}


ljungplotGarch <- function(resid, sig) {
  p1 <- rep(NA, 20)
  p2 <- rep(NA, 20)
  for (i in 1:20) {
    p1[i] <- Box.test(resid/sig, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
    p2[i] <- Box.test((resid/sig)^2, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
  }
  
  par(mfrow=c(1,2))
  plot(x=1:20, y=p1, main=expression(paste("Ljung-Box statistic: ", û)),
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="blue")
  points(x=10, y=p1[10], pch=16, col="red")
  points(x=15, y=p1[15], pch=16, col="red")
  points(x=20, y=p1[20], pch=16, col="red")
  
  plot(x=1:20, y=p2, main=expression(paste("Ljung-Box statistic: ", û^2)),
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="blue")
  points(x=10, y=p2[10], pch=16, col="red")
  points(x=15, y=p2[15], pch=16, col="red")
  points(x=20, y=p2[20], pch=16, col="red")
}



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


performante <- function(xin, xout, threshold_p=0.7, main="") {
  
  ## AR-Model-Order
  testerino <- rep(0, 10)
  j <- 1
  while (!all(testerino > threshold_p)) {
    # fit an ar-model
    arma_obj <- arima(xin, order=c(j,0,0))
    
    # estiamte the Ljung-Box statistics
    for (i in 1:10) {
      testerino[i] <- Box.test(arma_obj$residuals, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
    }
    j <- j + 1
  }
  ##
  ##
  ##
  ##
  ## AR-Prediction
  mat_out <- cbind(xout)
  for (k in 1:(j-1)) {
    mat_out <- cbind(mat_out, lag(xout, k=k))
  }
  ar_pred <- arma_obj$coef[length(arma_obj$coef)] +
    as.matrix(mat_out[,2:ncol(mat_out)])%*%arma_obj$coef[1:(length(arma_obj$coef)-1)]
  ##
  ##
  ##
  ##
  ## 
  ## MGARCH
  garch_11 <- garchFit(~garch(1,1), data=xin, delta=2,
                       include.delta=F, include.mean=F, trace=F)
  ##
  ##
  ##
  ##
  ## 
  # Sigma_t in-sample
  sigma_t_in <- garch_11@sigma.t
  names(sigma_t_in) <- index(xin)
  sigma_t_in <- as.xts(sigma_t_in)
  index(sigma_t_in) <- index(xin)
  ##
  ##
  ##
  ##
  ## 
  # Regression
  lm_obj <- lm(xin ~ sigma_t_in)
  ##
  ##
  ##
  ##
  ## 
  # Sigma_t out of sample
  sigma_t_out <- compute_sigma_t_out_of_sample_func(xout, garch_11, xin)$sigma_t
  ##
  ##
  ##
  ##
  ## 
  # MGARCH-Prediction
  mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t_out
  ##
  ##
  ##
  ##
  ## 
  ## Trading: Signum
  ret_arima <- sign(ar_pred)*xout
  returns_mgarch <- sign(mgarch_predict)*xout
  ##
  ##
  ##
  ##
  ##   
  # Sharpe
  sharpe_bnh <- as.double(sqrt(250) * mean(xout) / sqrt(var(xout)))
  sharpe_ar <- as.double(sqrt(250) * mean(ret_arima, na.rm=T) / sqrt(var(ret_arima, na.rm=T)))
  sharpe_mgarch <- as.double(sqrt(250) * mean(returns_mgarch, na.rm=T) / sqrt(var(returns_mgarch, na.rm=T)))
  ##
  ##
  ##
  ##
  ## 
  # Plot
  #par(mfrow=c(1,1))
  (plot(cumsum(xout), main=main, lwd=1.5))
  (lines(cumsum(na.exclude(ret_arima)), lty=2, lwd=1, col="red"))
  (lines(cumsum(na.exclude(returns_mgarch)), lty=3, col="blue"))
  
  
  print(addLegend("topleft", legend.names = c(paste("Buy & Hold:", round(sharpe_bnh, 2)),
                                              paste("AR(",j-1,"):", round(sharpe_ar, 2)),
                                              paste("MGARCH(1,1):", round(sharpe_mgarch, 2))),
                  lty=c(1, 2, 3),
                  lwd=c(1.5, 1, 1),
                  col=c("black", "red", "blue"),
                  cex=0.8))
}