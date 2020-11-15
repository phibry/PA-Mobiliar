par(mfrow=c(2,1))
acf(x1) 
acf(x1, type="partial") 

obj = arima(x1, order=c(0, 0, 3))
tsdiag(obj)
aic_bic(x1)


library(quantmod)
GOOGL <- getSymbols("GOOGL", auto.assign=F)
g.adj <- GOOGL[,6]
par(mfrow=c(1,1))
plot(g.adj, main="Adjusted Prices ~ GOOGL")


g.adj.lr <- na.exclude(diff(log(g.adj)))
plot(g.adj.lr, main="Log-Returns of Adj. Prices ~ GOOGL")


par(mfrow=c(1,2))
acf(g.adj, main="Adjusted Prices ~ GOOGL")
acf(g.adj.lr, main="LogReturns ~ GOOGL")


# ARMA
aic_bic <- function(x) {
  len <- length(x)
  maxarorder<-5
  maxmaorder<-5
  sigma_jk<-matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                   ncol=maxmaorder+1,nrow=maxarorder+1)
  
  sigma_jk[1,1] <- var(x)
  arma_mat <- matrix(rep(0,(maxmaorder+1)*(maxarorder+1)),
                     ncol=5, nrow=(maxmaorder+1)*(maxarorder+1))
  
  dimnames(arma_mat)[[2]] <- c("AR", "MA", "R-AIC", "AIC", "BIC")
  dimnames(arma_mat)[[1]] <- seq(1,36,1)
  
  iter <- 1
  for (j in 0:maxarorder)
  {
    for (k in 0:maxmaorder)
    {
      ARMA_obj<-arima(x,order=c(j,0,k))
      sigma_jk[j+1,k+1] <- ARMA_obj$sigma
      arma_mat[iter,1] <- j
      arma_mat[iter,2] <- k
      arma_mat[iter,3] <- ARMA_obj$aic
      iter <- iter + 1
    }
  }
  
  
  log_sigma_jk <- log(sigma_jk)
  
  iter <- 1
  for (j in 0:maxarorder)
  {
    for (k in 0:maxmaorder)
    {
      arma_mat[iter, 4] <- log_sigma_jk[j+1,k+1]+2*(j+k)/len
      arma_mat[iter, 5] <- log_sigma_jk[j+1,k+1]+log(len)*(j+k)/len
      
      iter <- iter + 1
    }
  }
  
  # ARIMA-AIC
  r_aic <- arma_mat[which(arma_mat==min(arma_mat[,3]), arr.ind=TRUE)[1],][1:2]
  r_aic_val <- arma_mat[which(arma_mat==min(arma_mat[,3]), arr.ind=TRUE)[1],][3]
  
  # Theoretical AIC
  t_aic <- arma_mat[which(arma_mat==min(arma_mat[,4]), arr.ind=TRUE)[1],][1:2]
  t_aic_val <- arma_mat[which(arma_mat==min(arma_mat[,4]), arr.ind=TRUE)[1],][4]
  
  # Theoretical BIC
  t_bic <- arma_mat[which(arma_mat==min(arma_mat[,5]), arr.ind=TRUE)[1],][1:2]
  t_bic_val <- arma_mat[which(arma_mat==min(arma_mat[,5]), arr.ind=TRUE)[1],][5]
  
  return(list("R-AIC"=c(r_aic, r_aic_val), "AIC"=c(t_aic, t_aic_val), "BIC"=c(t_bic, t_bic_val)))
}

par(mfrow=c(2,1))
acf(g.adj) 
acf(g.adj, type="partial")

par(mfrow=c(2,1))
acf(g.adj.lr) 
acf(g.adj.lr, type="partial")

aic_bic(g.adj.lr)


obj = arima(g.adj, order=c(1, 1, 1))
tsdiag(obj)

obj = arima(g.adj.lr, order=c(1, 0, 1))
tsdiag(obj)
?tsdiag

aic_bic(g.adj.lr)


library(PerformanceAnalytics)
par(mfrow=c(1,2))
chart.ACF(g.adj.lr, maxlag=20)

acf(g.adj)
chart.ACF(g.adj, maxlag=20)


par(mfrow=c(3,1))
plot(g.adj)
plot(g.adj.lr)
plot(cumsum(g.adj.lr))
