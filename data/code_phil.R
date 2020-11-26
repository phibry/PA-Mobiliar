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

plot(g.adj, main="Adjusted Prices ~ GOOGL")
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

chart.ACFplus(g.adj, maxlag=20, main="Adjusted Prices ~ Google", elementcolor = "gray")
chart.ACF(g.adj, maxlag=20, main="LogReturns ~ Google", type="partial")

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

?chart.ACF
par(mfrow=c(3,1))
plot(g.adj)
plot(g.adj.lr)
plot(cumsum(g.adj.lr))



library(forecast)
library(PerformanceAnalytics)
x <- g.adj["2010-01-01/"]
h <- 1

# For Visualization
in_sample <- x[1:(length(x)-h)]
out_sample <- x[((length(x)-h)+1):(length(x))]

# find best model
fit <- auto.arima(x)
tsdiag(fit)
fit$residuals

ljungplot <- function(resid, lag=10) {
  testerino <- rep(NA, lag)
  for (i in 1:lag) {
    testerino[i] <- Box.test(resid, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
  }
  
  plot(x=1:lag, y=testerino, main="Ljung-Box statistic",
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="blue")
}

ljungplot(fit$residuals)

?box
?axis
chart.ACF(x)
plot(x=1:10, y=testerino, main="Ljung-Box statistic",
     xlab = "lag", ylab = "p value")
?barplot
?type
# perform a forecast
fore <- forecast(fit, h=h)
fit

# Create xts-Object for the forecast
fcast <- data.frame("LowerBand" = fore$lower[,2],
                    "UpperBand" = fore$upper[,2],
                    "PointForecast" = fore$mean)
rownames(fcast) <- index(out_sample)
fcast <- as.xts(fcast)
index(fcast) <- index(out_sample)
fcast <- merge(fcast, out_sample)


x[index(tail(x, h))] <- NA

plot(tail(x, 10), ylim=c(1550, 1800), main="ARIMA(1,1,0) Forecast")
points(fcast[,1], col="red", pch=16)
points(fcast[,2], col="red", pch=16)
points(fcast[,3], col="blue", pch=16)
points(fcast[,4], col="green", pch=16)









# GARCH GOOGLE####
par(mfrow=c(2,1))
plot(g.adj, main="Adjusted Prices ~ GOOGL")
plot(g.adj.lr, main="Log-Returns of Adj. Prices ~ GOOGL")

# ACF
par(mfrow=c(1,2))
acf(g.adj, main="Adjusted Prices ~ GOOGL")
acf(g.adj.lr, main="LogReturns ~ GOOGL")


# ACF
par(mfrow=c(3,1))
chart.ACF(g.adj.lr, main="logReturns", maxlag=30)
chart.ACF(g.adj.lr^2, main="logReturns^2", maxlag=30)
chart.ACF(abs(g.adj.lr), main="abs(logReturns)", maxlag=30)

# Modell
library(fGarch)
library(PerformanceAnalytics)

#g.subset.lr <- g.adj.lr["2010-01-01/"]
g.subset.lr <- head(g.adj.lr, length(g.adj.lr)-100)

y.garch_11 <- garchFit(~garch(1,1), data=g.subset.lr, delta=2, include.delta=F, 
                       include.mean=F, trace=F)

summary(y.garch_11)

y.garch_11@title

y.garch_11@fit$coef
#omega       alpha1        beta1 
#1.110707e-05 7.473398e-02 8.936598e-01 

(y.garch_11@fit$matcoef)
y.garch_11@fit$matcoef[,1]

estimate <- round(y.garch_11@fit$matcoef[,1], 6)
stderror <- round(y.garch_11@fit$matcoef[,2], 6)
(pvalue <- round(y.garch_11@fit$matcoef[,4], 16))

paras <- data.frame(estimate, stderror, pvalue)
colnames(paras) <- c("Estimate", "Std. Error", "p-Value")
rownames(paras) <- c("$\\omega$", "$\\alpha_{1}$", "$\\beta_{1}$")


y.garch_11@residuals
par(mfrow=c(2,1))
ljungplot(y.garch_11@residuals/y.garch_11@sigma.t, lag=20)
ljungplot((y.garch_11@residuals/y.garch_11@sigma.t)^2, lag=20)



ljungplotGarch <- function(resid, sig) {
  p1 <- rep(NA, 20)
  p2 <- rep(NA, 20)
  for (i in 1:20) {
    p1[i] <- Box.test(resid/sig, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
    p2[i] <- Box.test(resid/sig^2, lag = i, type = c("Ljung-Box"), fitdf = 0)$p.value
  }
  
  par(mfrow=c(1,2))
  plot(x=1:20, y=p1, main=expression(paste("Ljung-Box statistic: ", R)),
       xlab = "lag", ylab = "p value", axes = FALSE,
       ylim=c(0,1))
  box(col="gray")
  axis(2, col="gray", cex.axis=0.8)
  axis(1, col="gray", cex.axis=0.8)
  abline(h=0.05, lty=2, col="blue")
  points(x=10, y=p1[10], pch=16, col="red")
  points(x=15, y=p1[15], pch=16, col="red")
  points(x=20, y=p1[20], pch=16, col="red")
  
  plot(x=1:20, y=p2, main=expression(paste("Ljung-Box statistic: ", R^2)),
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


ljungplotGarch(y.garch_11@residuals, y.garch_11@sigma.t)
chart.ACF(y.garch_11@residuals/y.garch_11@sigma.t)
pacf(y.garch_11@residuals/y.garch_11@sigma.t)


# Summe der Parameter
y.garch_11@fit$coef["alpha1"] + y.garch_11@fit$coef["beta1"]


# Stand. Residuals
eps <- y.garch_11@residuals
eps
# u's
u <- eps/y.garch_11@sigma.t

par(mfrow=c(2,1))
ts.plot(eps, main="epsilon"); abline(h=0)
ts.plot(u, main="u"); abline(h=0)

# ACF u and eps
par(mfrow=c(2,2))
chart.ACF(eps, main="eps")
chart.ACF(eps^2, main="eps^2")
chart.ACF(u, main="u")
chart.ACF(u^2, main="u^2")

# eps zeigen die Cluster, Abhängigkeitstrukturen
# us eigen white noise



# vola####
var(g.adj.lr)
var(y.garch_11@residuals)

# Ist besser wenn die Parameterrestriktionen erfüllt sind
# Schätzer für die unbedingte Varianz wenn die Parameterrestriktionen erfüllt sind
omega <- y.garch_11@fit$coef["omega"]
names(omega) <- NULL

alpha1 <- y.garch_11@fit$coef["alpha1"]
names(alpha1) <- NULL

beta1 <- y.garch_11@fit$coef["beta1"]
names(beta1) <- NULL

sig <- sqrt(y.garch_11@fit$coef["omega"]/(1-y.garch_11@fit$coef["alpha1"]-y.garch_11@fit$coef["beta1"]))

c <- omega/sig^2
c
1-alpha1-beta1

restr <- c + alpha1 + beta1
restr

sig
# GARCH-Forecast####
h <- 20
y.garch_11.pred <- predict(y.garch_11, n.ahead=h)



fcast_grach <- data.frame("MeanForecast" = y.garch_11.pred[,1],
                          "MeanError" = y.garch_11.pred[,2],
                          "Std.Deviation" = y.garch_11.pred[,3])
rownames(fcast_grach) <- index(tail(g.adj.lr["2010-01-01/"], h))
fcast_grach <- as.xts(fcast_grach)
index(fcast_grach) <- index(tail(g.adj.lr["2010-01-01/"], h))


tail(g.adj.lr["2010-01-01/"], h)

par(mfrow=c(1,1))
plot(g.adj.lr["2020-01-01/"], main="GARCH(1,1)")
lines(tail(g.adj.lr["2020-01-01/"], h), col="green", lwd=1.5)
lines(fcast_grach[,1], col="blue", lwd=1.5)
lines(fcast_grach[,1] + 1.96 * fcast_grach[,3], col="red", lwd=1.5)
lines(fcast_grach[,1] - 1.96 * fcast_grach[,3], col="red", lwd=1.5)



tail(g.adj.lr["2020-01-01/"])
tail(fcast_grach[,1])


# Create a list of Dates, longer than forecast horizon (We snip weekends later)
future <- seq(as.Date(tail(index(g.adj.lr),1)+1), length=h*2, by="days")

#we <- sapply(future, function(d) {as.POSIXlt(d)$wday}) %in% c(0,6)

# Snip weekends
future <- future[! sapply(future, function(d) {as.POSIXlt(d)$wday}) %in% c(0,6)]

# Length of the forecast horizon
future <- head(future, h)

# Create an xts object with the prediction
fcast_grach <- data.frame("MeanForecast" = y.garch_11.pred[,1],
                          "MeanError" = y.garch_11.pred[,2],
                          "Std.Deviation" = y.garch_11.pred[,3])
rownames(fcast_grach) <- future
fcast_grach <- as.xts(fcast_grach)

# Create a fake series for the forecast
fakeNA <- xts(rep(NA, h), index(fcast_grach))

plot(rbind(g.adj.lr["2018-01-01/"], as.matrix(fakeNA)), main="GARCH(1,1)")
lines(fcast_grach[,1], col="blue")
lines(fcast_grach[,1] + 1.96 * fcast_grach[,3], col="red")
lines(fcast_grach[,1] - 1.96 * fcast_grach[,3], col="red")





x <- na.exclude(g.adj["2010-01-01/"])
h <- 100

# For Visualization
in_sample <- x[1:(length(x)-h)]
out_sample <- x[((length(x)-h)+1):(length(x))]

# perform a forecast
fore <- forecast(fit, h=h)

# Create xts-Object for the forecast
fcast <- data.frame("LowerBand" = fore$lower[,2],
                    "UpperBand" = fore$upper[,2],
                    "PointForecast" = fore$mean)
rownames(fcast) <- index(out_sample)
fcast <- as.xts(fcast)
index(fcast) <- index(out_sample)


x[index(tail(x, h))] <- NA

plot(tail(x, 200), ylim=c(500, 2000), main="ARIMA(1,1,0)")
lines(fcast[,1], col="red", pch=16)
lines(fcast[,2], col="red", pch=16)
lines(fcast[,3], col="blue", pch=16)
lines(out_sample, col="green", pch=16)



#.####
# First Analysis####

# Intro####
# Buy-and-Hold
# ARMA
# ARMA-GARCH
# MGARCH
# Position-sizing
# Equally-weighted ptf

# 3m, 6m, 1y, 2y, 3y, 5y, 7y, 10y,
# Imports####
library(xts)
load("data/data_mobi")

load("/Users/phili/Desktop/7.Sem/PA-Mobiliar/data/data_mobi")

head(data)

# Split Dataframe into index-part and interest-part
par(mfrow=c(1,1))
ind <- data[,1:4]
int <- data[,5:12]

plot(int, lwd=1)

## Create Table
summary(int)
# 3m, 6m, 1y, 2y, 3y, 5y, 7y, 10y,
int.maturity <- c("3m", "6m", "1y", "2y", "3y", "5y", "7y", "10y")
int.mean <- round(apply(int, MARGIN=2, FUN=mean), 2)
int.sd <- round(apply(int, MARGIN=2, FUN=sd), 2)

int.table <- data.frame(int.maturity, int.mean, int.sd)
colnames(int.table) <- c("Maturity", "Mean", "Volatility")

int.table

# First Look####
## Plot Index
# Creating Color scheme with rainbow()
par(mfrow=c(1,1))
ts.plot(ind, main="Indexes 1-4", col=2:5, lwd=1)
for (i in 1:ncol(ind)) {
  i
  mtext(paste("Index ", i), side=3, line=-i,col=i+1)
}
index(head(ind,5))
index(tail(ind,5))
nrow(ind)
ind["2019-01-01/"]

par(mfrow=c(1,1))
par(bg = "yellow")
plot(ind["2019-01-01/"], main="Indexes 1-4", col=2:5, lwd=1, grid.col = NA)
for (i in 1:ncol(ind)) {
  i
  mtext(paste("Index ", i), side=3, line=-i,col=i+1)
}

apply(ind, MARGIN = 2, FUN = sd)

# Index               Index 1         Index 2         
# Min.   :2003-10-30   Min.   :606.3   Min.   : 683.7  
# 1st Qu.:2007-12-14   0   Median :781.5   Median :1019.3  
# Mean   :2012-01-29   Mean   :748.9   Mean   : 950.7  
# 3rd Qu.:2016-03-15   3rd Qu.:803.1   3rd Qu.:1077.7  
# Max.   :2020-04-30   Max.   :874.3   Max.   :1231.4 

# Index               Index 3          Index 4      
# Min.   :2003-10-30   Min.   : 722.4   Min.   : 730.0  
# 1st Qu.:2007-12-14   1st Qu.: 872.0   1st Qu.: 903.1  
# Median :2012-01-30   Median :1163.1   Median :1252.9  
# Mean   :2012-01-29   Mean   :1076.7   Mean   :1160.7  
# 3rd Qu.:2016-03-15   3rd Qu.:1257.3   3rd Qu.:1387.2  
# Max.   :2020-04-30   Max.   :1504.9   Max.   :1738.7 

# At start (2003-10-30) the 4 series are about the same level
# somewhere between 600 and 750. The 4 series' diverged over time

# All 4 series' act the same, they tend to change about the same,
# they only differ in how strong the change is.
# All 4 series' are probably in the same market. The series' change if smth
# happens in this market.
# Index 4 have seen the most growth, from 730 to 1739 while Index 1
# shows the smallest growth (from 606 to 874).
# All move the same, but their volatility differs
# This makes sense, Index 1 is the "safest" asset with a very low sd()
apply(FUN=sd, ind, MARGIN=2)
# compared to the sd of Index 4. But lower volatility means often lower returns.

# Strong drift in the series, include.mean=T for garchfit-models
# mu could be significant

## LogReturns
ind.lr <- na.exclude(diff(log(ind)))
summary(ind.lr)
par(mfrow=c(2,2))
plot(ind.lr[,1], main=paste("Index", 1))
plot(ind.lr[,2], main=paste("Index", 2))
plot(ind.lr[,3], main=paste("Index", 3))
plot(ind.lr[,4], main=paste("Index", 4))

# All LogReturns show the same pattern. In 2008 we can see high volatility
# cluster due to the global recession.

ind.lr.1 <- ind.lr[,1]
ind.lr.2 <- ind.lr[,2]
ind.lr.3 <- ind.lr[,3]
ind.lr.4 <- ind.lr[,4]

ind.1 <- ind[,1]
ind.2 <- ind[,2]
ind.3 <- ind[,3]
ind.4 <- ind[,4]

startdate <- "2010-01-01"
insample <- "2017-01-01"

ind.all <- ind[paste(startdate,"/",sep="")]
ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]

ind.lr.in <- ind.lr.all[paste("/",insample,sep="")]
ind.in <- ind.all[paste(startdate,"/",sep="")]

ind.lr.out <- ind.lr.all[paste(insample,"/",sep="")]
ind.out <- ind.all[paste(startdate,"/",sep="")]

# 1. Index 1####
par(mfrow=c(2,1))
plot(ind1, main="Index 1: LogReturns")
plot(ind[,1], main="Index 1: TimeSeries")
# We can see volatility clusters. (2008, Great Recession)
# Upward drift in the bottom plot


# 1.1 Subset####
startdate <- "2015-01-01"
insample <- "2018-01-01"

# time-window
ind1.all <- ind1[paste(startdate,"/",sep="")]

# in-sample
ind1.in <- ind1.all[paste("/",insample,sep="")]
head(ind1.in)
# out-of sample
ind1.out <- ind1.all[paste(insample,"/",sep="")]




#.####
#.####
#.####
#.####
source("add/libraries.R")
source("add/functions_PA.R")
load("data/data_mobi")


ind <- na.exclude(data[,1:4])
ind.lr <- na.exclude(diff(log(ind)))

plot(ind)
plot(ind.lr)


# Sample-Sizing
# Can be iterated to find the best model
startdate <- "2018-01-01"
insample <- "2019-01-01"

# time-window
ind.all <- ind[paste(startdate,"/",sep="")]
ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]

# in-sample
ind.in <- ind.all[paste("/",insample,sep="")]
ind.lr.in <- ind.lr.all[paste("/",insample,sep="")]

# out-of sample
ind.out <- ind.all[paste(insample,"/",sep="")]
ind.lr.out <- ind.lr.all[paste(insample,"/",sep="")]

head(ind.all)
tail(ind.all)

head(ind.in)
tail(ind.in)

head(ind.out)
tail(ind.out)


## Models####
## Index 1####
## ARIMA
ind.lr.out[,1]

chart.ACFplus(ind.lr.in[,1])
pacf(ind.lr.in[,1])

ind1.arma <- arima(ind.lr.in[,1], order=c(1,0,0))
tsdiag(ind1.arma)
ind1.arma$coef

mat_out1 <- cbind(ind.lr.out[,1],
                  lag(ind.lr.out[,1], k=1))
head(mat_out1, 10)

mat_out1 <- cbind(ind.lr.out[,1],
                  lag(ind.lr.out[,1], k=1),
                  lag(ind.lr.out[,1], k=2),
                  lag(ind.lr.out[,1], k=3),
                  lag(ind.lr.out[,1], k=4),
                  lag(ind.lr.out[,1], k=5),
                  lag(ind.lr.out[,1], k=6),
                  lag(ind.lr.out[,1], k=7))
head(mat_out1, 10)

arima_pred1 <- ind1.arma$coef[length(ind1.arma$coef)] +
  as.matrix(mat_out1[,2:ncol(mat_out1)])%*%ind1.arma$coef[1:(length(ind1.arma$coef)-1)]


# Compare Out-Of-Sample with AR(7)
par(mfrow=c(2,1))
plot(ind.lr.out[,1])
plot(as.xts(arima_pred1), type="l")


# Trading: Signum
ret1_arima <- sign(arima_pred1)*ind.lr.out[,1]

# Sharpe
sharpe_bnh <- as.double(sqrt(250) * mean(ind.lr.out[,1]) / sqrt(var(ind.lr.out[,1])))
sharpe1_arima <- as.double(sqrt(250) * mean(ret1_arima, na.rm=T) / sqrt(var(ret1_arima, na.rm=T)))


# Plot
par(mfrow=c(2,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
plot(cumsum(na.exclude(ret1_arima)),main=paste("ARIMA: Sharpe=",round(sharpe1_arima,2),sep=""))

sum(is.na(ind.lr.in[,1]))

## GARCH
ind1.garch_1111 <- garchFit(~garch(1,1), data=ind.lr.in[,1], delta=2,
                       include.delta=F, include.mean=F, trace=F, hessian="ropt")
summary(ind1.garch_1111)

plot(ind.lr.in[,1])
ind.lr.in[,1]

mean(ind.lr.in[,1])


auto.arima(ind.lr.in[,1])

#.####
## Index 2####
# ARMA
ind.lr.out[,2]

chart.ACFplus(ind.lr.in[,2])
pacf(ind.lr.in[,2])

ind2.arma <- arima(ind.lr.in[,2], order=c(2,0,0))
tsdiag(ind2.arma)
ind2.arma$coef

mat_out2 <- cbind(ind.lr.out[,2],
                  lag(ind.lr.out[,2], k=1),
                  lag(ind.lr.out[,2], k=2))
head(mat_out2, 10)


arima_pred2 <- ind2.arma$coef[length(ind2.arma$coef)] +
  as.matrix(mat_out2[,2:ncol(mat_out2)])%*%ind2.arma$coef[1:(length(ind2.arma$coef)-1)]


# Compare Out-Of-Sample with AR(7)
par(mfrow=c(2,1))
plot(ind.lr.out[,1])
plot(as.xts(arima_pred2), type="l")


# Trading: Signum
ret2_arima <- sign(arima_pred2)*ind.lr.out[,1]

# Sharpe
sharpe_bnh <- as.double(sqrt(250) * mean(ind.lr.out[,1]) / sqrt(var(ind.lr.out[,1])))
sharpe2_arima <- as.double(sqrt(250) * mean(ret2_arima, na.rm=T) / sqrt(var(ret2_arima, na.rm=T)))


# Plot
par(mfrow=c(2,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Index 2: Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
plot(cumsum(na.exclude(ret2_arima)),main=paste("Index 2: AR(2): Sharpe=",round(sharpe2_arima,2),sep=""))


# GARCH
ind2.garch <- garchFit(~garch(1,1), data=ind.lr.in[,2], delta=2,
                            include.delta=F, include.mean=T, trace=F)
summary(ind2.garch)
ljungplotGarch(ind2.garch@residuals, ind2.garch@sigma.t)



#.####
## Index 3####
ind3.garch <- garchFit(~garch(1,1), data=ind.lr.in[,3], delta=2,
                       include.delta=F, include.mean=F, trace=F)
summary(ind3.garch)
ljungplotGarch(ind3.garch@residuals, ind3.garch@sigma.t)


#.####
## Index 4####
# ARMA####
ind.lr.out[,4]

chart.ACFplus(ind.lr.in[,4])
pacf(ind.lr.in[,4])

ind4.arma <- arima(ind.lr.in[,4], order=c(2,0,0))
# auto.arima(ind.lr.in[,4])

tsdiag(ind4.arma)
ind4.arma$coef

mat_out4 <- cbind(ind.lr.out[,4],
                  lag(ind.lr.out[,4], k=1),
                  lag(ind.lr.out[,4], k=2))
head(mat_out4, 10)


arima_pred4 <- ind4.arma$coef[length(ind4.arma$coef)] +
  as.matrix(mat_out4[,2:ncol(mat_out4)])%*%ind4.arma$coef[1:(length(ind4.arma$coef)-1)]


# Compare Out-Of-Sample with AR(7)
par(mfrow=c(2,1))
plot(ind.lr.out[,4])
plot(as.xts(arima_pred4), type="l")


# Trading: Signum
ret4_arima <- sign(arima_pred1)*ind.lr.out[,4]

# Sharpe
sharpe_bnh <- as.double(sqrt(250) * mean(ind.lr.out[,4]) / sqrt(var(ind.lr.out[,4])))
sharpe4_arima <- as.double(sqrt(250) * mean(ret4_arima, na.rm=T) / sqrt(var(ret4_arima, na.rm=T)))


# Plot
par(mfrow=c(2,1))
plot(cumsum(ind.lr.out[,4]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
plot(cumsum(na.exclude(ret4_arima)),main=paste("ARIMA: Sharpe=",round(sharpe4_arima,2),sep=""))

acf(ind.lr.out[,4])


# GARCH####
ind4.garch <- garchFit(~garch(1,1), data=ind.lr.in[,4], delta=2,
                       include.delta=F, include.mean=F, trace=F)
summary(ind4.garch)
ljungplotGarch(ind4.garch@residuals, ind4.garch@sigma.t)





## Automation ARMA####
## ARIMA



ind1.arma <- arima(ind.lr.in[,1], order=c(7,0,0))
ind1.arma$sigma2

?arima









ind.lr.out[,1]

chart.ACFplus(ind.lr.in[,1])
pacf(ind.lr.in[,1])

ind1.arma <- arima(ind.lr.in[,1], order=c(7,0,0))
tsdiag(ind1.arma)
ind1.arma$coef

mat_out1 <- cbind(ind.lr.out[,1],
                  lag(ind.lr.out[,1], k=1),
                  lag(ind.lr.out[,1], k=2),
                  lag(ind.lr.out[,1], k=3),
                  lag(ind.lr.out[,1], k=4),
                  lag(ind.lr.out[,1], k=5),
                  lag(ind.lr.out[,1], k=6),
                  lag(ind.lr.out[,1], k=7))
head(mat_out1, 10)

arima_pred1 <- ind1.arma$coef[length(ind1.arma$coef)] +
  as.matrix(mat_out1[,2:ncol(mat_out1)])%*%ind1.arma$coef[1:(length(ind1.arma$coef)-1)]


# Compare Out-Of-Sample with AR(7)
par(mfrow=c(2,1))
plot(ind.lr.out[,1])
plot(as.xts(arima_pred), type="l")


# Trading: Signum
ret1_arima <- sign(arima_pred1)*ind.lr.out[,1]

# Sharpe
sharpe_bnh <- as.double(sqrt(250) * mean(ind.lr.out[,1]) / sqrt(var(ind.lr.out[,1])))
sharpe1_arima <- as.double(sqrt(250) * mean(ret1_arima, na.rm=T) / sqrt(var(ret1_arima, na.rm=T)))


# Plot
par(mfrow=c(2,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
plot(cumsum(na.exclude(ret1_arima)),main=paste("ARIMA: Sharpe=",round(sharpe1_arima,2),sep=""))