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
## ARIMA####
ind.lr.out[,1]

chart.ACFplus(ind.lr.in[,1])
pacf(ind.lr.in[,1])
pacf(ind.lr.in[,2])
pacf(ind.lr.in[,3])
pacf(ind.lr.in[,4])

ind1.arma <- arima(ind.lr.in[,1], order=c(1,0,0))
tsdiag(ind1.arma)
ind1.arma$coef

mat_out1 <- cbind(ind.lr.out[,1],
                  lag(ind.lr.out[,1], k=1))
head(mat_out1, 10)


arima_pred1 <- ind1.arma$coef[length(ind1.arma$coef)] +
  as.matrix(mat_out1[,2:ncol(mat_out1)])%*%ind1.arma$coef[1:(length(ind1.arma$coef)-1)]


# Compare Out-Of-Sample with AR(1)
par(mfrow=c(2,1))
plot(ind.lr.out[,1])
plot(as.xts(arima_pred1), type="l")


# Trading: Signum
ret1_arima <- sign(arima_pred1)*ind.lr.out[,1]

# Sharpe
sharpe_bnh <- as.double(sqrt(250) * mean(ind.lr.out[,1]) / sqrt(var(ind.lr.out[,1])))
sharpe1_arima <- as.double(sqrt(250) * mean(ret1_arima, na.rm=T) / sqrt(var(ret1_arima, na.rm=T)))


# Plot
par(mfrow=c(1,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
lines(cumsum(na.exclude(ret1_arima)),lty=2, col="red")


## M-GARCH####
ind1.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,1], delta=2,
                       include.delta=F, include.mean=F, trace=F)
summary(ind1.garch_11)
# ARMA(1,0)+GARCH(1,1): -12.38373
# ARMA(0,0)+GARCH(1,1): -12.40257

# Sigma_t in-sample
sigma_t_in <- ind1.garch_11@sigma.t
names(sigma_t_in) <- index(ind.lr.in[,1])
sigma_t_in <- as.xts(sigma_t_in)
index(sigma_t_in) <- index(ind.lr.in[,1])

# Regression
lm_obj <- lm(ind.lr.in[,1] ~ sigma_t_in)
summary(lm_obj)

# Sigma_t out of sample
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
sigma_t_out <- compute_sigma_t_out_of_sample_func(ind.lr.out[,1], ind1.garch_11, ind.lr.in[,1])$sigma_t

# MGARCH-Prediction
mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t_out

# MGARCH-Returns
returns_mgarch <- sign(mgarch_predict)*ind.lr.out[,1]

# Sharpe
sharpe1_mgarch <- as.double(sqrt(250) * mean(returns_mgarch, na.rm=T) / sqrt(var(returns_mgarch, na.rm=T)))


par(mfrow=c(1,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
lines(cumsum(na.exclude(ret1_arima)),lty=2, col="red")
lines(cumsum(na.exclude(returns_mgarch)), lty=2, col="blue")

sharpe_bnh
sharpe1_mgarch



#.####
## Index 2####
# ARMA####
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
par(mfrow=c(1,1))
plot(cumsum(ind.lr.out[,1]),main=paste("Index 2: Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
lines(cumsum(na.exclude(ret2_arima)), lty=2, col="red")

# M-GARCH####
ind2.garch <- garchFit(~garch(1,1), data=ind.lr.in[,2], delta=2,
                            include.delta=F, include.mean=T, trace=F)
summary(ind2.garch)
ljungplotGarch(ind2.garch@residuals, ind2.garch@sigma.t)


# Sigma_t in-sample
sigma_t_in <- ind2.garch@sigma.t
names(sigma_t_in) <- index(ind.lr.in[,2])
sigma_t_in <- as.xts(sigma_t_in)
index(sigma_t_in) <- index(ind.lr.in[,2])

# Regression
lm_obj <- lm(ind.lr.in[,2] ~ sigma_t_in)
summary(lm_obj)

# Sigma_t out of sample
sigma_t_out <- compute_sigma_t_out_of_sample_func(ind.lr.out[,2], ind1.garch_11, ind.lr.in[,2])$sigma_t

# MGARCH-Prediction
mgarch_predict <- lm_obj$coefficients[1]+lm_obj$coefficients[2]*sigma_t_out

# MGARCH-Returns
returns_mgarch <- sign(mgarch_predict)*ind.lr.out[,2]

# Sharpe
sharpe2_mgarch <- as.double(sqrt(250) * mean(returns_mgarch, na.rm=T) / sqrt(var(returns_mgarch, na.rm=T)))


par(mfrow=c(1,1))
plot(cumsum(ind.lr.out[,2]),main=paste("Buy & Hold: Sharpe=",round(sharpe_bnh,2),sep=""))
lines(cumsum(na.exclude(ret2_arima)),lty=2, col="red")
lines(cumsum(na.exclude(returns_mgarch)), lty=2, col="blue")

sharpe_bnh
sharpe1_mgarch



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




#.####
## Automation####
ind <- na.exclude(data[,1:4])
ind.lr <- na.exclude(diff(log(ind)))
startdate <- "2018-01-01"
insample <- "2019-01-01"

ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]
ind.lr.in <- ind.lr.all[paste("/",insample,sep="")]
ind.lr.out <- ind.lr.all[paste(insample,"/",sep="")]


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

par(mfrow=c(4,1))
performante(xin=ind.lr.in[,1], xout=ind.lr.out[,1], main="Index 1")
performante(xin=ind.lr.in[,2], xout=ind.lr.out[,2], main="Index 2")
performante(xin=ind.lr.in[,3], xout=ind.lr.out[,3], main="Index 3")
performante(xin=ind.lr.in[,4], xout=ind.lr.out[,4], main="Index 4")
