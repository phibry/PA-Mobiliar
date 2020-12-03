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
optim_ar <- function(x.lr, inx, insamp="2019-01-01") {
  ind.lr <- x.lr
  year_list <- NULL
  for (yx in 3:18) {
    year_list <- c(year_list, paste(2000+yx,"-01-01", sep=""))
  }
  
  # Create result_matrix
  res_mat <- matrix(1:(3*length(year_list)), ncol=3,
                    dimnames = list(1:length(year_list), c("StartDate", "AR-Sharpe", "AR-Order p")))
  
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
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
  return(res_df)
}

## Index1####
opt_ar_1 <- optim_ar(x.lr=ind.lr, inx=1)
# Best Model
opt_ar_1[which.max(opt_ar_1[,2]),]
# Plot


## Index2####
opt_ar_2 <- optim_ar(x.lr=ind.lr, inx=2)
# Best Model
opt_ar_2[which.max(opt_ar_2[,2]),]

## Index3####
opt_ar_3 <- optim_ar(x.lr=ind.lr, inx=3)
# Best Model
opt_ar_3[which.max(opt_ar_3[,2]),]

## Index4####
opt_ar_4 <- optim_ar(x.lr=ind.lr, inx=4)
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




#.####
# MA-Filter####
# Ind1####
pt_obj <- optimize_simple_MA_func(x=na.exclude(ind.lr[,1]), in_samp="2019-01-01", min_L=5, max_L=1000, x_trade=NULL)

pt_obj$L_opt
pt_obj$sharpe_opt

par(mfrow=c(1,1))
ts.plot(pt_obj$perf_vec)
perf_plot_func(x=na.exclude(ind.lr[,1]), L=pt_obj$L_opt, in_samp="2019-01-01", x_trade=NULL)

# Ind2####
pt_obj <- optimize_simple_MA_func(x=na.exclude(ind.lr[,2]), in_samp="2019-01-01", min_L=5, max_L=1000, x_trade=NULL)

pt_obj$L_opt
pt_obj$sharpe_opt

par(mfrow=c(1,1))
ts.plot(pt_obj$perf_vec)
perf_plot_func(x=na.exclude(ind.lr[,2]), L=pt_obj$L_opt, in_samp="2019-01-01", x_trade=NULL)

# Ind3####
pt_obj <- optimize_simple_MA_func(x=na.exclude(ind.lr[,3]), in_samp="2019-01-01", min_L=5, max_L=1000, x_trade=NULL)

pt_obj$L_opt
pt_obj$sharpe_opt

par(mfrow=c(1,1))
ts.plot(pt_obj$perf_vec)
perf_plot_func(x=na.exclude(ind.lr[,3]), L=pt_obj$L_opt, in_samp="2019-01-01", x_trade=NULL)

# Ind4####
pt_obj <- optimize_simple_MA_func(x=na.exclude(ind.lr[,4]), in_samp="2019-01-01", min_L=5, max_L=1000, x_trade=NULL)

pt_obj$L_opt
pt_obj$sharpe_opt

par(mfrow=c(1,1))
ts.plot(pt_obj$perf_vec)
perf_plot_func(x=na.exclude(ind.lr[,4]), L=pt_obj$L_opt, in_samp="2019-01-01", x_trade=NULL)










#.####
# MA-Cross-Filter####
load("data/data_mobi") 


n1 = 20
n2 = 200

mobidat = data[,4]





n1 = 2
n2 = 200

start = "2015-10-30"
end = "2018-12-31"
horizon = paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)

trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
ret <- na.exclude(ret)
cums <- cumsum(ret)[length(ret)]
cums <- as.data.frame(cums)
rownames(cums) <- paste(n1,"/",n2)
cums



L1min <- 1
L1max <- 50

L2min <- 100
L2max <- 250



# do not change!!!!
cross_optim <- function(x, start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250) {
  best_sharpe <- 0
  best_drawdown <- -1000
  mobidat <- x
  
  filler <- 1
  perf_mat <- matrix(1:(length(L1min:L1max)*(length(L2min:L2max))*3), ncol=3,
                     dimnames = list(1:(length(L1min:L1max)*length(L2min:L2max)),
                     c("Filter", "Sharpe", "Drawdown")))
  
  pb <- txtProgressBar(min = L1min, max = L1max, style = 3)
  for (k in L1min:L1max) {
    for (j in L2min:L2max) {
      n1 <- k
      n2 <- j
      
      # sample
      horizon <- paste(start,"::",end,sep = "")
      
      # Simple Moving Averages
      sma1 <- SMA(mobidat,n=n1)
      sma2 <- SMA(mobidat,n=n2)
      
      # Signals
      signal <- rep(0,length(sma1))
      signal[which(sma1>sma2&lag(sma1)<lag(sma2))] <- 1
      signal[which(sma1<sma2&lag(sma1)>lag(sma2))]< - -1
      signal[which(sma1>sma2)] <- 1
      signal[which(sma1<sma2)] <- -1
      signal <- reclass(signal,sma1)
      
      # Trading
      trade   <-   Lag(signal[horizon],1)
      return  <-   diff(log(mobidat))
      ret <- return*trade
      ret <- na.exclude(ret)
      
      # Sharpe
      sharpe <- sqrt(250)*mean(ret)/sqrt(var(ret))
      
      # Drawdown
      maxdraw <- -max(abs(Drawdowns(ret, geometric = F)))


      # Fill Matrix
      perf_mat[filler,1] <- paste(n1,"/",n2)
      perf_mat[filler,2:3] <- c(as.numeric(sharpe), maxdraw)

      filler <- filler + 1
      
      # Best
      if (sharpe > best_sharpe) {
        best_sharpe <- sharpe
        rownames(best_sharpe) <- paste(n1,"/",n2)
      }
      if (maxdraw > best_drawdown) {
        maxdraw <- as.data.frame(maxdraw)
        best_drawdown <- maxdraw
        rownames(best_drawdown) <- paste(n1,"/",n2)
      }
      
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
  print(best_drawdown)
  print(best_sharpe)
  return(list(best_drawdown=best_drawdown, best_sharpe=best_sharpe, perf_mat=perf_mat))
}

# Optim-Plot####
plot(ind1_opt$perf_mat[,2], col="red", type="l")
lines((as.numeric(ind1_opt$perf_mat[,3])*90)+1.7, col="green", type="l")

loop_func <- function(x.lr, insamp="2019-01-01", minyear=3, maxyear=18) {
  ind.lr <- x.lr
  year_list <- NULL
  for (yx in minyear:maxyear) {
    year_list <- c(year_list, paste(2000+yx,"-01-01", sep=""))
  }
  
  # Create result_matrix
  res_mat <- matrix(1:(5*length(year_list)), ncol=5,
                    dimnames = list(1:length(year_list), c("StartDate", "Sharpe", "S-Filter", "Drawdown", "D-Filter")))
  
  pb <- txtProgressBar(min = 1, max = length(year_list), style = 3)
  
  # Loop through years
  for (i in 1:length(year_list)) {
    startdate <- year_list[i]
    
    #startdate <- year_list[1]
    
    ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]
    ind.lr.in <- ind.lr.all[paste("/",insamp,sep="")]
    ind.lr.out <- ind.lr.all[paste(insamp,"/",sep="")]
    
    yolo1 <- cross_optim_easy(x=ind.lr.in, start=startdate)
    
    res_mat[i,1] <- startdate
    
    res_mat[i,2:3] <- c(as.numeric(yolo1$best_sharpe), rownames(yolo1$best_sharpe))
    
    res_mat[i,4:5] <- c(as.numeric(yolo1$best_drawdown), rownames(yolo1$best_drawdown))
    
    setTxtProgressBar(pb, i)
  }
  res_df <- as.data.frame(res_mat)
  close(pb)
  
  return(res_df)
}
cross_optim_easy <- function(x, start, end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250) {
  best_sharpe <- 0
  best_drawdown <- -1000
  mobidat <- x
  
  pb <- txtProgressBar(min = L1min, max = L1max, style = 3)
  for (k in L1min:L1max) {
    for (j in L2min:L2max) {
      n1 <- k
      n2 <- j
      
      # sample
      horizon <- paste(start,"::",end,sep = "")
      
      # Simple Moving Averages
      sma1 <- SMA(mobidat,n=n1)
      sma2 <- SMA(mobidat,n=n2)
      
      # Signals
      signal <- rep(0,length(sma1))
      signal[which(sma1>sma2&lag(sma1)<lag(sma2))] <- 1
      signal[which(sma1<sma2&lag(sma1)>lag(sma2))]< - -1
      signal[which(sma1>sma2)] <- 1
      signal[which(sma1<sma2)] <- -1
      signal <- reclass(signal,sma1)
      
      # Trading
      trade   <-   Lag(signal[horizon],1)
      return  <-   diff(log(mobidat))
      ret <- return*trade
      ret <- na.exclude(ret)
      
      # Sharpe
      sharpe <- sqrt(250)*mean(ret)/sqrt(var(ret))
      
      # Drawdown
      maxdraw <- -max(abs(Drawdowns(ret, geometric = F)))
      
      # Best
      if (sharpe > best_sharpe) {
        best_sharpe <- sharpe
        rownames(best_sharpe) <- paste(n1,"/",n2)
      }
      if (maxdraw > best_drawdown) {
        maxdraw <- as.data.frame(maxdraw)
        best_drawdown <- maxdraw
        rownames(best_drawdown) <- paste(n1,"/",n2)
      }
      
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
  return(list(best_drawdown=best_drawdown, best_sharpe=best_sharpe))
}


test1 <- loop_func(x.lr=data[,1])
save(test1, file="data/R_Files/optim_ma_cross_1.RData")
# StartDate            Sharpe S-Filter              Drawdown D-Filter
# 1  2003-01-01  1.45844222153173 49 / 246   -0.0141489634438965 19 / 105
# 2  2004-01-01  1.47125034690453 49 / 243   -0.0140297780479764 26 / 102
# 3  2005-01-01  1.48915679924878 49 / 243   -0.0140367194253574 26 / 100
# 4  2006-01-01  1.42311105563007 37 / 113   -0.0142801979799769 26 / 100
# 5  2007-01-01  1.23341916290696 37 / 113   -0.0148979753498477 26 / 100
# 6  2008-01-01 0.985242507857545 39 / 206  -0.00952124350747108 10 / 224
# 7  2009-01-01  1.12829996441047 42 / 113  -0.00952640050631492 10 / 224
# 8  2010-01-01 0.938642084363342 42 / 113  -0.00971507133204241 10 / 227
# 9  2011-01-01 0.868689748882843  1 / 148  -0.00679858760873919  1 / 171
# 10 2012-01-01 0.861010216124702 37 / 113  -0.00680627442924042  1 / 173
# 11 2013-01-01 0.909432395375849  1 / 171  -0.00680577256804404  1 / 171
# 12 2014-01-01  0.97574405364782 37 / 113  -0.00684933140843302  1 / 171
# 13 2015-01-01  1.01232702605895 18 / 102  -0.00690089276907202  1 / 173
# 14 2016-01-01  1.59439214889633 44 / 113  -0.00610750793887171 40 / 110
# 15 2017-01-01  1.76789386791698  2 / 117  -0.00381724861593424  2 / 114
# 16 2018-01-01  3.57988708556172  1 / 224 -0.000344832672721118  1 / 247


test2 <- loop_func(x.lr=data[,2])
save(test2, file="data/R_Files/optim_ma_cross_2.RData")
# StartDate            Sharpe S-Filter            Drawdown D-Filter
# 1  2003-01-01 0.714241766942486 35 / 242 -0.0442627463061008 50 / 249
# 2  2004-01-01 0.727413283775737 34 / 241 -0.0415871669398634 29 / 134
# 3  2005-01-01 0.859970781894646 33 / 242 -0.0378077702541536 35 / 104
# 4  2006-01-01 0.838576096690756 32 / 250  -0.032868251320908 14 / 119
# 5  2007-01-01 0.757388542748876 24 / 136 -0.0336980711236377 14 / 119
# 6  2008-01-01 0.668255764918196 38 / 106 -0.0322014116644994 11 / 128
# 7  2009-01-01 0.708170190122589 33 / 182 -0.0322690446518665 39 / 198
# 8  2010-01-01 0.624332357270294 38 / 106 -0.0342575037133864  3 / 250
# 9  2011-01-01 0.527307621044327 35 / 182 -0.0242843452401353 15 / 131
# 10 2012-01-01 0.473894666684324 35 / 182 -0.0225986384448282 15 / 133
# 11 2013-01-01 0.555089400702877 38 / 106 -0.0226914800190647 15 / 133
# 12 2014-01-01 0.742132702028808 16 / 135 -0.0189907999173068 27 / 120
# 13 2015-01-01 0.764497914066422 13 / 129 -0.0193988162548194 27 / 120
# 14 2016-01-01  1.14131156757606 17 / 111 -0.0137615531429115 48 / 109
# 15 2017-01-01  1.28684846731804 17 / 111 -0.0136734158098419 38 / 135
# 16 2018-01-01  3.79682626850827 24 / 215 -0.0014425362406767  1 / 248


test3 <- loop_func(x.lr=data[,3])
save(test3, file="data/R_Files/optim_ma_cross_3.RData")
# StartDate            Sharpe S-Filter             Drawdown D-Filter
# 1  2003-01-01 0.567270710544995 18 / 188  -0.0549962129364439 22 / 120
# 2  2004-01-01 0.528065625879041 18 / 188   -0.055115302523482 22 / 120
# 3  2005-01-01  0.58935212142563 18 / 188  -0.0517029289063775 36 / 105
# 4  2006-01-01   0.6819517317697 28 / 102  -0.0494857506058194 37 / 106
# 5  2007-01-01 0.630007549439238 31 / 102  -0.0514636689657824 37 / 106
# 6  2008-01-01 0.605050110801017 28 / 102  -0.0507678619830981  5 / 139
# 7  2009-01-01 0.561016298885095 16 / 186  -0.0540205400931484  5 / 139
# 8  2010-01-01 0.577308212431729 22 / 105  -0.0539970099068866  1 / 239
# 9  2011-01-01 0.524296167840498 25 / 116  -0.0372989296815835 18 / 172
# 10 2012-01-01 0.402454012067928 20 / 121  -0.0380847654608469 18 / 172
# 11 2013-01-01 0.547497077821796 18 / 115  -0.0340166834298516 16 / 197
# 12 2014-01-01 0.783317343576516 17 / 118  -0.0280123198844132 20 / 121
# 13 2015-01-01 0.734831081895045 13 / 107  -0.0273866568131926 12 / 147
# 14 2016-01-01   1.0727090311748 18 / 104   -0.020980932394247 14 / 104
# 15 2017-01-01 0.984964823102248 18 / 102  -0.0199278298520777 10 / 102
# 16 2018-01-01  2.88802424535405  1 / 227 -0.00271745745750329  1 / 248


test4 <- loop_func(x.lr=data[,4])
save(test4, file="data/R_Files/optim_ma_cross_4.RData")
# StartDate            Sharpe S-Filter             Drawdown D-Filter
# 1  2003-01-01 0.511127961896197  7 / 116  -0.0784071560569436 15 / 129
# 2  2004-01-01 0.512090071424057 12 / 103  -0.0784102211535621  9 / 154
# 3  2005-01-01 0.562778084170941 12 / 103  -0.0768745646069597  9 / 154
# 4  2006-01-01 0.638716914421034 12 / 103  -0.0759902988629919 11 / 101
# 5  2007-01-01 0.640084734095819 11 / 101  -0.0775411136421347 11 / 101
# 6  2008-01-01  0.60594315747554 11 / 101  -0.0777126920737493 15 / 130
# 7  2009-01-01 0.541553293913331 11 / 145  -0.0718201386085348  4 / 193
# 8  2010-01-01 0.677527970261711 10 / 117  -0.0514031307839711  8 / 122
# 9  2011-01-01 0.709196468879411 13 / 146  -0.0500564075063146 14 / 166
# 10 2012-01-01  0.61300633235829 13 / 146  -0.0423142297951923 12 / 150
# 11 2013-01-01  0.67194162089095  9 / 113    -0.04345226069608 12 / 145
# 12 2014-01-01 0.774373652156288 15 / 136  -0.0383275866251787 16 / 131
# 13 2015-01-01 0.719511492508388 15 / 136  -0.0326315350580271 11 / 121
# 14 2016-01-01  1.00483115143095 18 / 100  -0.0248493631861116 19 / 101
# 15 2017-01-01 0.891839938705778 11 / 103  -0.0256875325812206 11 / 102
# 16 2018-01-01  2.64063599985071 11 / 235 -0.00360926583826626 43 / 245



# do not change!!!!
cross_optim <- function(x, start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250) {
  best_sharpe <- 0
  best_drawdown <- -1000
  mobidat <- x
  
  filler <- 1
  perf_mat <- matrix(1:(length(L1min:L1max)*(length(L2min:L2max))*3), ncol=3,
                     dimnames = list(1:(length(L1min:L1max)*length(L2min:L2max)),
                                     c("Filter", "Sharpe", "Drawdown")))
  
  pb <- txtProgressBar(min = L1min, max = L1max, style = 3)
  for (k in L1min:L1max) {
    for (j in L2min:L2max) {
      n1 <- k
      n2 <- j
      
      # sample
      horizon <- paste(start,"::",end,sep = "")
      
      # Simple Moving Averages
      sma1 <- SMA(mobidat,n=n1)
      sma2 <- SMA(mobidat,n=n2)
      
      # Signals
      signal <- rep(0,length(sma1))
      signal[which(sma1>sma2&lag(sma1)<lag(sma2))] <- 1
      signal[which(sma1<sma2&lag(sma1)>lag(sma2))]< - -1
      signal[which(sma1>sma2)] <- 1
      signal[which(sma1<sma2)] <- -1
      signal <- reclass(signal,sma1)
      
      # Trading
      trade   <-   Lag(signal[horizon],1)
      return  <-   diff(log(mobidat))
      ret <- return*trade
      ret <- na.exclude(ret)
      
      # Sharpe
      sharpe <- sqrt(250)*mean(ret)/sqrt(var(ret))
      
      # Drawdown
      maxdraw <- -max(abs(Drawdowns(ret, geometric = F)))
      
      
      # Fill Matrix
      perf_mat[filler,1] <- paste(n1,"/",n2)
      perf_mat[filler,2:3] <- c(as.numeric(sharpe), maxdraw)
      
      filler <- filler + 1
      
      # Best
      if (sharpe > best_sharpe) {
        best_sharpe <- sharpe
        rownames(best_sharpe) <- paste(n1,"/",n2)
      }
      if (maxdraw > best_drawdown) {
        maxdraw <- as.data.frame(maxdraw)
        best_drawdown <- maxdraw
        rownames(best_drawdown) <- paste(n1,"/",n2)
      }
      
    }
    setTxtProgressBar(pb, k)
  }
  close(pb)
  print(best_drawdown)
  print(best_sharpe)
  return(list(best_drawdown=best_drawdown, best_sharpe=best_sharpe, perf_mat=perf_mat))
}


ind1_opt <- cross_optim(data[,1], start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250)
save(ind1_opt, file="data/R_Files/optim_ma_cross_obj_1.RData")


ind2_opt <- cross_optim(data[,2], start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250)
save(ind2_opt, file="data/R_Files/optim_ma_cross_obj_2.RData")


ind3_opt <- cross_optim(data[,3], start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250)
save(ind3_opt, file="data/R_Files/optim_ma_cross_obj_3.RData")


ind4_opt <- cross_optim(data[,4], start = "2015-10-30", end = "2018-12-31", L1min = 1, L1max = 50, L2min =  100, L2max = 250)
save(ind4_opt, file="data/R_Files/optim_ma_cross_obj_4.RData")

# Optim-Plot####
plot(ind1_opt$perf_mat[,2], col="red", type="l")
lines((as.numeric(ind1_opt$perf_mat[,3])*90)+1.7, col="green", type="l")


#.####
# Optim Index 1####
# Index.1
# 1 / 106 1.225487
n1 <- 1
n2 <- 106

mobidat = data[,1] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"

start = "2015-10-30"
end = "2018-12-31"

horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
#chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="", yrange=c(700, 900))
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
chartSeries(mobidat["2019-01-01/"], theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
mobidat["2019-01-01/"]
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")

?addSMA

sma_verif <- SMA(mobidat["2019-01-01/"], n=n2)
plot(filt_obj$yhat, col="purple")
lines(sma_verif, col="green")


addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
names(ret)="filter"

SharpeRatio(ret,FUN="StdDev")*sqrt(250)







# Verification####
L1 <- 106
b1 <- matrix(rep(1/L1,L1),ncol=1,nrow=L1)

filt_func<-function(x,b)
{
  L<-nrow(b)
  if (is.matrix(x))
  {  
    length_time_series<-nrow(x)
  } else
  {
    if (is.vector(x))
    {
      length_time_series<-length(x)
    } else
    {
      print("Error: x is neither a matrix nor a vector!!!!")
    }
  }
  if (is.xts(x))
  {
    yhat<-x[,1]
  } else
  {
    yhat<-rep(NA,length_time_series)
  }
  for (i in L:length_time_series)#i<-L
  {
    # If x is an xts object then we cannot reorder x in desceding time i.e. x[i:(i-L+1)] is the same as  x[(i-L+1):i]
    #   Therefore, in this case, we have to revert the ordering of the b coefficients.    
    if (is.xts(x))
    {
      if (ncol(b)>1)
      {
        yhat[i]<-as.double(sum(apply(b[L:1,]*x[i:(i-L+1),],1,sum)))
      } else
      {
        yhat[i]<-as.double(b[L:1,]%*%x[i:(i-L+1)])#tail(x) x[(i-L+1):i]
      }
    } else
    {
      if (ncol(b)>1)
      {
        yhat[i]<-as.double(sum(apply(b[1:L,]*x[i:(i-L+1),],1,sum)))
      } else
      {
        yhat[i]<-as.double(as.vector(b)%*%x[i:(i-L+1)])#tail(x) x[(i-L+1):i]
      }
    }
  }
  #  names(yhat)<-index(x)#index(yhat)  index(x)
  #  yhat<-as.xts(yhat,tz="GMT")
  return(list(yhat=yhat))
}

mobidat["2019-01-01/"]

filt_obj<-filt_func(x=mobidat["2019-01-01/"],b1)
filt_obj$yhat

# Plot verif
sma_verif <- SMA(mobidat["2019-01-01/"], n=n2)
plot(filt_obj$yhat, col="purple")
lines(sma_verif, col="green")












# Optim Index 2####
# 17 / 111 0.9386703
n1=17
n2 =111

mobidat= data[,2] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"

start = "2015-10-30"
end = "2018-12-31"

horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
names(ret)="filter"

SharpeRatio(ret,FUN="StdDev")*sqrt(250)


# Optim Index 3####
# 16 / 112 0.8723012
n1 = 16
n2 = 112

mobidat= data[,3] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"

start = "2015-10-30"
end = "2018-12-31"

horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
names(ret)="filter"

SharpeRatio(ret,FUN="StdDev")*sqrt(250)


# Optim Index 4####
# 15 / 135 0.8837106
n1=15
n2 =135

mobidat= data[,4] # chose the row and horizon
start="2019-01-01"
end = "2020-04-31"

start = "2015-10-30"
end = "2018-12-31"



horizon=paste(start,"::",end,sep = "")
sma1 <-SMA(mobidat,n=n1)
sma2 <-SMA(mobidat,n=n2)
signal <-rep(0,length(sma1))
signal[which(sma1>sma2&lag(sma1)<lag(sma2))]<-1
signal[which(sma1<sma2&lag(sma1)>lag(sma2))]<--1
signal[which(sma1>sma2)]<-1
signal[which(sma1<sma2)]<--1
signal=reclass(signal,sma1)
chartSeries(mobidat,subset=horizon,theme=chartTheme("white", bg.col="#FFFFFF"),name= "sMa",type="")
addSMA(n=n1,on=1,col = "blue")
addSMA(n=n2,on=1,col = "red")
addTA(signal,type="S",col="red")
trade   =   Lag(signal[horizon],1)
return  =   diff(log(mobidat))
ret = return*trade
names(ret)="filter"

SharpeRatio(ret,FUN="StdDev")*sqrt(250)