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
library(zoo)
load("data/data_mobi")


head(data)

# Split Dataframe into index-part and interest-part
par(mfrow=c(1,1))
ind <- data[,1:4]
int <- data[,5:12]

plot(int, col=tsRainbow, lwd=1)

# First Look####
## Plot Index
# Creating Color scheme with rainbow()
tsRainbow = rainbow(ncol(as.zoo(ind)))
par(mfrow=c(1,1))
plot(ind, main="Indexes 1-4", col=tsRainbow, lwd=1)
addLegend("topleft",
          legend.names=c("Index 1", "Index 2", "Index 3", "Index 4"),
          lty=c(1,1,1,1),
          col=tsRainbow)

# Index               Index 1         Index 2         
# Min.   :2003-10-30   Min.   :606.3   Min.   : 683.7  
# 1st Qu.:2007-12-14   1st Qu.:697.3   1st Qu.: 811.2  
# Median :2012-01-30   Median :781.5   Median :1019.3  
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

ind1 <- ind.lr[,1]
ind2 <- ind.lr[,2]
ind3 <- ind.lr[,3]
ind4 <- ind.lr[,4]

startdate <- "2014-01-01"
insample <- "2017-01-01"

ind.lr.all <- ind.lr[paste(startdate,"/",sep="")]

ind.lr.in <- ind.lr.all[paste("/",insample,sep="")]
ind.lr.out <- ind.lr.all[paste(insample,"/",sep="")]

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

# out-of sample
ind1.out <- ind1.all[paste(insample,"/",sep="")]


# 1.2. Fit ARMA####
par(mfrow=c(2,1))
acf(ind1, main="Index 1")
acf(ind1, main="Index 1", type="partial")

ind1_obj <- arima(ind1, order=c(5,0,4))
tsdiag(ind1_obj)

aic_bic(ind1)
# The BIC-Criteria suggests to fit a ARMA(5,4)-model. We would have to fit
# 9 parameters. In order to

#.####
# 1.2. ARMA???? DROP####
# Very difficult to display volaclusters with ARMA-models
# 1.2.1. Index 1####
par(mfrow=c(1,2))
acf(ind.lr.in[,1], main="Index 1")
acf(ind.lr.in[,1], main="Index 1", type="partial")
arma_bic(ind.lr.in[,1])
# AR(2), MA(2)

test_obj <- arima(ind.lr.in[,1], order=c(0, 0, 1))
tsdiag(test_obj)

# 1.2.2. Index 2####
acf(ind.lr.in[,2], main="Index 2")
acf(ind.lr.in[,2], main="Index 2", type="partial")
arma_bic(ind.lr.in[,2])
# AR(2), Ma(2)
test_obj <- arima(ind.lr.in[,2], order=c(0, 0, 1))
tsdiag(test_obj)

# 1.2.3. Index 3####
acf(ind.lr.in[,3], main="Index 3")
acf(ind.lr.in[,3], main="Index 3", type="partial")
arma_bic(ind.lr.in[,3])
# MA(1)
test_obj <- arima(ind.lr.in[,3], order=c(0, 0, 1))
tsdiag(test_obj)

# 1.2.4. Index 4####
acf(ind.lr.in[,4], main="Index 4")
acf((ind.lr.in[,4]), main="Index 4", type="partial")
arma_bic(ind.lr.in[,4])
# MA(1)
test_obj <- arima(ind.lr.in[,4], order=c(0, 0, 1))
tsdiag(test_obj)

# Something special with dependency on the second and 7th lag


#.####
# 1.3. ARMA-GARCH####
# 1.3.1. Index 1####
library(fGarch)
par(mfrow=c(2,1))
plot(ind.in[,1])
plot(ind.lr.in[,1])
# Trend vorhanden, auch wenn nur von 750-790
# Volacluster vorhanden
# Probleme mit dem Index 1 -> neue Zeitperiode

ind1.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,1], delta=2,
                       include.delta=F, include.mean=T)

summary(ind1.garch_11)
# Mu nicht signifkant ->include.mean=F, knapp mit den R -> ARMA-Teil

ind1.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,1], delta=2,
                          include.delta=F, include.mean=F)

summary(ind1.garch_11)
# Ljung-Box zu den R's -> ARMA-Teil
# Knapp über dem 0.05-Niveau, könnte zu Problemen führen

sum(ind1.garch_11@fit$coef["alpha1"], ind1.garch_11@fit$coef["beta1"])

# ARMA-Teil hinzufügen
ind1.garch_11_ar <- garchFit(~arma(1,1) + garch(1,1), data=ind.lr.in[,1], delta=2,
                          include.delta=F, include.mean=F)

summary(ind1.garch_11_ar)

# 1.3.2. Index 2####
plot(ind.in[,2])
plot(ind.lr.in[,2])
ind2.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,2], delta=2,
                          include.delta=F, include.mean=T)

summary(ind2.garch_11)
# mu nicht signifkant: ->include.mean=F
# Probleme mit dem ARMA-Teil

ind2.garch_11 <- garchFit(~arma(3,0)+garch(1,1), data=ind.lr.in[,2], delta=2,
                          include.delta=F, include.mean=F)

summary(ind2.garch_11)


sum(ind2.garch_11@fit$coef["alpha1"], ind2.garch_11@fit$coef["beta1"])


# 1.3.3. Index 3####
plot(ind.in[,3])
plot(ind.lr.in[,3])
ind3.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,3], delta=2,
                          include.delta=F, include.mean=T)

summary(ind3.garch_11)
# Alle Parameter sind auf dem 5%-Niveau signifkant

# Paramterrestriktion: kleiner als 1
sum(ind3.garch_11@fit$coef["alpha1"], ind3.garch_11@fit$coef["beta1"])

# Normalverteilung:
# Jarque-Beta und Shapiro < 0.05 -> gut

# ARMA-Teil:
# Ljung-Box für R's sind alle > 0.05 -> gut

# GARCH-Teil:
# Ljung-Box für R^2's. evtl Probleme beim R^2 Q(20)


# 1.3.4. Index 4####
plot(ind.in[,4])
plot(ind.lr.in[,4])
# Volacluster wirken fast schon wie Rauschen
ind4.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,4], delta=2,
                          include.delta=F, include.mean=T)

summary(ind4.garch_11)
# Alle Parameter sind auf dem 5%-Niveau signifkant

# Paramterrestriktion: kleiner als 1
sum(ind4.garch_11@fit$coef["alpha1"], ind4.garch_11@fit$coef["beta1"])

# Normalverteilung
# Jarque-Beta und Shapiro < 0.05 -> gut

# ARMA-Teil:
# Ljung-Box für R's sind alle > 0.05 -> gut

# GARCH-Teil:
# Ljung-Box für R^2's sind alle > 0.05 -> gut



# Funerino####
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
