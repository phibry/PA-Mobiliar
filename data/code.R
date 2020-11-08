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

par(bg="lightgray")
plot(rnorm(100))


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

ind1 <- ind.lr[,1]
ind2 <- ind.lr[,2]
ind3 <- ind.lr[,3]
ind4 <- ind.lr[,4]

startdate <- "2014-01-01"
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

# out-of sample
ind1.out <- ind1.all[paste(insample,"/",sep="")]

#.####
# 1.2. ARMA-GARCH####
# 1.2.1. Index 1####
library(fGarch)
par(mfrow=c(2,1))
plot(ind.in[,1])
plot(ind.lr.in[,1])
# Trend vorhanden, auch wenn nur von 750-790
# Volacluster vorhanden
# Probleme mit dem Index 1 -> neue Zeitperiode

ind1.garch_11 <- garchFit(~garch(1,1), data=ind.lr.in[,1], delta=2,
                       include.delta=F, include.mean=T, trace=F)

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

# 1.2.2. Index 2####
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


# 1.2.3. Index 3####
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


# 1.2.4. Index 4####
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



library(quantmod)
SMI <- getSymbols("GOOGL", auto.assign=F)
plot(SMI[,6])
par(mfrow=c(1,2))
acf(SMI[,6])
pacf(SMI[,6])
