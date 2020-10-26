load("data/data_mobi")

par(mfrow=c(1,1))
ts.plot(data[,1:4], col=rainbow(4), main="TimeSeries")
legend('topleft', legend=1:4, col=rainbow(4), lty=1)

data[,1]

head(data)
head(log_data)

par(mfrow=c(4,1))
ts.plot(diff(log(data[,1])), col="red", main="TimeSeries")
ts.plot(diff(log(data[,2])), col="blue", main="TimeSeries")
ts.plot(diff(log(data[,3])), col="green", main="TimeSeries")
ts.plot(diff(log(data[,4])), col="purple", main="TimeSeries")

row.names(data[,1])
# d
# Jeder der 4 Indexes möchte man traden.
# MA-Filter auf jeder Reihe (univariat). Jeder der 4 Reihen ist ein target.
# Evtl.

# Multivariat
# Erklärende zusätzliche
# Interest 1-8 sind reale Zinsen.
# 1. 3 Monate
# 2. 6 Monat
# 3. etc.


# Zinsreihen sind echte Reihen
# Index 1-4 sind Assets die man traden kann.
# Haben einen regelmässigen Trend nach oben.

# Fixed Income-Teil




library(xts)
library(ggplot2)
load("data/data_mobi")
head(data)


is(data)

index(data)

ggplot(data, aes(x = Index, y = data[,1])) + 
  geom_line()

ts.plot(data[,1])


autoplot(data[,1:4], geom = "line")

