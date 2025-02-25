getwd()
setwd("C:/Users/gayat/OneDrive/Desktop/PRO/United states")
data<-read.csv("usa.csv")
data

data<-ts(data)
data

acf(data[,2]) 
pacf(data[,2])

#install.packages("tseries")
library(tseries)
adf.test(data[,2])

df<-ts(data[,2],frequency=1,start=1950,end=2022)
dftrain<-ts(df,frequency=1,start=1950,end=2017)
dftest<-data[69:73,2]

#install.packages("forecast")
library("forecast")
auto.arima(dftrain) 
model <- Arima(dftrain,order=c(2,2,0))
predicted=forecast(model,h=5)
predicted
plot(predicted,col="blue",xlim=c(2010,2025),ylim=c(300000000,350000000))
lines(df,col="red")
accuracy(model)
plot(model$x,col="red")
lines(fitted(model),col="blue")

modelar<-Arima(dftrain/100000000,order=c(2,0,0),method="ML")
modelma<-Arima(dftrain/100000000,order=c(0,0,0))
modelarma<-Arima(dftrain/100000000,order=c(2,0,0),method="ML")
modelarima<-Arima(dftrain/100000000,order=c(2,2,0))
accuracy(modelar)
accuracy(modelma)
accuracy(modelarma)
accuracy(modelarima)

bestmodel=Arima(df,order=c(2,2,0))
p=forecast(bestmodel,h=3)
p$mean[3]
(p$mean[3]-dftest[5])/dftest[5]*100
