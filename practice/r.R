getwd()
setwd("C:/Users/gayat/OneDrive/Desktop/PRO/practice")
data<-read.csv("practice.csv")
data

acf(data) 
pacf(data)

install.packages("tseries")
library(tseries)
adf.test(data[,2])

df<-ts(data[,2],frequency=1,start=1950,end=2021)
dftrain<-ts(df,frequency=1,start=1950,end=2016)
dftest<-data[68:72,2]

install.packages("forecast")
library("forecast")
auto.arima(dftrain) 
model <- Arima(dftrain,order=c(5,2,2))
predicted=forecast(model,h=5)
predicted
plot(predicted,col="blue")
lines(df,col="red")
accuracy(model)
plot(model$x,col="red")
lines(fitted(model),col="blue")
