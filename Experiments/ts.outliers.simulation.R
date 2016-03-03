#Timeseries outlier types simulation 
library('tsoutliers')
library('ggplot2')
#simulation time series data 

# Arma model ARMA(1,1)
wn <- arima.sim(model=list(ar=c(0.5,0.3),ma=c(0.5,0.0),sd=1),200)
#add additive autlier with weight of 10 times to current value at two section 20 and 170
wn[20]<- 10*wn[20]
wn[170] <- 10*wn[170]
plot(1:200,wn,type='l') 
lines(18:21,wn[18:21],col='red')
lines(169:171,wn[169:171],col='red')


#Innovation outliers 

library(tsoutliers)
#Yahoo server log outliers. 
yh<-read.csv('E:/Research/Datasets/Yahoo Labeled data/Webscope_S5/ydata-labeled-time-series-anomalies-v1_0/A1Benchmark/real_12.csv')






## examples 
library(tsoutliers)
library(expsmooth)
library(fma)

outlier.chicken <- tsoutliers::tso(chicken,types=c("AO","LS","TC"),maxit.iloop=10)

# Measurement of the annual flow of the river Nile at Ashwan 1871-1970
data(Nile)
plot(Nile)

#fit arima model

