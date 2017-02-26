library(forecast)
library(zoo)
library(tseries)
library(fBasics)
library(ggplot2)
library(lmtest)
library(fUnitRoots)

#Question 1 
consumption = read.csv("F:/MSPA/CSC 425/Homework 4/consump.csv")

#time object
consump = ts(consumption$consump, start = c(2000,1),freq = 12)

#a Time plot
plot(consump, type = 'l', xlab = 'Time', ylab = 'Consumption', main  = 'Time Plot - Consumption')
#b Plot acf 
acf(as.vector(consump), lag = 20, plot = T, main = 'ACF for Consumption')
pacf(as.vector(consump), lag = 20, plot = T, main = 'ACF for Consumption')


#Computing the first difference 
d_con = diff(consump)
# ACF & PACF
plot(d_con)
acf(as.vector(d_con), lag =20, plot = T, main = 'ACF for Consumption - First Difference')
pacf(as.vector(d_con), lag =20, plot = T, main = 'ACF for Consumption - First Difference')
#Lag 1
adfTest(d_con, lags = 1, type = c("c"))
adfTest(d_con, lags = 1, type = c("ct"))
#Lag 3
adfTest(d_con, lags = 3, type = c("c"))
adfTest(d_con, lags = 3, type = c("ct"))
#Lag 5
adfTest(d_con, lags = 5, type = c("c"))
adfTest(d_con, lags = 5, type = c("ct"))




pacf(as.vector(d_con), lag =20, plot = T, main = 'PACF for Consumption - First Difference')
#Dickey Fuller Test
library(fUnitRoots)
#Lag 1
adfTest(consump, lags = 1, type = c("c"))
adfTest(consump, lags = 1, type = c("ct"))
#Lag 3
adfTest(consump, lags = 3, type = c("c"))
#Lag 5
adfTest(consump, lags = 5, type = c("c"))


#Auto Arima Function
m1=auto.arima(consump, max.P=8, max.Q=8, ic="bic", trace=T, stationary=F)

auto.arima(d_con, max.P=8, max.Q=8, ic="bic", trace=T, stationary=T)


m1= Arima(consump, order=c(1,1,1), include.drift = T, method='ML')
coeftest(m1)
#Residuals
#Null Hypothesis <- No Serial Correaltion
#lag - 4
Box.test(m1$residuals, lag = 8, "Ljung-Box", fitdf = 2)
#lag - 3
Box.test(m1$residuals, lag = 6, 'Ljung-Box', fitdf = 2)
#acf of residuals
acf(m1$residuals, plot = T, main = 'ACF - Residuals')
pacf(as.vector(m1$residuals), plot = T, main = 'PACF -Residual')

l = as.vector(consumption$consump)
source("F:/MSPA/CSC 425/In Class/backtest.R")
backtest(m1, consump, h =1, orig = length(x)*0.9)
n_train  = length(consump)*0.9
backtest(m1, n_train, h =1, inc.drift=TRUE, inc.mean = FALSE)
length(consump)
k = length(consump)*0.9 


############################## 
colnames(consumption)
pairs(~consump + pers_inc + unemp, data = consumption, main = "Simple Scatter Plot")

