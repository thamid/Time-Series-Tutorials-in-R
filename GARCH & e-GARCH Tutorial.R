## LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

myd =read.table("F:/MSPA/CSC 425/Homework 4/consump.csv", header=T, sep=',')

#time object
conts = ts(myd$consump, start = c(2000,1),freq = 12)
incomets = ts(myd$pers_inc, start = c(2000,1), freq = 12)
unempts = ts(myd$unemp, start = c(2000,1), freq = 12)

# COMPUTE SUMMARY STATISTICS
basicStats(conts)

#5 CREATE HISTOGRAM
# creates 2 by 1 display for 2 plots
par(mfcol=c(1,1)) 
hist(conts, xlab="Consumption", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(conts),max(conts),length=40)
yfit<-dnorm(xfit,mean=mean(conts),sd=sd(conts))
lines(xfit, yfit, col="blue", lwd=2) 


# Time series plot
plot(conts, type = 'l', xlab = 'Time', ylab = 'Consumption', main  = 'Time Plot Consumption')
plot(incomets, type = 'l', xlab = 'Time', ylab = 'income', main  = 'Time Plot Personal Income')
plot(conts, type = 'l', xlab = 'Time', ylab = 'Unemployment', main  = 'Time Plot Unemployment')

qqnorm(conts)
qqline(conts)

#log of consump

lnconsump = log(conts)

#5 CREATE HISTOGRAM
# creates 2 by 1 display for 2 plots
par(mfcol=c(1,1)) 
hist(lnconsump, xlab="log Consumption", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(lnconsump),max(lnconsump),length=40)
yfit<-dnorm(xfit,mean=mean(lnconsump),sd=sd(lnconsump))
lines(xfit, yfit, col="blue", lwd=2) 
qqnorm(lnconsump)
qqline(lnconsump)


# scatterplot matrix
pairs(~myd$consump + myd$pers_inc+ myd$unemp, main  = 'Scatter Plot')

# Correlation analysis
cor(data.frame(myd$consump, myd$pers_inc, myd$unemp))

# MODEL 1
# Fit Regression Model with covariates
m1=arima(as.vector(conts), order=c(1,1,1), xreg=data.frame(myd$pers_inc, myd$unemp), method="ML")
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


#Removing Unemployment as it is insignificant
m1=arima(as.vector(conts), order=c(1,1,1), xreg=data.frame(myd$pers_inc), method="ML")
coeftest(m1)

polyroot(c(1, -m1$coef[1:3]))
#Residuals
#Null Hypothesis <- No Serial Correaltion
#lag - 4
Box.test(m1$residuals, lag = 8, "Ljung-Box", fitdf = 2)
#lag - 3
Box.test(m1$residuals, lag = 6, 'Ljung-Box', fitdf = 2)
#acf of residuals
acf(m1$residuals, plot = T, main = 'ACF - Residuals')
pacf(as.vector(m1$residuals), plot = T, main = 'PACF -Residual')

####################### Fitting Lagged Regressor ######################
m2=arima(as.vector(conts),order=c(1,1,1), xreg=data.frame( myd$pers_inc,myd$unemp, tslag(myd$pers_inc), tslag(myd$unemp)), method="ML")
coeftest(m2)

m3=arima(as.vector(conts),order=c(1,1,1), xreg=data.frame( myd$pers_inc, myd$unemp, tslag(myd$unemp)), method="ML")
coeftest(m3)

#Residuals
#Null Hypothesis <- No Serial Correaltion
#lag - 4
Box.test(m3$residuals, lag = 8, "Ljung-Box", fitdf = 2)
#lag - 3
Box.test(m3$residuals, lag = 6, 'Ljung-Box', fitdf = 2)
#acf of residuals
acf(coredata(m3$residuals), na.action  = na.pass, plot = T, main = 'ACF - Residuals')
pacf(as.vector(m3$residuals), na.action = na.pass, plot = T, main = 'PACF -Residual')
