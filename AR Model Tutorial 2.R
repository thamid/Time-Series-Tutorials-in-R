library(tseries)
library(fBasics)
library(zoo)
#2. IMPORT DATA
#  Load data with no variable names into the data frame "da"
df=read.table("F:/MSPA/CSC 425/Homework 2/NAPM.csv",header=F, skip=1, sep=',') 
ts_index = ts(df$V2, start=c(1980, 1), freq = 12)

''' 
par(mfcol=c(1,1)) 
hist(ts_index, xlab="Index", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(ts_index),max(ts_index),length=40)
yfit<-dnorm(xfit,mean=mean(ts_index),sd=sd(ts_index))
lines(xfit, yfit, col="blue", lwd=2) 
# CREATE NORMAL PROBABILITY PLOT
qqnorm(ts_index)
qqline(ts_index, col = 2) 
#basic stats
basicStats(ts_index)
'''
#2b. Plotting time series index
plot(ts_index, type="l", xlab="Time", ylab="PMI Index", main = 'Time Plot - PMI Index')

#2c. Plotting the Acf Function
acf_value=acf(coredata(ts_index), plot=T, lag=15, main = 'Auto Correlation Function - PMI Index')
#Compute Ljun Box Test for Serial Correlation
# to Lag 6
Box.test(ts_index,lag=6,type='Ljung')
# to Lag 10
Box.test(ts_index,lag=10, type='Ljung')
# to Lag 20
Box.test(ts_index,lag=20, type='Ljung')



#2d. PACF plot
pacf(coredata(ts_index), lag = 15, main = 'Partial Auto Correlation Function')

#2e
#a. fitting an adequate AR Model & finding about the significance
library(forecast)
model=Arima(ts_index, c(3,0,0), method="ML")  
library(lmtest)
coeftest(model)

#b. Plotting the acf of residuals
res = model$residuals
acf(coredata(res), lag = 15, main = 'Auto Correlation Function - Residuals')


#Compute Ljun Box Test for White Noise
# to Lag 9
Box.test(res,lag=9,type='Ljung')
# to Lag 12
Box.test(res,lag=12, type='Ljung')
# Histogram & QQ Plot
par(mfcol=c(1,1)) 
hist(res, xlab="Index", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(res),max(res),length=40)
yfit<-dnorm(xfit,mean=mean(res),sd=sd(res))
lines(xfit, yfit, col="blue", lwd=2) 
# CREATE NORMAL PROBABILITY PLOT
qqnorm(res)
qqline(res, col = 2) 
#basic stats
basicStats(res)

#2g

polyroot(c(1,-model$coef[1:3]))
plot(model)

#2h. Computing the 5 step forecast
forecast_5 = forecast.Arima(model, h=5)
forecast_5


#2h.i Computing the 10 step forecast
forecast_10 = forecast.Arima(model, h=10)
forecast_10
plot(forecast_10, include  = 150)

