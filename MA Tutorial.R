library(forecast)
library(zoo)
library(tseries)
library(fBasics)
library(ggplot2)
library(lmtest)

#2a
#importing the data file
df = read.csv("F:/MSPA/CSC 425/Homework 3/indpro.csv")
#time object
ratechg = ts(df$rate, start = c(1990,1),freq = 12)

#2b Time Series Plot 
plot(ratechg, type = "l", xlab = 'Time - In Years', ylab = "Index Growth Rate", main = "Time Series Plot - INDPRO Index")
basicStats(ratechg)

#2c Distribution of ratechg
#Histogram
hist(ratechg, prob = T, main = "Histogram of Rate Change Indpro", xlab = "Rate Change INDPRO")
# add approximating normal density curve
xfit<-seq(min(ratechg),max(ratechg),length=40)
yfit<-dnorm(xfit,mean=mean(ratechg),sd=sd(ratechg))
lines(xfit, yfit, col="blue", lwd=2) 
#QQplot
qqnorm(ratechg)
qqline(ratechg)

#2d ACF for the ratechg 
acf(coredata(ratechg), lag = 15, plot = T, main = 'ACF for the Growth Rate of Index')

#2e ACF & PACF function 
#ACF
acf(coredata(ratechg),  plot = T, main = 'ACF for the Growth Rate of Index')
#PACF
pacf(coredata(ratechg), lag = 15, plot = T, main = 'PACF for the Growth Rate of Index')

#2f 
m1=auto.arima(ratechg, max.P=8, max.Q=8, ic="bic", trace=T, stationary=T)
m1= Arima(ratechg, order=c(1,0,2), method='ML')
coeftest(m1)

# RESIDUAL ANALYSIS
# RESIDUAL ANALYSIS
Box.test(m1$residuals,lag=6,fitdf=3, type='Ljung')
Box.test(m1$residuals,lag=8,fitdf=3, type='Ljung')
res =  m1$residuals
acf(coredata(res), main ="ACF - Residual for ARMA(1,0,2)" )

#FIT AN ALTERNATIVE MA(4)) MODEL
m2=Arima(ratechg, order=c(0,0,4), method='ML')
coeftest(m2)
Box.test(m2$residuals,lag=4,fitdf=4,type='Ljung')
Box.test(m2$residuals,lag=6,fitdf=4,type='Ljung')
acf(coredata(m2$residuals), main = "ACF - Residual for MA(4)")

#Plotting the Prediction
plot(forecast.Arima(m1, h=10), include = 50, main = 'Forecast from ARMA(1,0,2)')
plot(forecast.Arima(m2, h=10), include = 50, main = 'Forecast from MA(4)')


pred=forecast.Arima(m1)
pred1=forecast.Arima(m2)
