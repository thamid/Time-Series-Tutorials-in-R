library(forecast)
library(zoo)
library(tseries)
library(fBasics)
library(ggplot2)
library(lmtest)

#Question 1 
costco = read.csv("F:/MSPA/CSC 425/Homework 4/costco.csv")
x = costco$eps
#time object
costco_eps = ts(costco$eps, start = c(2002,1),freq = 4)

#a Time plot
plot(costco_eps, type = 'l', xlab = 'Time', ylab = 'Earning Per Share', main  = 'Time Plot - EPS Costco')
hist(costco_eps, prob = T, main = "Histogram of Rate Change Indpro", xlab = "Rate Change INDPRO")
# add approximating normal density curve
xfit<-seq(min(costco_eps),max(costco_eps),length=40)
yfit<-dnorm(xfit,mean=mean(costco_eps),sd=sd(costco_eps))
lines(xfit, yfit, col="blue", lwd=2) 
#QQplot
qqnorm(costco_eps)
qqline(costco_eps)

#Applying the Logarithm to EPS
costco_lg = log(x)
costco_log = ts(costco_lg, start = c(2002,1),freq = 4)

plot(costco_log, type = 'l', xlab = 'Time', ylab = 'Earning Per Share', main  = 'Time Plot - EPS Costco')
#Histogram
hist(costco_log, prob = T, main = "Histogram of Rate Change Indpro", xlab = "Rate Change INDPRO")
# add approximating normal density curve
xfit<-seq(min(costco_log),max(costco_log),length=40)
yfit<-dnorm(xfit,mean=mean(costco_log),sd=sd(costco_log))
lines(xfit, yfit, col="blue", lwd=2) 
#QQplot
qqnorm(costco_log)
qqline(costco_log)

acf(as.vector(costco_log), lag =40, plot = T, main = 'ACF for Costco EPS after First Differencing')

#Computing the first difference 
d_eps = diff(costco_log)
#Time series plot
plot(d_eps, type = 'l', xlab = 'Time', ylab = 'Earning Per Share', main  = 'Time Plot - First Difference')
# ACF & PACF
acf(as.vector(d_eps), lag =20, plot = T, main = 'ACF for Costco EPS after First Differencing')

#Auto Arima Function
auto.arima(costco_log, ic="bic", trace=T, seasonal = T)

#
m1= Arima(d_eps, order=c(1,0,0), seasonal = list(order = c(1,1,0), period = 4), include.drift = T, method='ML')
coeftest(m1)
#Residuals
#Null Hypothesis <- No Serial Correaltion
#lag - 4
Box.test(m1$residuals, lag = 4, "Ljung-Box", fitdf = 1)
#lag - 3
Box.test(m1$residuals, lag = 3, 'Ljung-Box', fitdf = 1)
#acf of residuals
acf(m1$residuals, plot = T, main = 'ACF - Residuals')
pacf(as.vector(m1$residuals), plot = T, main = 'PACF -Residual')



#e -> Forecast 
f1 = forecast(m1, h = 4)
#Prediction in Original Scale
exp(f1$mean)
