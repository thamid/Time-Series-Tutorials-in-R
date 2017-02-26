library(tseries)
library(fBasics)
library(zoo)
df =  read.csv("D:/MSPA/CSC 425/Homework1/groceries.csv", sep = ',')

#Creating a time series for the variable
units = zoo(df$ToothPaste, as.Date(as.character(df$Date), format = "%d%b%Y"))

#2a
#Histogram for rate
par(mfcol = c(1,1))
hist(units, xlab = 'Units Sold - Toothpaste', prob = TRUE, main = 'Histogram - Toothpaste')
# add approximating normal density curve
xfit<-seq(min(units),max(units),length=20)
yfit<-dnorm(xfit,mean=mean(units),sd=sd(units))
lines(xfit, yfit, col="blue", lwd=2) 
basicStats(units)
# QQ Plot
qqnorm(units)
qqline(units, col = 2) 

#2b Perform Jarque-Bera normality test.
normalTest(units,method=c("jb"))  

#2c Creating the time plot for the time series
plot(units, xlab = "Year" , ylab ='ToothPaste', type = 'l', main = 'Time Series of Units Sold of ToothPaste')

#2d Compute the autocorrelation function
acf(coredata(units), plot=T, lag=15, main = 'Auto Correlation')

#2e Ljung Box 
#For lag  3 
Box.test(units,lag=3,type='Ljung')
#For Lag 6
Box.test(units,lag=6, type='Ljung')
#For Lag 8
Box.test(units,lag=8, type='Ljung')

