library(tseries)
library(fBasics)
library(zoo)

#Loading the data into R
df =  read.csv("D:/MSPA/CSC 425/Homework1/crudeoil_w0416.csv", sep = ',')

#Creating a time series of spot prices
spot = zoo(df$price, as.Date(as.character(df$date), format = "%d-%b-%y"))

#Creating the time plot for the time series
plot(spot, xlab = "Year" , ylab ='Price', type = 'l', main = 'Time Series of Spot Prices')


#1b
#Computing the Percentage Change Rate of Spot 
rate = (spot - lag(spot, k =-1))/lag(spot, k = -1)
head(rate)

#1c
#Histogram for rate
par(mfcol = c(1,1))
hist(rate, xlab = 'Percentage Change Rate of Spot Prices', prob = TRUE, main = 'Histogram of  Rate')
# add approximating normal density curve
xfit<-seq(min(rate),max(rate),length=40)
yfit<-dnorm(xfit,mean=mean(rate),sd=sd(rate))
lines(xfit, yfit, col="blue", lwd=2) 
#Computing the basic statistics
basicStats(rate)
#QQ Plot
qqnorm(rate)
qqline(rate, col = 2) 

#1d Symmetry test
#Skewness test
skew_test = skewness(rate)/sqrt(6/length(rate))
skew_test
#P-value
2* (1-pnorm(abs(skew_test)))

#1e Kurtosis Test
# Fat-tail test
k_stat = kurtosis(rate)/sqrt(24/length(rate))
print("Kurtosis test statistic")
k_stat
print("P-value = ")
2*(1-pnorm(abs(k_stat)))

#1f Jarque-Bera normality test.
normalTest(rate,method=c("jb"))  


#1g Time plot of Rate
plot(rate, xlab = "Year" , ylab ='Percentage Change of Rate', type = 'l', main = 'Time Series of Rate')


#Compute the autocorrelation function
acf(coredata(rate), plot=T, lag=15, main = 'Auto Correlation Function')


