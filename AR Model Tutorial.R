library(fBasics)
library(zoo)
library(tseries)
setwd("E:/QUARTER 2 DEPAUL UNIVERSITY/CSC 425/coding")
groc= read.table("groceries.csv", header=T, sep=',') 
head(groc)
myd= data.frame(groc$D , groc$Tooth)
head(myd)
hist(myd$groc.Tooth,prob=TRUE, main="Toothpaste", xlab="Sales")
qqnorm(myd$groc.Tooth)
qqline(myd$groc.Tooth, col=2)# col=2 is just for color red, col =1 is for black
grocts=zoo(myd$groc.Tooth,as.Date(as.character(myd$groc.D), format="%d-%b-%Y"))
grocts
head(grocts)
growth = (grocts - lag(grocts, k=-1))/lag(grocts, k=-1)
plot(grocts, xlab="Date", ylab="Sales", main="Toothpaste")
# 8. NORMALITY TESTS
# Perform Jarque-Bera normality test.
normalTest(growth,method=c("jb"))  
# Fat-tail test
k_stat = kurtosis(growth)/sqrt(24/length(growth))
print("Kurtosis test statistic")
k_stat
print("P-value = ")
2*(1-pnorm(abs(k_stat)))

#Skewness test
skew_test = skewness(growth)/sqrt(6/length(growth))
skew_test
#P-value
2* (1-pnorm(abs(skew_test)))


#9 COMPUTE ACF, STANDARD ERRORS, PACF AND PLOT CORRELOGRAM
# NOTE: acf(xvar) will display lags in integers if xvar is a numeric vector
# Thus, we use coredata(tsobject) to retrieve numeric values of ts object.
#plots acf (correlogram)
acf(coredata(growth), plot=T, lag=15)

# COMPUTES ACF VALUES AND ST. ERRORS
#saves scf values into a variable
acf_value=acf(coredata(growth), plot=F, lag=15)
# PACF plot
pacf(growth)

#10 COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 3
Box.test(growth,lag=3,type='Ljung')
# to Lag 6
Box.test(growth,lag=6, type='Ljung')


plot(crudets, main="Time plot", xlab="Date", ylab="Price")
crudelag = lag(crudets, k=-1)
plot(crudets,crudelag, main="Autocorrelation plot", xlab="crudets", ylab="crudelag")
cor(crudets[-1],crudelag)
diff(crude$price,crude$price(t-1))
pricedif = diff(crudets)
rate=(crudets-crudelag)/crudelag
rate
rate= (crude$price - crude$price(t-1)/ crude$price(t-1))
rate = diff(crude$price)/crude$price[-nrow(crude),] * 100
rate = (crudelag/crude$price.t.1.)
rate = ((crude$price - crude$price.t.1.)/ crude$price.t.1.)*100
ret= diff(crudets)
rate = ret/crude$price.t.1.


head(growth)
growth[ ,2]
rate = (growth[ ,2])*100
rate #ask about this!
plot(growth, xlab = "what", ylab = "what")
par(mfcol=c(1,1)) 
hist(growth)
basicStats(growth)
qqnorm(growth)
qqline(growth, col=2)
