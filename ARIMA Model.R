###################### Market Analysis (ARIMA Model) ################################
######################  7-Day Moving Average ################################

library(forecast)
library(ggplot2)
library(tseries)
library(zoo) ##rolling mean (moving average)

# Read Data
data<-read.csv(file="data.csv", header=TRUE)

#Transform to time series
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
tsdata = ts(data[,2],start = c(2018,1), frequency = 53)

#Plot
plot(tsdata, xlab='', ylab = '', main='Market State')


## Forecast ##
# ARIMA MODEL #
ARIMAfit.data = auto.arima(tsdata, D=1,trace=F,stepwise = T)
summary(ARIMAfit.data)
pred.data = forecast(ARIMAfit.data,h = 178) #h is the length of data
pred.data
mean(pred.data$mean)
max(pred.data$mean)

#Plot with predicted values
plot(tsdata, type='l',main='Expected Market State',
     xlab = '',ylab = '',xlim=c(2018,2022),ylim=c(0,.5), lwd=.25, col="grey")
lines((pred.data$mean),col='light blue', lwd=.25) #lwd = font size
abline(h=c(1), col="red",lty=2) #add a cut off dashed line at 1

## Moving Average / Rolling Mean##
sma=rollmean(tsdata, 7, align="right")
sma1=rollmean(pred.data$mean, 7, align="left")
lines(sma, col='#00B050',lwd=2)
lines(sma1, col='blue',lwd=2)