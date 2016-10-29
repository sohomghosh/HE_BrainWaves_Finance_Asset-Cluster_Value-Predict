setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)
timese<-ts(train[,102])
plot.ts(timese)
plot(timese)
timese_dec<-decompose(timese)
#Decompose does not work as time series has only 2 periods.. No seasonal effects

library(forecast)
timese_ma <- ma(timese,3)
plot.ts(kingstimeseries)
plot.ts(kingstimeseriescomponents)


library(fpp)


library(tseries)
adf.test(timese,alternative="stationary")
kpss.test(timese)
acf(timese)
acf(timese,lag=30)



#Seasonal Decomposition
# Seasonal decomposition
fit <- stl(timese, s.window="period")
plot(fit)

# additional plots
monthplot(myts)
library(forecast)
seasonplot(myts)




######## Exponential Models  ###################
# simple exponential - models level
fit <- HoltWinters(timese, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(timese, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(timese)
# predictive accuracy
library(forecast)
accuracy(fit)
# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))

##############ARIMA Models#################
'''
lag(ts, k)	lagged version of time series, shifted back k observations
diff(ts, differences=d)	difference the time series d times
ndiffs(ts)	Number of differences required to achieve stationarity (from the forecast package)
acf(ts)	autocorrelation function
pacf(ts)	partial autocorrelation function
adf.test(ts)	Augemented Dickey-Fuller test. Rejecting the null hypothesis suggests that a time series is stationary (from the tseries package)
Box.test(x, type="Ljung-Box")	Pormanteau test that observations in vector or time series x are independent
'''
# fit an ARIMA model of order P, D, Q
fit <- arima(myts, order=c(p, d, q))
             
# predictive accuracy
library(forecast)
accuracy(fit)
             
# predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5))



##################AUTOMATED FORECASTING###############
library(forecast)
# Automated forecasting using an exponential model
fit <- ets(timese)

# Automated forecasting using an ARIMA model
fit <- auto.arima(timese)


lg<-lag(timese,2)


library(DataCombine)
DataSlid1 <- slide(train, Var = "Y", slideBy = -2)

