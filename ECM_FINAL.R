library(astsa)
library(xts)
library(tseries)
library(CADFtest)
library(forecast)
help("globtemp")
plot(globtemp)
plot(diff(globtemp))
#stocks=djia$Close
par(mfrow = c(2,1))
plot(globtemp)
plot(diff(globtemp))

#converting to a stationary time series
globtemp_diff = diff(globtemp)
sum(is.na(globtemp_diff))
#stocks_diff_log=stocks_diff_log[complete.cases(stocks_diff_log),]

#doing a adf test
tseries::adf.test(globtemp_diff)

##WE SEE THAT ADF statistic is a negative number with lower values indicating
#stronger failure to reject the null hypothesis
CADFtest(globtemp_diff,type = 'trend')
plot(acf2(globtemp))
#shows acf lags over time indicating a AR model
#PACF drops after first lag indicating a AR(1)model
#PACF and ACF are not correlated

plot(acf2(globtemp_diff))
#for the differenced model the ACF falls after 1 lag so it may be a MA(1) model
#So it is a ARMA model


#After plotting the sample ACF and PACF of the differenced data diff(globtemp)

###RESIDUAL ANALYSIS

# Fit an ARIMA(1,1,1) model to globtemp
model1=sarima(globtemp_diff, p = 1, d = 0, q = 1)


# Fit an ARIMA(1,0,1) model to globtemp
model2=sarima(globtemp_diff, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
model3=sarima(globtemp_diff, p = 1, d = 0, q = 2)

print(model1$BIC)
###so model1 has the highest BIC

#THEREFORE the globetemp_diff can be predicted using a ARIMA(1,1,1)

#using autoarima
library(forecast)
auto.arima(globtemp)

#FORECASTING ARIMA

# Forecast the data 2 time periods ahead
sarima.for(globtemp, n.ahead = 2, p = 1, d = 1, q = 1) 

