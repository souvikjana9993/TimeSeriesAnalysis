library(astsa)
library(xts)
help("AirPassengers")
plot(AirPassengers)
#The AirPassengers data show a handful of important qualities, 
#including seasonality, trend, and heteroscedasticity, 
#which distinguish the data from standard white noise.
###plot(djia$Close)
plot(soi)
help(soi)
plot(diff(log((AirPassengers))))
#log the data to remove the heteroskedasticity 
#and differencing to make mean stationary 

par(mar=c(1,1,1,1))
#now lets look for tow more series globtemp and cmort data
# Plot globtemp and detrended globtemp
par(mfrow = c(2,1))
plot(globtemp)
plot(diff(globtemp))

# Plot cmort and detrended cmort
par(mfrow=c(2,1))
plot(cmort)
plot(diff(cmort))


# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))

# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

###lets simulate ar models with arima.sim() from the astsa package
# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)

# Generate and plot an MA(1) with parameter .9 by filtering the noise
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = .9), n = 200)  
plot(MA)

# Generate and plot an AR(1) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -.75)), n = 200) 
plot(AR)

###ARMA models
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
plot(acf2(x))

# Fit an AR(1) to the data and examine the t-table
sarima(x,p=1,d=0,q=0)
## thereby using the above lines of code we can determine the p,d,q for 
#a real world time series data

##sarima for a ARMA(2,1) model
y <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)
# Plot x
plot(y)

# Plot the sample P/ACF of x
plot(acf2(y))

# Fit an ARMA(2,1) to the data and examine the t-table
sarima(y,2,0,1)
#so we see that a sarima(2,0,1) predicts the arma(2,1) pretty well


###RESIDUAL ANALYSIS
dl_varve = diff(log(varve))
plot(dl_varve)
sarima(dl_varve,0,0,1)

# Fit an MA(1) to dl_varve   
sarima(dl_varve,0,0,1)

# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve,0,0,2)

# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve,1,0,1)

#In each run, check the four residual plots as follows:

#1.The standardized residuals should behave as a white noise sequence with mean zero and variance one. Examine the residual plot for departures from this behavior.
#2.The sample ACF of the residuals should look like that of white noise. Examine the ACF for departures from this behavior.
#3.Normality is an essential assumption when fitting ARMA models. Examine the Q-Q plot for departures from normality and to identify outliers.
#4.Use the Q-statistic plot to help test for departures from whiteness of the residuals.



####NEW EXAMPLE
# Plot the sample P/ACF pair of the differenced data 
acf2(diff(globtemp))

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, p = 0, d = 1, q = 2)
"""
The data in globtemp (from astsa) are the annual global temperature deviations to 2015. In this exercise, you will use established techniques to fit an ARIMA model to the data. A plot of the data shows random walk behavior, which suggests you should work with the differenced data. The differenced data diff(globtemp) are also plotted.

After plotting the sample ACF and PACF of the differenced data diff(globtemp), you can say that either

The ACF and the PACF are both tailing off, implying an ARIMA(1,1,1) model.
The ACF cuts off at lag 2, and the PACF is tailing off, implying an ARIMA(0,1,2) model.
The ACF is tailing off and the PACF cuts off at lag 3, implying an ARIMA(3,1,0) model. Although this model fits reasonably well, it is the worst of the three (you can check it) because it uses too many parameters for such small autocorrelations.
After fitting the first two models, check the AIC and BIC to choose the preferred model.
"""

#FORECASTING ARIMA

# Forecast the data 20 time periods ahead
sarima.for(globtemp, n.ahead = 35, p = 1, d = 1, q = 1) 
lines(y)



#### USING AUTO ARIMA
fit <- auto.arima(globtemp)
plot(forecast(fit,h=35))


