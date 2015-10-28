###########################################################
##Causal Inference for Medicine and Public Health
##Time Series Homework Problem Set
############################################################

##Set Working Data
setwd("~/Dropbox/Causal Inference 2014")

rm(list=ls())
par(mfrow = c(1, 1))

library(foreign)
library(forecast)
library(TSA)
library(lmtest)
library(nlme)

##Open Dataset

crash = read.dta("Lab6.time.series.fars.dta")
head(crash)

###############
## :: Data Description
## :: state - factor variable identifying state records
## :: Year 
## :: Month 
## :: ratio of the # of drinking drivers in fatal crashes to the # of nondrinking drivers in fatal crashes. 
## :: lnratio : Natural Log of the Ratio Variable
## :: time - a  continuous  variable that numbers observation 1 to 180, by state
############

# ::::: create treatment variable (indicator of demonstration prohect) :::::::::::::::
# ::::: Demonstration Project was conducted in 2000 :::::
crash$treatment = ifelse(crash$year < 2000, 0, 1)
table(crash$year, crash$treatment)


##Generate State Specific Time Series


alabama = crash[crash$state=="Alabama",  5]
alabama.ts = ts(alabama, frequency=12, start=c(1990,1)) 
##frequency tells R this is monthy data, the start option tells R the data starts in 1990


georgia = crash[crash$state=="Georgia", 5]
georgia.ts = ts(georgia, frequency=12, start=c(1990,1))

tennessee = crash[crash$state=="Tennessee", 5]
tennessee.ts = ts(tennessee, frequency=12, start=c(1990,1))


georgia.ts
tennessee.ts
alabama.ts

##Basic Plotting
ts.plot(tennessee.ts) ##Plotting method for objects inheriting from class "ts"##
abline(v = 2000, col = "red")
seasonplot(tennessee.ts)

# Decomposing Time Series
# Decomposing is the process of separating the time sereis 
# it into its constituent components, 
# which are usually consist of a trend component, an irregular component, 
# and if it is a seasonal time series, a seasonal component.

# To estimate the trend component and seasonal component of a seasonal time
# series use the decompose()" function from forecast package 

t.components = decompose(tennessee.ts)

# The estimated values of the seasonal, trend and irregular components are now
# stored as variables in t.components

t.components


# plot the estimated trend, seasonal, and irregular components of the time series

plot(t.components)
abline (v=2000, col = "red")

# Seasonal Adjustment
t.ts.adjusted = tennessee.ts - t.components$seasonal

#plot the seasonally adjusted time series using the "plot()" function
plot(t.ts.adjusted)
abline (v=2000, col = "red")

## :: ARIMA Models :: ##

# Identify the Arima Model

# ARIMA models are defined for stationary time series If you have a
# non-stationary time series, you will need  'difference' the time series
# until you obtain a stationary time series

# You can difference a time series using the "diff()" function

t.diff1 = diff(tennessee.ts, differences=1)
plot(t.diff1)


t.diff2 = diff(tennessee.ts, differences=2)
plot(t.diff2)

# You can do formal tests for stationarity data with a "unit root test"



## Identifying an ARIMA Model
par(mfrow = c(1, 1))
acf(tennessee.ts, lag.max=60)
pacf(tennessee.ts, lag.max=60)

acf(t.diff1, lag.max=60)
pacf(t.diff1, lag.max=60)


# The ACF will first test whether adjacent observations are autocorrelated; that
# is, whether there is correlation between observations #1 and #2, #2 and #3, #3
# and #4, etc.

# The Partial Autocorrelation Function (PACF) removes the effect of shorter lag
# autocorrelation from the correlation estimate at longer lags.

# The ACF and PACF each vary between plus and minus one. Values closer to plus or
# minus one indicate strong correlation. The confidence limits are provided to
# show when ACF or PACF appears to be significantly different from zero. In
# other words, lags having values outside these limits (shown as blue dashed lines)
# should be considered to have significant correlation.

t.model1 = auto.arima(tennessee.ts)
t.model1

## This is the  parameters selected with the auto.arima() fucntion
t.model2 = Arima(tennessee.ts, order=c(2,1,1),seasonal=list(order=c(1,0,1)), include.drift=TRUE)
t.model2

##Check the ACF/PACF on the residuals
par(mfrow = c(1, 1))
acf(t.model1$residuals)
pacf(t.model1$residuals)

Box.test(t.model1$residuals)

##You can use AIC/BIC to compare different ARIMA modles

t.model1b = Arima(tennessee.ts, order=c(1,0,0))
t.model1b

par(mfrow = c(1, 1))
acf(t.model1b$residuals)
pacf(tmodel1b$residuals)
Box.test(t.model1b$residuals)

## :: Forcasting :: ##

forecast = forecast.Arima(t.model1, level=c(95))
plot(forecast)

#Important! forecast.Arima is part of the forecast package. 
# For it to work properly you time series model must either be 
# estimated with the auto.arima() function or the Arima() function. 
# Forecast.Arima will NOT work if you use the arima() function.


## :: Evaluating the Intervention :: ##

# Indentify the model in the preintervention period

tennessee = crash[crash$state=="Tennessee" & crash$year < 2000, 5]
tennessee.pre = ts(tennessee, frequency=12, start=c(1990,1))

model.pre = auto.arima(tennessee.pre)
model.pre

##Check the ACF/PACF on the residuals
par(mfrow = c(1, 1))
acf(model.pre$residuals)
pacf(model.pre$residuals)
Box.test(model.pre$residuals)

## :: Please note - we will be estimating the time series with a different package. The forcast package
# cannot handle our intervention covariate. The following set of models will be estimated with functions 
# from the TSA package. arima() and Arima() are NOT the same function.

# ##Create a new data frame to examine the Tennessee data
tenn2 = crash[crash$state=="Tennessee", ]



t.model3 <- arimax(tenn2$lnratio, order=c(1,1,1),seasonal=list(order=c(2,0,0), frequency = 12),transform.pars= T); t.model3


par(mfrow = c(1, 1))
acf(t.model3$residuals)
pacf(t.model3$residuals)
Box.test(t.model3$residuals)

##Test for outliers
detectAO(t.model3, alpha = 0.05, robust = TRUE)


## :: Estimate the Model with the Intervetion :: ##
t.model4 <- arimax(tenn2$lnratio, order=c(1,1,2),seasonal=list(order=c(2,0,0),period=12),  
                   xtransf=tenn2$treatment, transfer=list(c(1,0)), transform.pars= T); t.model4


t.model4 <- arimax(tenn2$lnratio, order=c(1,1,2),seasonal=list(order=c(2,0,0),period=12),  
                   xreg=tenn2$treatment, transform.pars= T); t.model4

par(mfrow = c(1, 1))
acf(t.model4$residuals)
pacf(t.model4$residuals)
Box.test(t.model4$residuals)



##Plot the Intervention Model
full.noinv<-tenn2$lnratio-t.model3$residuals
full.inv<-tenn2$lnratio-t.model4$residuals
plot(y=tenn2$lnratio, x=tenn2$time, type= "l")
abline(v = 133, col = "orange")
lines(y=full.noinv, x=tenn2$time, lty=2, col = "blue")
lines(y=full.inv, x=tenn2$time, lty=2, col = "red")



# ::: Segemented Regression ::

tenn2$intervention = ifelse(tenn2$year >2000,1,0)

table(tenn2$intervention, tenn2$year)
tenn2$time = 1:180
tenn2$time2 = tenn2$time - 132
tenn2$postslope = ifelse(tenn2$intervention==0, 0, tenn2$time2 )
tenn2$interaction = tenn2$time * tenn2$intervention 


model1 = lm(lnratio ~ time + intervention + postslope, data = tenn2); model1

##Alternate coding 
model1.b = lm(lnratio ~ time + intervention + time:intervention, data = tenn2); model1.b

##The Durbin-Watson statistic is a test statistic used to detect 
##autocorrelation in the residuals (prediction errors) from a regression analysis.
## the dwtest function in the lmtest package

dwtest(ratio ~ time + intervention + postslope, data = tenn2)

## R does not have a canned function for Prais-Winston Regression
## so we'll use the gls() function in the nlme package to fit a linear 
## model with autocorrelated errors by ML


model2 = gls(lnratio ~ time + intervention + postslope, data = tenn2, correlation=corARMA(p=2))
summary(model2)
intervals(model2)
plot(model2$residuals)
plot(model2)

# ::::: plot data :::::

pre.inv <- predict(model2, data.frame(intervention= 0, time = 1:132, postslope = 0))
post.inv <- predict(model2, data.frame(intervention = 1, time = 133:180, postslope = 1:48))
plot(lnratio ~ time, data = tenn2, cex = .7)
abline(v = 133, col = "blue")
lines(1:132,pre.inv, col = 'red', lwd = 2)
lines(133:180, post.inv, col = 'red', lwd = 2)

# extrapolate pre-intervention time series
pre.extrap <- predict(model2, data.frame(intervention= 0, time = 133:180, postslope = 1:48))
lines(133:180, pre.extrap, col = 'blue', lwd = 2, lty = 2)

