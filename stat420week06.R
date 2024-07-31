## Data Visualization

# simply plot your data
source(file='series1-4.txt')
xx = series4
time = 1:length(xx)
plot(xx, type='l')
# not stationary

## Detrending Through Regression

# fit a curve through the data
xx.lm = lm(xx~time)
par(mfrow=c(2,2))
plot(xx, xlab="time", ylab="x", main="Series 4", type='l')
plot(xx, type='l', xlab="time")
abline(reg=xx.lm)
plot(resid(xx.lm), ylab="residuals")
acf(resid(xx.lm))
xx.lm
# sequence contains significant autocorrelations up to a lag of 20
# residuals look better, though they are strongly correlated

# we can always try more complicated regression models (quadratic regression)
xx.lm2 = lm(xx~time + I(time^2))
par(mfrow=c(2,2))
plot.ts(xx, xlab="time", ylab="x", main="Series 4")
plot(xx, type='l', xlab="time")
plot(resid(xx.lm2), ylab="residuals")
acf(resid(xx.lm2))
xx.lm2
# we can regress on other variables too (other time-series covariates, lagged variables, etc.)

## the AIC function in R calculates AIC directly
c(AIC(xx.lm), AIC(xx.lm2))

## differencing

x = 1:10
diff(x)
diff(x, differences = 2)

x = (1:10)^2
diff(x)
diff(x, differences = 2)

plot(diff(xx))
acf(diff(xx))

## example
library(astsa)
xx = chicken
plot(xx)

# detrending through regression
xx.lm = lm(x~time(xx))
plot(xx)
abline(xx.lm)

res = resid(xx.lm)
plot(y=res, x=time(xx), type='l', ylab='residual', main='Residual plot')

plot(diff(x))
acf(diff(x))
