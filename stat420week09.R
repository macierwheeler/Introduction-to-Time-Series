### simulating ARMA(p, q) models 

## simulating MA(q) with q = 2
theta <- c(-1.5, .56) # 1 - 1.5z + .56z^2

# invertible?
polyroot(c(1, theta)) # finds roots of 1 + theta[1] z + ... + theta[q] z^q
# all larger than 1, so invertible

# simulate a path from this model
ma_path <- arima.sim(model=list(ma=theta), n=5000)
plot(ma_path[1:500], type='l')
ma_acf <- acf(ma_path) # estimates for autocorrelation function
# starts at lag 0
print(ma_acf$acf[1:10])
# when acf gets closer to 0 that tells us what process the model is

## or simulate a path manually
ww <- rnorm(5002)
xx <- rep(0, 50002)

for (ii in 3:5002) {
  xx[ii] <- ww[ii] + theta[1]*ww[ii - 1] + theta[2]*ww[ii - 2]
}

xx <- xx[3:5002]
ma_acf <- acf(xx)
print(ma_acf$acf[1:10])

# the function ARMAacf lets us calculate the theoretical acf
ma_acf_true = ARMAacf(ma=theta, lag.max=10)
barplot(ma_acf_true, space=100, axes=T)
abline(h=0)

# we can plot the pacf using acf with option type = 'pacf' or just use the command pacf
# starts at lag 1
pacf(ma_path)
ma_pacf_true = ARMAacf(ma=theta, pacf=TRUE, lag.max=10)
barplot(ma_pacf_true, space=100, axes=T)
abline(h=0)

# we can use the arima function to fit the model parameters
arima(ma_path, order=c(0,0,2), include.mean=FALSE)
# if include.mean = FALSE it also estimates mean of autocorrelation function (intercept)
theta
# a shorter path will fit model parameters worse
# we can try different orders of the MA process, for example MA(3) would be decent, but MA(1) would be bad
# simpler model can not recover enough data, but more complicated model could potentially overfit but would recover the data
# can compare different models by looking at the log likelihood and AIC (want a smaller AIC and a smaller log likelihood)

# we can also try fitting AR processes
arima(ma_path, order=c(2,0,0), include.mean=FALSE)

## consider an AR(p) model with parameters phi(1), ..., phi(p)
phi <- c(.3, .1)
polyroot(c(1, -phi))
# all roots lie outside unit circle, so causal
ar_path <- arima.sim(model=list(ar=phi), n=5000) # for stationary AR processes
plot(ar_path[1:500], type='l')

# or simulate manually
xx <- rep(0, 5000)
xx[1] <- -10
xx[2] <- -10

for (ii in 3:5000) {
  xx[ii] <- phi[1]*xx[ii - 1] + phi[2]*xx[ii - 2] + rnorm(1)
}

plot(xx, type='l')

# estimate autocorrelation function
acf(ar_path)

ar_acf_true = ARMAacf(ar=phi, lag.max = 10)
barplot(ar_acf_true, space = 100, axes = T)
abline(h=0)

# estimate partial autocorrelation function
pacf(ar_path)

ar_acf_true = ARMAacf(ar=phi, pacf = TRUE, lag.max = 10)
barplot(ar_acf_true, space = 100, axes = T)
abline(h=0)

# estimate parameters
arima(ar_path, order=c(2,0,0), include.mean=FALSE)
phi

arima(ar_path, order=c(0,0,2), include.mean=FALSE)

## random walk (can't use arma.sim)
xx <- rep(0, 5000)
xx[1] <- 0

for (ii in 2:5000) {
  xx[ii] <- xx[ii - 1] + rnorm(1)
}

plot(xx, type='l')

## simulating arma models
arma_path <- arima.sim(model=list(ma=theta, ar=phi), n=5000)
plot(arma_path[1:500], type='l')

acf(arma_path)
pacf(arma_path)
# both decrease gradually to 0

arima(arma_path, order=c(2,0,2), include.mean=FALSE)
c(phi, theta)
arima(arma_path, order=c(2,0,0), include.mean=FALSE)
arima(arma_path, order=c(0,0,2), include.mean=FALSE)
