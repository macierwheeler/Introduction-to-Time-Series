### Question 1 ###############################################
# part a #################
ma1_gen <- function(theta, len) {
  # MA(1) path realization. xx[1] is the path value at time 1.
  xx <- rep(0,len)
  
  # Generate white noise. Note that it is offset by 1, 
  # so that ww[1] is the white noise at time 0.
  ww <- rnorm(len+1)
  
  # Generate MA(1) path from white noise
  for(i in 1:len) {
    xx[i] <- ww[i+1] + theta*ww[i]
  }
  
  return(list(xx=xx, ww=ww))
}

innov_alg <- function(xx,theta) {
  len <- length(xx)
  ww_est <- rep(0,len)
  xx_hat <- rep(0,len)
  
  for(i in 1:len) {
    xx_hat[i] <- theta*ww_est[i]
    ww_est[i+1] <- xx[i] - xx_hat[i]
  }
  
  return(ww_est)
}

# part b ###################
path1 <- ma1_gen(0.1, 100)
path1_whitenoise <- innov_alg(path1$xx, 0.1)

plot(path1$ww - path1_whitenoise, type='l')

# part c ################
path2 <- ma1_gen(0.5, 100)
path2_whitenoise <- innov_alg(path2$xx, 0.5)

plot(path2$ww - path2_whitenoise, type='l')

# part d ################
path3 <- ma1_gen(0.9, 100)
path3_whitenoise <- innov_alg(path3$xx, 0.9)

plot(path3$ww - path3_whitenoise, type='l')

### Question 2 ##############################
# part a #################
ar1_negloglik <- function(params, data) {
  phi <- params[1]
  sigma2w <- params[2]
  
  n <- length(data)
  x_sum <- 0
  
  for(i in 2:n) {
    x_sum <- x_sum + (data[i] - phi*data[i-1])^2
  }
  
  negloglik <- log(sqrt(1-phi)) * log((2*pi*sigma2w)^(-n/2)) * ((-1/(2*sigma2w))*(((1-phi^2)*data[1]^2)+x_sum))
  negloglik <- negloglik * (-1)
  
  return(negloglik)
}

# part b ################
yy <- arima.sim(model=list(ar=0.5), n=100)

# part c ################
optim(par=c(.5,.5), fn=ar1_negloglik, method="CG", data=yy)

# part d ############
arima(yy, order=c(1,0,0), include.mean=FALSE)
