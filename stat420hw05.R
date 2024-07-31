### Question 2 ##########################################################
# part b ######################

phi <- c(0.1, 0.57, 0.135)
polyroot(c(1, -phi))
theta <- c(1.3, 0.26, -0.112)
polyroot(c(1, theta))

my_path <- arima.sim(model = list(ma=theta, ar=phi), n = 10000)
plot(my_path[0:100], type='l')

acf(my_path)
pacf(my_path)

arima(my_path, order=c(3,0,3), include.mean=FALSE)
c(phi, theta)

my_path_rev <- rev(my_path)
plot(my_path_rev[9901:10000], type='l')

acf(my_path_rev)
pacf(my_path_rev)

arima(my_path_rev, order=c(3,0,3), include.mean=FALSE)
c(phi, theta)
