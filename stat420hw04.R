### Question 2 ############################################################
# part 4 ############################
theta <- 0.8
size <- 10000
mean <- 0
var <- 1 + theta^2

x <- rep(0, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (theta * w[i - 1]) + w[i]
}

plot(x[1:100], type='l')

# part 5 ############################
theta <- -0.8
size <- 10000
mean <- 0
var <- 1 + theta^2

x <- rep(0, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (theta * w[i - 1]) + w[i]
}

plot(x[1:100], type='l')

### Question 3 #############################################################
# part 1 ############################
theta <- 0.8
size <- 10000
mean <- 0
var <- 1 / (1 - theta^2)

x <- rep(0, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (theta * x[i - 1]) + w[i]
}

plot(x[1:100], type='l')

# part 2 ############################
size <- 10000
x_avg <- rep(0, size)

for (i in 1:size) {
  avg <- mean(x[1:i])
  x_avg[i] <- avg
}

plot(x_avg[1:1000], type='l', xlab = "Time")

# part 3 ############################
theta <- -0.8
size <- 10000
mean <- 0
var <- 1 / (1 - theta^2)

x <- rep(o, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (theta * x[i - 1]) + w[i]
}

plot(x[1:100], type='l')

x_avg <- rep(0, size)

for (i in 1:size) {
  avg <- mean(x[1:i])
  x_avg[i] <- avg
}

plot(x_avg[1:1000], type='l', xlab = "Time")

# part 4 #################################
size <- 10000

y100 <- vector(mode = "list", length = 100)
y_avg100 <- vector(mode = "list", length = 100)

for (j in 1:100) {
  y <- rep(0, size)
  w <- rnorm(size, 0, 1)
  y[1] <- w[1]
  
  for (i in 2:size) {
    y[i] = y[i-1] + w[i]
  }
  y100[j] <- list(y[1:100])

  y_avg <- rep(0, size)
  for (i in 1:size) {
    avg <- mean(y[1:i])
    y_avg[i] <- avg
  }
  y_avg100[j] <- list(y_avg)
}

matplot(do.call(cbind, y100), type='l')
matplot(do.call(cbind, y_avg100), type='l')

### Question 4 #############################################################
# part 1 ###########################
phi <- 0.8
size <- 100
mean <- 0
var <- 1 / (1 - phi^2)

x <- rep(0, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (phi * x[i - 1]) + w[i]
}

acf(x)
acf_values <- acf(x)$acf[0:10]

# part 2 ##########################
acf_values[1]
acf_values[2]

# part 3 ###########################
phi <- 0.8
size <- 1000
mean <- 0
var <- 1 / (1 - phi^2)

x <- rep(0, size)
w <- rnorm(size, 0, 1)

x[1] <- rnorm(1, mean, var)
for (i in 2:size) {
  x[i] = (phi * x[i - 1]) + w[i]
}

acf(x)
acf_values <- acf(x)$acf[0:10]

acf_values[1]
acf_values[2]
  
