## Question 1 and 2
n <- 1000
var <- 1
theta <- 0.5

Y <- rep(0, n)
Epsilon <- rnorm(n, 0, var)

Y[1] <- (Epsilon[1]) / sqrt(1 - theta^2)
for (t in 2:n) {
  Y[t] <- (theta * Epsilon[t - 1]) + Epsilon[t]
}

acf(Y)
acf_values <- acf(Y)

## Question 4
n <- 50
var <- 1
theta <- 0.5

X_orig <- rep(0, n)
Epsilon <- rnorm(n, 0, var)

X_orig[1] <- (Epsilon[1]) / sqrt(1 - theta^2)
for (t in 2:n) {
  X_orig[t] <- (theta * Epsilon[t - 1]) + Epsilon[t]
}

acf(X_orig)
acf_values <- acf(X_orig)
acf_orig <- acf_values$acf[0:11]

## Question 5 and 6
acf_matrix <- matrix(0, nrow = 100, ncol = 11)
for (r in 1:100) {
  X_temp <- sample(X_orig, 50, replace = FALSE)
  acf_values_temp <- acf(X_temp)$acf[0:11]
  for (c in 1:11) {
    acf_matrix[r, c] <- acf_values_temp[c]
  }
}

low_quant <- c()
high_quant <- c()
for (col in 1:11) {
  low_value <- quantile(acf_matrix[, col], probs = c(0.025))
  high_value <- quantile(acf_matrix[, col], probs = c(0.975))
  low_quant <- c(low_quant, low_value)
  high_quant <- c(high_quant, high_value)
}

## Question 7
plot(acf_orig, type='l', xlab='Lag')
lines(low_quant, type='l', lty=2, col='blue')
lines(high_quant, type='l', lty=2, col='blue')
