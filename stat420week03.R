set.seed(100)
p <- 0.5

# simulating i.i.d. coin flips
Z <- rbinom(100, 1, p)
Y <- 2*Z - 1
plot(Y)
lines(Y)

# Simulating the random walk
X <- rep(0,100)
for (t in 2:100) {
  X[t] = X[t-1] + Y[t]
}
plot(X)
lines(X)

# simulating the moving average
c0 <- 1
Epsilon <- rnorm(100, 0, 2)
X <- rep(c0, 100)
for (t in 2:100) {
  X[t] = c0 + (0.5) * (Epsilon[t] + Epsilon[t-1])
}
plot(X)
lines(X)
