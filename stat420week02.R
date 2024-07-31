#independent coin toss example 
set.seed(100)
p <- 0.3

Z <- rbinom(100, 1, p)
Z
sum(Z)

Y <- 2*Z - 1
Y

plot(Y)
lines(Y)