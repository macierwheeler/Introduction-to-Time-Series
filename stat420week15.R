# let us create a random 4-state Markvov transition matrix T
Tmat <- matrix(rexp(16), nrow=4)
Tmat <- t(Tmat/rowSums(Tmat))
Tmat

pi_t <- matrix(c(.5,.5,0,0), nrow=4)
h <- 5
for (i in 1:h) {
  pi_t <- Tmat %*% pi_t
  print(pi_t)
}

# as h increases, the initial distribution is "forgotten" and pi^t converges
# to the stationary distribution (this is called mixing)

# this method of computing the stationary distribution is called the power method
# start with some arbitrary distribution over states
# repeatedly multiply by the transition matrix until the result stabilizes

# can also directly compute using the eigen function