### Question 1 #############################################################
# part a #######################
arch1_gen <- function(len, alpha0, alpha1) {
  
  # Vector of variances and values
  sigma <- rep(0, len)
  rr <- rep(0, len)
  
  # Initialize both vectors to 1 at time 1
  rr[1] <- 1
  sigma[1] <- 1
  
  w <- rnorm(len)
  
  for (i in 2:len) {
    sigma2 <- alpha0 + alpha1*(rr[i-1]^2)
    sigma[i] <- sqrt(sigma2)
    rr[i] <- sigma[i] * w[i]
  }
  
  return(list(rr=rr, sigma=sigma))
}

# part b #####################
arch1 <- arch1_gen(10000, 2, 0.6)

plot(arch1$rr, type='l')
plot(arch1$sigma, type='l')

mean(arch1$rr)
var(arch1$rr)

# part c #######################

length(arch1$rr[arch1$rr > 7.5])
newarch1 <- rnorm(10000, 0, sqrt(5))
length(newarch1[newarch1 > 7.5])

# part d #######################

acf(arch1$rr)
acf(arch1$rr^2)

### Question 2 ############################################################
# part a ##########################
markov_sim <- function(len) {
  A <- matrix(c(.4, .3, .3, .1, .7, .2, .5, .5, 0), nrow=3)

  # Below, the L in 0L and 1L stands for 'long' and ensures xx contains integers
  # rather than floats. You don't have to worry about this in your code.
  xx <- rep(0L, len)
  xx[1] <- 1L
  
  for (i in 2:len) {
    a <- A[,xx[i-1]]
    xx[i] <- sample(3, size=1, prob=a)
  }
  
  return(xx)
}

markov3 <- markov_sim(500)
probs <- table(markov3)
probs[1]/500
probs[2]/500
probs[3]/500

# part b #######################
A <- rbind(c(-0.6,0.1,0.5), c(0.3,-0.3,0.5), c(0.3,0.2,-1), c(1,1,1))
B <- c(0,0,0,1)
qr.solve(A,B)
