simulate_weibull <- function(N) {
  U <- runif(N)
  
  # Generate X using inverse transform
  X <- (1/sqrt(3)) * (-log(U))^(1/2)
  
  X5 <- X^5
  
  mean_X5 <- mean(X5)
  var_X5  <- var(X5)
  
  return(c(mean = mean_X5, variance = var_X5))
}

set.seed(123)

simulate_weibull(1000)
simulate_weibull(5000)
simulate_weibull(10000)

#solution 3
simulate_truncated <- function(N) {
  U <- runif(N)
  
  a <- exp(-12)
  b <- exp(-27)
  
  # Inverse transform
  X <- sqrt( -log(a - U * (a - b)) / 3 )
  
  X5 <- X^5
  
  mean_X5 <- mean(X5)
  var_X5  <- var(X5)
  
  return(c(mean = mean_X5, variance = var_X5))
}

set.seed(123)

simulate_truncated(1000)
simulate_truncated(5000)
simulate_truncated(10000)

#solution 4
simulate_normal <- function(N) {
  U1 <- runif(N)
  U2 <- runif(N)
  
  Z <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
  
  log_abs <- log(abs(Z))
  
  return(mean(log_abs))
}

set.seed(123)
simulate_normal(10000)

simulate_exp <- function(N) {
  U <- runif(N)
  
  Y <- -log(U)   # Exp(1)
  
  log_abs <- 0.5 * log(2 * Y)
  
  return(mean(log_abs))
}

set.seed(123)
simulate_exp(10000)
###prefer exponential:
#having low variance since it uses one uniform per sample and avoides oscillations
#uses only one unif than normal
#simple transformation and faster