#sol 2

set.seed(123)

# Generate exponential using runif
rexp_custom <- function(n, lambda) {
  u <- runif(n)
  return(-log(1 - u) / lambda)
}

# MLE over discrete parameter space
mle_exp_discrete <- function(x, omega) {
  n <- length(x)
  s <- sum(x)
  
  loglik <- sapply(omega, function(lam) {
    n * log(lam) - lam * s
  })
  
  return(omega[which.max(loglik)])
}

simulate_exp <- function(N, lambda_true, omega, reps=1000) {
  est <- numeric(reps)
  
  for(i in 1:reps) {
    x <- rexp_custom(N, lambda_true)
    est[i] <- mle_exp_discrete(x, omega)
  }
  
  list(
    mean_est = mean(est),
    mse = mean((est - lambda_true)^2)
  )
}

omega <- c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5)

res_50  <- simulate_exp(50, 1.5, omega)
res_100 <- simulate_exp(100, 1.5, omega)
res_200 <- simulate_exp(200, 1.5, omega)
res_500 <- simulate_exp(500, 1.5, omega)

res_50; res_100; res_200; res_500

# sol 3

# Normal using Box-Muller
rnorm_custom <- function(n, mu, sigma) {
  u1 <- runif(n)
  u2 <- runif(n)
  
  z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  return(mu + sigma * z)
}

mle_mu_discrete <- function(x) {
  return(round(mean(x)))  # nearest integer
}

simulate_normal <- function(N, mu_true, sigma, reps=1000) {
  est <- numeric(reps)
  
  for(i in 1:reps) {
    x <- rnorm_custom(N, mu_true, sqrt(sigma))
    est[i] <- mle_mu_discrete(x)
  }
  
  list(
    mean_est = mean(est),
    mse = mean((est - mu_true)^2)
  )
}

res_25  <- simulate_normal(25, 1, 10)
res_50  <- simulate_normal(50, 1, 10)
res_75  <- simulate_normal(75, 1, 10)
res_100 <- simulate_normal(100, 1, 10)

res_25; res_50; res_75; res_100

#sol 4

# Generate from given PDF
r_custom_alpha <- function(n, alpha) {
  u <- runif(n)
  y <- -log(1 - u)   # Exp(1)
  x <- (y / 2)^(1 / alpha)
  return(x)
}

# Newton-Raphson
mle_alpha_NR <- function(x, tol=0.001, max_iter=100) {
  alpha <- 1.5
  n <- length(x)
  
  for(i in 1:max_iter) {
    score <- n/alpha + sum(log(x)) - 2 * sum(x^alpha * log(x))
    
    info <- -n/alpha^2 - 2 * sum(x^alpha * (log(x))^2)
    
    new_alpha <- alpha - score / info
    
    if(abs(new_alpha - alpha) < tol) {
      return(list(alpha=new_alpha, iter=i))
    }
    
    alpha <- new_alpha
  }
  
  return(list(alpha=alpha, iter=max_iter))
}

simulate_alpha <- function(N, alpha_true=2, reps=1000) {
  est <- numeric(reps)
  iter <- numeric(reps)
  
  for(i in 1:reps) {
    x <- r_custom_alpha(N, alpha_true)
    res <- mle_alpha_NR(x)
    est[i] <- res$alpha
    iter[i] <- res$iter
  }
  
  list(
    mean_est = mean(est),
    mse = mean((est - alpha_true)^2),
    avg_iter = mean(iter),
    est_values = est
  )
}

res_25  <- simulate_alpha(25)
res_50  <- simulate_alpha(50)
res_75  <- simulate_alpha(75)
res_100 <- simulate_alpha(100)

# Histogram
hist(res_25$est_values, main="Histogram of alpha_hat (N=25)")
hist(log(res_25$est_values), main="Histogram of log(alpha_hat)")