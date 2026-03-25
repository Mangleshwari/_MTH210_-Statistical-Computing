#sol 2

#method 1
set.seed(42)

N <- 10000

x <- runif(N)
y <- runif(N)

inside <- (x^2 + y^2) <= 1

pi_est1 <- 4 * mean(inside)

pi_est1

#method 2
set.seed(42)

N <- 10000

x <- runif(N)

pi_est2 <- 4 * mean(sqrt(1 - x^2))

pi_est2

#comparision between these two methods
pi_est1
pi_est2
pi  # true value

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#sol 3

#method 1
set.seed(42)

N <- 10000
a <- 11.275
k <- 11
delta <- a - k   # 0.275

# Generate Gamma(k,1) using uniforms
gamma_sample <- function(n, k) {
  X <- numeric(n)
  for (i in 1:n) {
    U <- runif(k)
    X[i] <- -sum(log(U))
  }
  return(X)
}

x <- gamma_sample(N, k)

# Monte Carlo estimate
gamma_est1 <- gamma(k) * mean(x^delta)

gamma_est1

#method 2
set.seed(42)

N <- 10000
a <- 11.275

u <- runif(N)

x <- u / (1 - u)

g <- (x^(a - 1)) * exp(-x) / (1 - u)^2

gamma_est2 <- mean(g)

gamma_est2

#comparision between method 1 and 2
gamma(11.275)
gamma_est1
gamma_est2
#-------------------------------------------------------------------------------
# sol 4

set.seed(42)

lambda_true <- 1.5
replications <- 1000

simulate_mle <- function(N) {
  lambda_hat <- numeric(replications)
  
  for (i in 1:replications) {
    # Generate sample using runif
    U <- runif(N)
    X <- -log(U) / lambda_true
    
    # MLE
    lambda_hat[i] <- 1 / mean(X)
  }
  
  # Average estimate
  avg_lambda <- mean(lambda_hat)
  
  # Mean Squared Error
  mse <- mean((lambda_hat - lambda_true)^2)
  
  return(list(avg = avg_lambda, mse = mse))
}

# Run for different sample sizes
res_50  <- simulate_mle(50)
res_100 <- simulate_mle(100)
res_200 <- simulate_mle(200)
res_500 <- simulate_mle(500)

res_50
res_100
res_200
res_500

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#sol 5

set.seed(42)

mu_true <- 1
sigma_true <- 2   # since variance = 4
replications <- 1000

# Function to generate N(0,1) using Box-Muller
rnorm_runif <- function(n) {
  U1 <- runif(n)
  U2 <- runif(n)
  
  Z <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
  return(Z)
}

simulate_normal <- function(N) {
  mu_hat <- numeric(replications)
  sigma2_hat <- numeric(replications)
  
  for (i in 1:replications) {
    # Generate normal sample
    Z <- rnorm_runif(N)
    X <- mu_true + sigma_true * Z
    
    # MLEs
    mu_hat[i] <- mean(X)
    sigma2_hat[i] <- mean((X - mean(X))^2)
  }
  
  # Averages
  avg_mu <- mean(mu_hat)
  avg_sigma2 <- mean(sigma2_hat)
  
  # MSEs
  mse_mu <- mean((mu_hat - mu_true)^2)
  mse_sigma2 <- mean((sigma2_hat - 4)^2)
  
  return(list(
    avg_mu = avg_mu,
    mse_mu = mse_mu,
    avg_sigma2 = avg_sigma2,
    mse_sigma2 = mse_sigma2
  ))
}

# Run for different sample sizes
res_50  <- simulate_normal(50)
res_100 <- simulate_normal(100)
res_200 <- simulate_normal(200)
res_500 <- simulate_normal(500)

res_50
res_100
res_200
res_500