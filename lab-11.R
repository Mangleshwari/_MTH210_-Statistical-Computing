#####sol 2

set.seed(123)

# --------- Sample generator ----------
r_custom2 <- function(n, mu) {
  y <- rweibull(n, shape = 3, scale = (1/2)^(1/3))
  mu + y
}

# --------- Log-likelihood ----------
loglik2 <- function(mu, x) {
  if (mu >= min(x)) return(-Inf)
  sum(log(6) + 2*log(x - mu) - 2*(x - mu)^3)
}

# --------- MLE ----------
mle_mu2 <- function(x) {
  optimize(function(mu) -loglik2(mu, x),
           interval = c(min(x) - 1, min(x) - 1e-6))$minimum
}

# --------- Simulation ----------
run_sim2 <- function(N, mu_true = 1, reps = 1000) {
  
  mu_hat <- numeric(reps)
  mu_tilde <- numeric(reps)
  
  for (i in 1:reps) {
    x <- r_custom2(N, mu_true)
    
    mu_hat[i] <- mle_mu2(x)
    mu_tilde[i] <- min(x)
  }
  
  # Histograms
  par(mfrow = c(1,2))
  hist(mu_hat, main = paste("MLE mu_hat (N =", N, ")"),
       col = "lightblue", xlab = "mu_hat")
  hist(mu_tilde, main = paste("Min estimator (N =", N, ")"),
       col = "lightgreen", xlab = "mu_tilde")
  
  list(
    avg_hat = mean(mu_hat),
    avg_tilde = mean(mu_tilde),
    mse_hat = mean((mu_hat - mu_true)^2),
    mse_tilde = mean((mu_tilde - mu_true)^2)
  )
}

res2_25  <- run_sim2(25)
res2_50  <- run_sim2(50)
res2_75  <- run_sim2(75)
res2_100 <- run_sim2(100)

######### sol 3

set.seed(123)

# --------- Sample generator ----------
r_custom3 <- function(n, mu) {
  y <- rgamma(n, shape = 3, rate = 2)
  mu + y
}

# --------- Log-likelihood ----------
loglik3 <- function(mu, x) {
  if (mu >= min(x)) return(-Inf)
  sum(log(4) + 2*log(x - mu) - 2*(x - mu))
}

# --------- MLE ----------
mle_mu3 <- function(x) {
  optimize(function(mu) -loglik3(mu, x),
           interval = c(min(x) - 5, min(x) - 1e-6))$minimum
}

# --------- Simulation ----------
run_sim3 <- function(N, mu_true = 2, reps = 1000) {
  
  mu_hat <- numeric(reps)
  mu_tilde <- numeric(reps)
  
  for (i in 1:reps) {
    x <- r_custom3(N, mu_true)
    
    mu_hat[i] <- mle_mu3(x)
    mu_tilde[i] <- min(x)
  }
  
  # Histograms
  par(mfrow = c(1,2))
  hist(mu_hat, main = paste("MLE mu_hat (N =", N, ")"),
       col = "lightblue", xlab = "mu_hat")
  hist(mu_tilde, main = paste("Min estimator (N =", N, ")"),
       col = "lightgreen", xlab = "mu_tilde")
  
  list(
    avg_hat = mean(mu_hat),
    avg_tilde = mean(mu_tilde),
    mse_hat = mean((mu_hat - mu_true)^2),
    mse_tilde = mean((mu_tilde - mu_true)^2)
  )
}

res3_25  <- run_sim3(25)
res3_50  <- run_sim3(50)
res3_75  <- run_sim3(75)
res3_100 <- run_sim3(100)

####sol 4

set.seed(123)

# --------- Sample generator ----------
r_custom4 <- function(n, alpha, lambda) {
  rweibull(n, shape = alpha, scale = (1/lambda)^(1/alpha))
}

# --------- Newton-Raphson ----------
nr_alpha <- function(x, alpha_init = 1.8, tol = 1e-6, max_iter = 100) {
  
  alpha <- alpha_init
  
  for (i in 1:max_iter) {
    
    A <- sum(x^alpha)
    B <- sum(x^alpha * log(x))
    C <- sum(x^alpha * (log(x))^2)
    
    g  <- 1/alpha + mean(log(x)) - B/A
    g_prime <- -1/alpha^2 - (C*A - B^2)/(A^2)
    
    alpha_new <- alpha - g/g_prime
    
    if (abs(alpha_new - alpha) < tol) break
    alpha <- alpha_new
  }
  
  return(alpha)
}

# --------- MLE ----------
mle_weibull_nr <- function(x) {
  alpha_hat <- nr_alpha(x)
  lambda_hat <- length(x) / sum(x^alpha_hat)
  list(alpha = alpha_hat, lambda = lambda_hat)
}

# --------- Simulation ----------
run_sim4 <- function(N, alpha_true = 2, lambda_true = 1, reps = 1000) {
  
  alpha_hat <- numeric(reps)
  lambda_hat <- numeric(reps)
  
  for (i in 1:reps) {
    x <- r_custom4(N, alpha_true, lambda_true)
    
    est <- mle_weibull_nr(x)
    alpha_hat[i] <- est$alpha
    lambda_hat[i] <- est$lambda
  }
  
  # Histograms
  par(mfrow = c(1,2))
  hist(alpha_hat, main = paste("alpha_hat (N =", N, ")"),
       col = "lightblue", xlab = "alpha")
  hist(lambda_hat, main = paste("lambda_hat (N =", N, ")"),
       col = "lightgreen", xlab = "lambda")
  
  list(
    avg_alpha = mean(alpha_hat),
    avg_lambda = mean(lambda_hat),
    mse_alpha = mean((alpha_hat - alpha_true)^2),
    mse_lambda = mean((lambda_hat - lambda_true)^2)
  )
}

res4_25  <- run_sim4(25)
res4_50  <- run_sim4(50)
res4_75  <- run_sim4(75)
res4_100 <- run_sim4(100)

###sol 5

set.seed(123)

# --------- Function g(alpha) ----------
g_alpha <- function(alpha, x) {
  A <- sum(x^alpha)
  B <- sum(x^alpha * log(x))
  1/alpha + mean(log(x)) - B/A
}

# --------- Bisection ----------
bisection_alpha <- function(x, lower = 0.5, upper = 5, tol = 1e-6, max_iter = 100) {
  
  g_low <- g_alpha(lower, x)
  g_up  <- g_alpha(upper, x)
  
  # Adjust if needed
  if (g_low * g_up > 0) {
    lower <- 0.1
    upper <- 10
  }
  
  for (i in 1:max_iter) {
    mid <- (lower + upper) / 2
    g_mid <- g_alpha(mid, x)
    
    if (abs(g_mid) < tol) break
    
    if (g_low * g_mid < 0) {
      upper <- mid
    } else {
      lower <- mid
      g_low <- g_mid
    }
  }
  
  return((lower + upper)/2)
}

# --------- MLE ----------
mle_weibull_bisect <- function(x) {
  alpha_hat <- bisection_alpha(x)
  lambda_hat <- length(x) / sum(x^alpha_hat)
  list(alpha = alpha_hat, lambda = lambda_hat)
}

# --------- Simulation ----------
run_sim5 <- function(N, alpha_true = 2, lambda_true = 1, reps = 1000) {
  
  alpha_hat <- numeric(reps)
  lambda_hat <- numeric(reps)
  
  for (i in 1:reps) {
    x <- rweibull(N, shape = alpha_true, scale = (1/lambda_true)^(1/alpha_true))
    
    est <- mle_weibull_bisect(x)
    alpha_hat[i] <- est$alpha
    lambda_hat[i] <- est$lambda
  }
  
  # Histograms
  par(mfrow = c(1,2))
  hist(alpha_hat, main = paste("alpha_hat (N =", N, ")"),
       col = "lightblue", xlab = "alpha")
  hist(lambda_hat, main = paste("lambda_hat (N =", N, ")"),
       col = "lightgreen", xlab = "lambda")
  
  list(
    avg_alpha = mean(alpha_hat),
    avg_lambda = mean(lambda_hat),
    mse_alpha = mean((alpha_hat - alpha_true)^2),
    mse_lambda = mean((lambda_hat - lambda_true)^2)
  )
}

res5_25  <- run_sim5(25)
res5_50  <- run_sim5(50)
res5_75  <- run_sim5(75)
res5_100 <- run_sim5(100)