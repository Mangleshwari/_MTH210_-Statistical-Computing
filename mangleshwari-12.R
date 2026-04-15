############################################################################
#sol2
############################################################################
set.seed(123)

# Generate exponential using runif()
rexp_custom <- function(n, lambda) {
  u <- runif(n)
  return(-log(1 - u) / lambda)
}

# Conditional expectation E[X | X <= T]
cond_exp_leq_T <- function(lambda, T) {
  return(1/lambda - (T * exp(-lambda * T)) / (1 - exp(-lambda * T)))
}

# Direct MLE
mle_direct <- function(x_obs, n_leq, T, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n_leq
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    ex <- cond_exp_leq_T(lambda, T)
    lambda_new <- n / (sum(x_obs) + n_leq * ex)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# EM algorithm
mle_em <- function(x_obs, n_leq, T, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n_leq
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    ex <- cond_exp_leq_T(lambda, T)
    lambda_new <- n / (sum(x_obs) + n_leq * ex)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# One simulation
one_run <- function(N, lambda_true, T) {
  x <- rexp_custom(N, lambda_true)
  
  x_obs <- x[x > T]
  n_leq <- sum(x <= T)
  
  lambda_d <- mle_direct(x_obs, n_leq, T)
  lambda_e <- mle_em(x_obs, n_leq, T)
  
  return(c(lambda_d, lambda_e))
}

# Simulation
simulate <- function(N, lambda_true, T, reps = 1000) {
  results <- matrix(0, nrow = reps, ncol = 2)
  
  for (i in 1:reps) {
    results[i, ] <- one_run(N, lambda_true, T)
  }
  
  avg <- colMeans(results)
  mse <- colMeans((results - lambda_true)^2)
  
  return(c(avg, mse))
}

# Parameters
N_vals <- c(25, 30, 40, 50)
T_vals <- c(1.0, 1.5)
lambda_vals <- c(0.1, 0.2, 0.3)



data1 <- data.frame()

for (lambda_true in lambda_vals) {
  for (T in T_vals) {
    for (N in N_vals) {
      
      res <- simulate(N, lambda_true, T)
      
      temp <- data.frame(
        N = N,
        lambda_true = lambda_true,
        T = T,
        avg_direct = res[1],
        avg_em = res[2],
        mse_direct = res[3],
        mse_em = res[4]
      )
      
      data1 <- rbind(final_df, temp)
    }
  }
}

# View result
View(data1)


############################################################################
#sol3
############################################################################

set.seed(123)

# Generate Rayleigh using inverse transform
r_custom <- function(n, lambda) {
  u <- runif(n)
  return(sqrt(-log(1 - u) / lambda))
}

# E[X^2 | X <= T]
cond_exp_sq <- function(lambda, T) {
  return(1/lambda - (T^2 * exp(-lambda * T^2)) / (1 - exp(-lambda * T^2)))
}

# Direct MLE (fixed-point iteration)
mle_direct <- function(x_obs, n_leq, T, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n_leq
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    ex2 <- cond_exp_sq(lambda, T)
    lambda_new <- n / (sum(x_obs^2) + n_leq * ex2)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# EM algorithm
mle_em <- function(x_obs, n_leq, T, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n_leq
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    # E-step
    ex2 <- cond_exp_sq(lambda, T)
    
    # M-step
    lambda_new <- n / (sum(x_obs^2) + n_leq * ex2)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# One simulation
one_run <- function(N, lambda_true, T) {
  x <- r_custom(N, lambda_true)
  
  x_obs <- x[x > T]
  n_leq <- sum(x <= T)
  
  lambda_d <- mle_direct(x_obs, n_leq, T)
  lambda_e <- mle_em(x_obs, n_leq, T)
  
  return(c(lambda_d, lambda_e))
}

# Simulation
simulate <- function(N, lambda_true, T, reps = 1000) {
  results <- matrix(0, nrow = reps, ncol = 2)
  
  for (i in 1:reps) {
    results[i, ] <- one_run(N, lambda_true, T)
  }
  
  avg <- colMeans(results)
  mse <- colMeans((results - lambda_true)^2)
  
  return(c(avg, mse))
}

# Parameters
N_vals <- c(25, 30, 40, 50)
T_vals <- c(1.0, 1.5)
lambda_vals <- c(0.1, 0.2, 0.3)


data2 <- data.frame()

for (lambda_true in lambda_vals) {
  for (T in T_vals) {
    for (N in N_vals) {
      
      res <- simulate(N, lambda_true, T)
      
      temp <- data.frame(
        N = N,
        lambda_true = lambda_true,
        T = T,
        avg_direct = res[1],
        avg_em = res[2],
        mse_direct = res[3],
        mse_em = res[4]
      )
      
      data2 <- rbind(final_df, temp)
    }
  }
}

View(data2)

############################################################################
#sol4
############################################################################

set.seed(123)

# Generate exponential using inverse transform
rexp_custom <- function(n, lambda) {
  u <- runif(n)
  return(-log(1 - u) / lambda)
}

# E[X | X <= T1]
cond_exp_leq_T1 <- function(lambda, T1) {
  return(1/lambda - (T1 * exp(-lambda * T1)) / (1 - exp(-lambda * T1)))
}

# E[X | X > T2]
cond_exp_gt_T2 <- function(lambda, T2) {
  return(T2 + 1/lambda)
}

# Direct MLE (fixed-point)
mle_direct <- function(x_obs, n1, n2, T1, T2, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n1 + n2
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    e1 <- cond_exp_leq_T1(lambda, T1)
    e2 <- cond_exp_gt_T2(lambda, T2)
    
    lambda_new <- n / (sum(x_obs) + n1 * e1 + n2 * e2)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# EM algorithm
mle_em <- function(x_obs, n1, n2, T1, T2, tol = 1e-6, max_iter = 1000) {
  n <- length(x_obs) + n1 + n2
  lambda <- 0.1
  
  for (i in 1:max_iter) {
    # E-step
    e1 <- cond_exp_leq_T1(lambda, T1)
    e2 <- cond_exp_gt_T2(lambda, T2)
    
    # M-step
    lambda_new <- n / (sum(x_obs) + n1 * e1 + n2 * e2)
    
    if (abs(lambda_new - lambda) < tol) break
    lambda <- lambda_new
  }
  
  return(lambda)
}

# One simulation
one_run <- function(N, lambda_true, T1, T2) {
  x <- rexp_custom(N, lambda_true)
  
  x_obs <- x[x > T1 & x < T2]
  n1 <- sum(x <= T1)
  n2 <- sum(x >= T2)
  
  lambda_d <- mle_direct(x_obs, n1, n2, T1, T2)
  lambda_e <- mle_em(x_obs, n1, n2, T1, T2)
  
  return(c(lambda_d, lambda_e))
}

# Simulation
simulate <- function(N, lambda_true, T1, T2, reps = 1000) {
  results <- matrix(0, nrow = reps, ncol = 2)
  
  for (i in 1:reps) {
    results[i, ] <- one_run(N, lambda_true, T1, T2)
  }
  
  avg <- colMeans(results)
  mse <- colMeans((results - lambda_true)^2)
  
  return(c(avg, mse))
}

# Parameters
N_vals <- c(25, 30, 40, 50)
T_pairs <- list(c(1.0, 2.0), c(1.0, 3.0), c(1.5, 3.5))
lambda_vals <- c(0.1, 0.2, 0.3)

# Data frame
data3 <- data.frame()

# Run all combinations
for (lambda_true in lambda_vals) {
  for (tp in T_pairs) {
    T1 <- tp[1]
    T2 <- tp[2]
    
    for (N in N_vals) {
      
      res <- simulate(N, lambda_true, T1, T2)
      
      temp <- data.frame(
        N = N,
        lambda_true = lambda_true,
        T1 = T1,
        T2 = T2,
        avg_direct = res[1],
        avg_em = res[2],
        mse_direct = res[3],
        mse_em = res[4]
      )
      
      data3 <- rbind(data3, temp)
    }
  }
}

View(data3)
