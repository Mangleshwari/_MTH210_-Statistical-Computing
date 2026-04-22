#sol 2

N = 25
lambda_1 = 0.15
lambda_2 = 0.16
lambda_3 = 0.18

U1 <- -log(runif(N)) / lambda_1
U2 <- -log(runif(N)) / lambda_2
U3 <- -log(runif(N)) / lambda_3
  
X <- pmin(U1, U3)
Y <- pmin(U2, U3)


#------------------------------------------------------
mle_gradient <- function( X, Y, lr = 0.001, tol = 1e-6, max_iter = 5000) {
  
  lambda1 <- 0.1
  lambda2 <- 0.1
  lambda3 <- 0.1
  
  for(iter in 1:max_iter) {
    
    I1 <- X < Y
    I2 <- Y < X
    I3 <- X == Y
    
    n1 <- sum(I1)
    n2 <- sum(I2)
    n3 <- sum(I3)
    
    g1 <- n1 / lambda1 - sum(X)  #gradient 
    g2 <- n2 / lambda2 - sum(Y)
    g3 <- n3 / lambda3 - sum(pmin(X, Y))
    
    lambda1_new <- lambda1 + lr * g1 #by gradient ascent algorith, updated lambda
    lambda2_new <- lambda2 + lr * g2
    lambda3_new <- lambda3 + lr * g3
    
    lambda1_new <- max(lambda1_new, 1e-6) #to keep positive
    lambda2_new <- max(lambda2_new, 1e-6)
    lambda3_new <- max(lambda3_new, 1e-6)
    
    # checking convergence 
    if(max(abs(c(lambda1_new - lambda1,
                 lambda2_new - lambda2,
                 lambda3_new - lambda3))) < tol) {
      break
    }
    
    lambda1 <- lambda1_new
    lambda2 <- lambda2_new
    lambda3 <- lambda3_new
  }
  
  return(c(lambda1, lambda2, lambda3))
}

#-----------------------------------------------

init_lambda <- function(X, Y) {
  theta1 <- 1 / mean(X)
  theta2 <- 1 / mean(Y)
  theta3 <- 1 / mean(pmin(X, Y))
  
  lambda3 <- theta1 + theta2 - theta3
  lambda1 <- theta1 - lambda3
  lambda2 <- theta2 - lambda3
  
  lambda1 <- max(lambda1, 1e-6)
  lambda2 <- max(lambda2, 1e-6)
  lambda3 <- max(lambda3, 1e-6)
  
  return(c(lambda1, lambda2, lambda3))
}

#-------------------------------------------------------------
em_algorithm <- function(X, Y, tol = 1e-6, max_iter = 1000) {
  
  n <- length(X)
  
  # smart initialization
  init <- init_lambda(X, Y)
  lambda1 <- init[1]
  lambda2 <- init[2]
  lambda3 <- init[3]
  
  for(iter in 1:max_iter) {
    
    I1 <- X < Y
    I2 <- X > Y
    I3 <- X == Y
    
    # ---------- E-step ----------
    
    # I1: X < Y
    a <- Y[I1] + (lambda3 / (lambda2 + lambda3)) * (1 / lambda2)   # E(U2)
    b <- Y[I1] + (lambda2 / (lambda2 + lambda3)) * (1 / lambda3)   # E(U3)
    
    # I2: X > Y
    c <- X[I2] + (lambda3 / (lambda1 + lambda3)) * (1 / lambda1)   # E(U1)
    d <- X[I2] + (lambda1 / (lambda1 + lambda3)) * (1 / lambda3)   # E(U3)
    
    # I3: X = Y
    e <- X[I3] + 1 / lambda1   # E(U1)
    f <- X[I3] + 1 / lambda2   # E(U2)
    
    # ---------- M-step ----------
    
    lambda1_new <- n / (
      sum(X[I1]) +   # U1 in I1
        sum(c) +       # U1 in I2
        sum(e)         # U1 in I3
    )
    
    lambda2_new <- n / (
      sum(a) +       # U2 in I1
        sum(Y[I2]) +   # U2 in I2
        sum(f)         # U2 in I3
    )
    
    lambda3_new <- n / (
      sum(b) +       # U3 in I1
        sum(d) +       # U3 in I2
        sum(X[I3])     # U3 in I3
    )
    
    # convergence check
    if(max(abs(c(lambda1_new - lambda1,
                 lambda2_new - lambda2,
                 lambda3_new - lambda3))) < tol) {
      break
    }
    
    lambda1 <- lambda1_new
    lambda2 <- lambda2_new
    lambda3 <- lambda3_new
  }
  
  return(c(lambda1, lambda2, lambda3))
}
#------------------------------------------------------------

set.seed(123)

reps <- 1000

est_grad <- matrix(0, reps, 3)
est_em   <- matrix(0, reps, 3)

for(r in 1:reps) {
  
  # ---- Generate fresh sample ----
  U1 <- -log(runif(N)) / lambda_1
  U2 <- -log(runif(N)) / lambda_2
  U3 <- -log(runif(N)) / lambda_3
  
  X <- pmin(U1, U3)
  Y <- pmin(U2, U3)
  
  # ---- Store estimates ----
  est_grad[r, ] <- mle_gradient(X, Y)
  est_em[r, ]   <- em_algorithm(X, Y)
}

#---------------------------------------------------------

colMeans(est_grad)
colMeans(est_em)
colMeans((est_grad - true)^2)
colMeans((est_em - true)^2)

lambda_1 = 0.15
lambda_2 = 0.16
lambda_3 = 0.18

U1 <- -log(runif(N)) / lambda_1
U2 <- -log(runif(N)) / lambda_2
U3 <- -log(runif(N)) / lambda_3

X <- pmin(U1, U3)
Y <- pmin(U2, U3)


#--------------------------------------------------------------

N <- 50

est_grad <- matrix(0, reps, 3)
est_em   <- matrix(0, reps, 3)

for(r in 1:reps) {
  
  U1 <- -log(runif(N)) / lambda_1
  U2 <- -log(runif(N)) / lambda_2
  U3 <- -log(runif(N)) / lambda_3
  
  X <- pmin(U1, U3)
  Y <- pmin(U2, U3)
  
  est_grad[r, ] <- mle_gradient(X, Y)
  est_em[r, ]   <- em_algorithm(X, Y)
}

avg_grad_50 <- colMeans(est_grad)
avg_em_50   <- colMeans(est_em)

mse_grad_50 <- colMeans((est_grad - true)^2)
mse_em_50   <- colMeans((est_em - true)^2)
