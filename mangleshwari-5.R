##sol_1
set.seed(123)
n = 1000
d = 4
u = runif(n*d/2)
v = runif(n*d/2)
z1 = sqrt(-2*log(u, base = exp(1)))*cos(2*pi*v) 
z2 = sqrt(-2*log(u, base = exp(1)))*sin(2*pi*v) 
Z = c(z1, z2)
z = matrix(Z, nrow = n, ncol = d)
mu = matrix(c(1,-1,2,-2), nrow = d)
Sigma = matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1), nrow = d, ncol = d)
sv = svd(Sigma)
U = sv$u
D = sqrt(sv$d)
V = sv$v
sigma = U %*% diag(D) %*% t(V)
final_x = z %*% sigma+ matrix(mu, nrow = n, ncol = d, byrow =T)


##sol_2
est_mu = colMeans(final_x)
est_sigma = cov(final_x)


##sol_3
set.seed(123)
n = 1000
d = 8
u = runif(n*d/2)
v = runif(n*d/2)
z1 = sqrt(-2*log(u, base = exp(1)))*cos(2*pi*v) 
z2 = sqrt(-2*log(u, base = exp(1)))*sin(2*pi*v) 
Z = c(z1, z2)
z = matrix(Z, nrow = n, ncol = d)
mu = matrix(c(1,0,1,0,1,0,1,0), nrow = d)
u = 
x = matrix(c(1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5,1), nrow = 4, byrow = T)
y = matrix(0, nrow = 4, ncol = 4, byrow = T)
x1 = cbind(x,y)
x2 = cbind(y,2*x)
Sigma = rbind(x1, x2)
sv = svd(Sigma)
U = sv$u
D = sqrt(sv$d)
V = sv$v
sigma = U %*% diag(D) %*% t(V)
final_x = z %*% sigma+ matrix(mu, nrow = n, ncol = d, byrow =T)


##sol_4
est_mu = colMeans(final_x)
est_sigma = cov(final_x)
