#sol_2
set.seed(123)
u = runif(100)
v = runif(100)
z1 = sqrt(-2*log(u, base = exp(1)))*cos(2*pi*v) 
z2 = sqrt(-2*log(u, base = exp(1)))*sin(2*pi*v) 
par(mfrow = c(2,2))
hist(z1, probability = TRUE)
plot(ecdf(z1))
hist(z2, probability = TRUE)
plot(ecdf(z2))
par(mfrow = c(1,1))

#sol_3
set.seed(123)
#α = 1.5 and λ = 2.5
lambda = 2.5
alpha = 1.5
u = runif(100)
v = runif(100)
x = -log(u, base = exp(1))/lambda
g = x^(alpha-1)*exp(-lambda*x)
c = max(g)
gamma = x[v<= g/c][1:100]
par(mfrow = c(1,2))
hist(gamma)
plot(ecdf(gamma))
par(mfrow = c(1,1))

#α = 0.75 and λ = 1.5
lambda = 1.5
alpha = 0.75
u = runif(100)
v = runif(100)
x = -log(u, base = exp(1))/lambda
g = x^(alpha-1)*exp(-lambda*x)
c = max(g)
gamma = x[v<= g/c][1:100]
par(mfrow = c(1,2))
hist(gamma)
plot(ecdf(gamma))
par(mfrow = c(1,1))


#sol_4
z = sqrt(-2*log(u, base = exp(1)))*cos(2*pi*v)
modz = abs(z)
par(mfrow = c(1,2))
hist(modz)
plot(ecdf(modz))
par(mfrow = c(1,1))

#sol_5
mean(modz)
