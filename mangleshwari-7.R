#sol 2
N = 100
u1 = runif(N)
u2 = runif(N)
u3 = runif(N)
U1 = -log(u1, base = exp(1))
U2 = -log(u2, base = exp(1))/2
U3 = -log(u3, base = exp(1))/3
x = pmin(U1, U3)
y = pmin(U2, U3)
plot(x, y)

#sol 3
EX = mean(x)
EY = mean(y)
VarX = var(x)
VarY = var(y)
Emin = mean(pmin(x,y))
corr = cor(x,y)
E1 = mean(x[x<y])
E2 = mean(x[x>y])
E3 = mean(x[x == y])

#sol 4
reps = 1000
EX = EY = Emin = corr = E1 = E2 = E3 = numeric(reps)
for (i in 1:reps)
{
  N = 100
  u1 = runif(N)
  u2 = runif(N)
  u3 = runif(N)
  U1 = -log(u1, base = exp(1))
  U2 = -log(u1, base = exp(1))/2
  U3 = -log(u1, base = exp(1))/3
  x = pmin(U1, U3)
  y = pmin(U2, U3)
  EX[i] = mean(x)
  EY[i] = mean(y)
  Emin[i] = mean(pmin(x,y))
  corr[i] = cor(x,y)
  E1[i] = mean(x[x<y])
  E2[i] = mean(x[x>y])
  E3[i] = mean(x[x == y])
}
mean(EX)
mean(EY)
mean(Emin)
mean(corr)
mean(E1)
mean(E2)
mean(E3)

#sol = 5
lambda = 0.5
pois_gen = function(r)
{
  u = exp(-r)
  k = 0
  p = 1
  
  while (p > u)
  {
    k = k + 1
    v = runif(u)
    p = k * v
  }
}
N1 = pois_gen(0.5)
N2 = pois_gen(0.5*2)
N3 = pois_gen(0.5*3)
N4 = pois_gen(0.5*4)
N5 = pois_gen(0.5*5)
