##sol_2
set.seed(123)
N = 10^7
x = runif(N,-2,2)
fx = sum(exp(x+x^2))/N                                                                                                                                                                                                                                                                                                                                                                                                       
I = 4*fx
I
##sol_3
u = runif(N)
x = tan(pi*(u - 0.5))
I = pi*mean((x/(1+x^2))*(x>0))
I
##sol_4
x = runif(N)
y = runif(N)
I = mean(exp(x+y)^2)
I
##sol_5
N = 10000
x6 = numeric(N)
for (i in 1:N)
{
  u = runif(11)
  x = -log(u, base = exp(1))
  x6[i] = sort(x)[6]
}
mean(x6)
var(x6)
