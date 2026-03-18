##sol_2
set.seed(1)
n = 100
z1 = numeric(n)
a = sqrt(2/exp(1))
i = 1
total_trials = 0
while(i <= n)
{
  u = runif(1)
  v = runif(1, -a, a)
  total_trials = total_trials + 1
  
  if (v^2 <= -2*u^2*log(u, base = exp(1)))
  {
    z1[i] = v/u
    i = i+1
  }
}
acc_prop = n/total_trials
#----------------------------------------------------------------------------------
n = 100
z2 = numeric(n)
i = 1
total_trials = 0
M = (2/sqrt(2*pi)*exp(0.5))
while (i <= n)
{
  u = runif(1)
   if (u < 0.5)
   {
     y = -log(2*u)
   }
  else
  {
    y = -log(2*(1-u))
  }
  v = runif(1)
  f = (1/sqrt(2*pi))*exp(-y^2/2)
  g = 0.5*exp(-abs(y))
  total_trials = total_trials + 1
  if (v <= f/(M*g))
  {
    z2[i] = y
    i = i + 1
  }
}
acc_prop = n/total_trials

hist(z1, probability = TRUE, main = "normal using ratio to unif")
hist(z2, probability = TRUE, main = "normal using acceptance rejection method")
#acceptance of ratio to unif is less than AR
################################################################################
##sol_3
