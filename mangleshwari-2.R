#Complete the Table in Lab-1.
set.seed(123)
my_table = function(N,K)
{
  results = data.frame(N = integer(), K = integer(), chi_sq = numeric())
  for (i in N)
  {
    for (j in K)
    {
      O = i/j
      k = seq(0,1, length.out = j+1)
      E = hist(runif(i), plot = F, breaks = k)$counts
      chi_sq = sum(((O - E)^2)/E)
      results = rbind(results, data.frame(N = i, K = j, chi_sq = chi_sq))
    }
  }
  return(results)
}
N = list(50,100,200,1000)
K = list(5,10,15)
my_table(N,K)

#Generate a sample of size 50, from a Binomial distribution with n = 5 and p = 2/3.
#Perform a chi-square test and compute the corresponding p-values.
n = 5
p = 2/3
sample_size = 50
u = matrix(runif(sample_size*n), nrow = sample_size)
binom_sample = rowSums(u<=p)
x = ifelse(u <= p, 1, 0)
x = rbinom(50,5,2/3)
O = tabulate(x+1, nbins = 6)
E = dbinom(0:5, 5, 2/3)*50
sum(((E-O)^2)/E)
1-pchisq(chi_sq, 5)

#Generate a sample of size 100 from a Poisson distribution with mean 2.5. Find the
#mean and variance of this sample. Perform a chi-square test with number of intervals
#5 and 10 and compute the corresponding p-values
x = rpois(100,2.5)
O = table(x)
k = 0:5
exp = dpois(5,2.5)
E = exp*100
chi_sq = sum(((E-O)^2)/E)
p_value = 1-pchisq(chi_sq, 5)

#Generate a sample of size 50 from X which has the following probability mass function
#P (X = k) = (ln 5)^k/4*k! ; k = 1, 2, . . . .
#Find the mean and variance of this sample. Perform a chi-square test with number of
#intervals 5 and 10 and compute the corresponding p-values.
fun = function(k)
{
  return(((logb(5))^k)/(4*factorial(k)))
}
k1 = fun(1:5)
k2 = fun(1:15)
mean(k1)
mean(k2)
var(k1)
var(k2)
chisq.test(table(cut(k1, breaks = 5)))
chisq.test(table(cut(k1, breaks = 15)))

#Generate a sample of size 100 from X which has the following probability mass function
#P (X = k) = exp(-1)*( (1/2)^k + 1/factorial(k))  ; k = 1, 2, . . . .
#Find the mean and variance of this sample. Perform a chi-square test with number of
#intervals 5 and 10 and compute the corresponding p-values.
fun = function(k)
{
  return(exp(-1)*( (1/2)^k + 1/factorial(k)))
}
k1 = fun(1:5)
k2 = fun(1:15)
mean(k1)
mean(k2)
var(k1)
var(k2)
chisq.test(table(cut(k1, breaks = 5)))
chisq.test(table(cut(k1, breaks = 15)))

#Let Ω = {i1i2i3i4i5; 1 ≤ i1 6 = i2 6 = i3 6 = i4 6 = i5 ≤ 5}. Here Ω has total 5! elements
#(i.e. all possible permutations of 1,2,3,4,5). Let X be a random variable takes any
#particular value from Ω with probability 1/5!. Draw a random sample of size 10 from X.
x = sample(1:5)
r.s. = replicate(10, x)
p = 1/factorial(5)

#Suppose X and Y are two random random variables with the joint probability mass function
#P (X = k, Y = 1) = 1/(3 * 2^k) ; k = 1, 2, . . . and P (X = k, Y = 2) = 2^k/((3^k)+1) ; k = 1, 2, . . .
#Draw a random sample of size 100 from Z = X + Y . Compute the mean and variance of Z.
fun1 = function(k)
{
  return(1/(3 * 2^k))
}
fun2 = function(k)
{
  return(2^k/((3^k)+1))
}
k = 1:100
X = fun1(k)
Y = fun2(k)
Z = X + Y
mean(Z)
var(Z)


