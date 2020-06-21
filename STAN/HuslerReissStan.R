set.seed(1234)
library(rstan)
# -------- These functions are taken from the package 'copula'
# -------- cant load package for some reason
# just need these to generate random samples from a hustler reiss copula for testing
# cdf (copula) of Hüsler-Reiss bivariate distribution
pHuslerReiss <- function(u, v, lambda){
  arg1 = pnorm( (1/lambda) + (lambda/2) * log(log(u)/log(v)))
  arg2 = pnorm( (1/lambda) + (lambda/2) * log(log(v)/log(u)))
  
  exp(arg1*log(u) + arg2*log(v))
}

# pdf (copula) of Hüsler-Reiss bivariate distribution
dHuslerReiss <- function(u, v, lambda, log = FALSE){
  arg1 = pnorm( (1/lambda) + (lambda/2) * log(log(v)/log(u)))
  arg2 = pnorm( (1/lambda) + (lambda/2) * log(log(u)/log(v)))
  arg3 = dnorm( (1/lambda) + (lambda/2) * log(log(u)/log(v)))
  
  if(log)
    log((1/(u*v)) * (arg1* arg2 - (lambda/(2*log(v)))*arg3) * pHuslerReiss(u, v, lambda))
  else
    (1/(u*v)) * (arg1* arg2 - (lambda/(2*log(v)))*arg3) * pHuslerReiss(u, v, lambda)
}


#---- random sample form HR ----#
rhuslerReissCopula <- function(n, dep) {
  u1 <- runif(n)
  v <- runif(n)
  alpha <- dep
  eps <- .Machine$double.eps ^ 0.8  ## don't know a better way
  myfun <- function(u2, u1, v) {
    pHuslerReiss(u1, u2, dep) / u1 *
      pnorm(1/alpha + 0.5 * alpha * log(log(u1) / log(u2))) - v
  }
  cbind(u1, if(n >= 1) vapply(1:n, function(i) uniroot(myfun, c(eps, 1 - eps),
                                                       v=v[i], u1=u1[i])$root, double(1))
        else v,
        deparse.level=0L)
}



# ---- generate random samples from hustler reiss copula
N = 1000 # number of samples
lambda = 6 # dependence parameter to estimate (0 < lambda)
samples = rhuslerReissCopula(N,lambda)

plot(samples[,1], samples[,2])

my_data = list(N=N, u = samples[,1], v = samples[,2])
fit = stan(file = 'StanCode/HuslerReiss.stan', data = my_data)

posterior <- extract(fit)
lambda_est = mean(posterior$lambda)

#plot samples from hustler resis distribution with estimated paramer 
#over samples from hustler resis distribution with true parameter
estimated_samples = rhuslerReissCopula(N,lambda_est)
plot(samples[,1], samples[,2])
points(estimated_samples[,1], estimated_samples[,2],col="green")

# looks good :)