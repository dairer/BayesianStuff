# fit GEV with stan :)
library(rstan)
library(extRemes) # used for generating random samples

# generare some data from a GEV
set.seed(1234)
N = 100
y = revd(n = N, loc = 0, scale = 1, shape = 0, type = "GEV")

my_data = list(y = y, N = N)
fit = stan(file = 'gevStan.stan', data = my_data, warmup = 1000, iter = 4000, thin = 2)


plot(fit)
traceplot(fit)
posterior <- extract(fit)
location_est = mean(posterior$location)
shape_est = mean(posterior$shape)
scale_est = mean(posterior$scale)


# distribution function (pdf)
pGEV = function(data, location, scale, shape){
  if(shape == 0) t = exp(-(data - location)/scale)
  if(shape != 0) t = (1 + shape*((data - location)/scale))^(-1/shape)
  return ((1/scale)*t^(shape+1)*exp(-t))
}