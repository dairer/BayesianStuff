# fitting a generalised extreme value distribution with Stan
library(rstan)
library(extRemes) # used for generating random samples for a GEV

# generare some data from a GEV
set.seed(1234)
N = 100 # number of samples
y = revd(n = N, loc = 2, scale = 1, shape = 0, type = "GEV") # samples

# prep data
my_data = list(y = y, N = N)

# run model
fit = stan(file = 'gevStan.stan', data = my_data, warmup = 1000, iter = 4000, thin = 2)

# diagnostic plots
plot(fit)
traceplot(fit)

# get estimates 
posterior <- extract(fit)
location_est = mean(posterior$location)
shape_est = mean(posterior$shape)
scale_est = mean(posterior$scale)