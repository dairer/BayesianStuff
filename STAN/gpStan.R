# fitting a generalised pareto distribution with Stan

# not working yet :(
library(rstan)
library(extRemes) # used for generating random samples for a GEV

# generare some data from a GP distribution
set.seed(1234)
N = 100 # number of samples
y = revd(n = N, threshold = 2, scale = 1, shape = 0, type = "GP") # samples
threshold = 2
# prep data
my_data = list(y = y, N = N, threshold = threshold)

# run model
fit = stan(file = 'gpStan.stan', data = my_data, warmup = 1000, iter = 4000, thin = 20)

# diagnostic plots
plot(fit)
traceplot(fit)

# get estimates 
posterior <- extract(fit)
location_est = mean(posterior$location)
shape_est = mean(posterior$shape)
scale_est = mean(posterior$scale)
