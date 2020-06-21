# fitting a generalised pareto distribution with Stan

library(rstan)
library(extRemes) # used for generating random samples for a GEV

# generare some data from a GP distribution
set.seed(1234)
N = 50 # number of samples
shape = 0.12
thresh = 0
scale = 2
y = revd(n = N, loc = thresh, scale = scale, shape = shape, type = "GP") # samples

# ----- following must hold for a generalised pareto distribution:
# scale > 0 
# if shape >= 0, data >= location
# if shape < 0, location <= data <= location - scale/shape
# hence: data - location <= - scale/shape
# hence: shape >= -scale/(data-location)

# prep data
my_data = list(y = y, N = N, threshold = thresh)
fit = stan(file = 'StanCode/genPar.stan', data = my_data, thin = 2, iter = 3000)
fit
# diagnostic plots
plot(fit)
traceplot(fit)

# get estimates 
posterior <- extract(fit)
shape_est = mean(posterior$shape)
scale_est = mean(posterior$scale)

print(paste0("estimated shape = ", signif(shape_est,4), " true shape = ", shape))
print(paste0("estimated scale = ", signif(scale_est,4), " true scale = ", scale))
