
# This script generates random data from a gaussian distribution 
# and then estimates the parameters of this distribution using JAGS 
set.seed(1234)

# --- libraries 
library(rstan)
library(ggplot2)
library(gridExtra)

# ------ model specifications
# - - - - - - - - - - - - - - 
# --- model
# y ~ alpha + beta*x + noise
# noise ~ gaussian(0, stdev)
# therefore, y ~ gaussian(alpha + beta*x, stdev)

# --- likelihood
# y_1, ..., y_N | alpha_est, beta_est , stdev_est ~ Gaussian(mean = alpha_est + beta_est*x, standard deviation = stdev_est)

# --- priors
# alpha_est ~ Gaussian(mean = 0, standard deviation = 100)
# beta_est ~ Gaussian(mean = 0, standard deviation = 100)
# stdev_est ~ Gamma(shape = 1, scale = 1)


# --- generate data from normal distribution
alpha_0 = 1
beta_0 = 5
stdev_0 = 2
N=100
x = seq(1, 10, length.out = N)
y = rnorm(N, mean = alpha_0 + beta_0*x, sd = stdev_0)

# visualise data
plot(x,y)


my_data = list(y = y, x = x, N = N)

fit = stan(file = 'LinReg.stan', data = my_data)

print(fit)


posterior <- extract(fit)
alpha_est = posterior$alpha_est
beta_est = posterior$beta_est
stdev_est = posterior$stdev_est



# Find SSE and MSE
yfit = mean(alpha_est) + x*mean(beta_est)
n = length(y)
sse <- sum((y - yfit)^2)
mse <- sse / (n - 2)
se <- sqrt(sum((y - yfit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))

slope.upper <- yfit + 1.96 * se
slope.lower <- yfit - 1.96 * se

results = data.frame(x,
                     y,
                     yfit,
                     slope.upper,
                     slope.lower)
names(results) = c('x', 'y', 'model', 'upper', 'lower')
ggplot()+
  geom_point(data = results, aes(x = x, y=y), colour = 'lightgreen', size = 1)+
  geom_line(data = results, aes(x = x, y=model), colour = 'darkgreen', size = 1)+
  geom_line(data = results, aes(x = x, y=upper), colour = 'red', size = 0.5)+
  geom_line(data = results, aes(x = x, y=lower), colour = 'red', size = 0.5)+
  theme_minimal()

