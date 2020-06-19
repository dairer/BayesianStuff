
# This script generates random data from a gaussian distribution 
# and then estimates the parameters of this distribution using JAGS 
set.seed(1234)

# --- libraries 
library(R2jags)
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
stdev_0 = 4
N=1000
x = seq(1, 10, length.out = N)
y = rnorm(N, mean = alpha_0 + beta_0*x, sd = stdev_0)


# visualise data
plot(x,y)

# --- jags model
# note :  JAGS uses precision rather than standard deviation in the normal distribution
# precision = standard deviation ^ (-2)
my_model = '
  model{
    # --- likelihood (sampling density)
    for(i in 1:N){
      y[i] ~ dnorm(alpha_est + beta_est*x[i], stdev_est^-2)
    }
  
    # --- priors
    alpha_est ~ dnorm(0, 100^-2)
    beta_est ~ dnorm(0, 100^-2)
    stdev_est ~ dgamma(1, 4)
  }
'

# run model with default jags settings
data = list("y" = y, "x" = x, "N" = N)
jags.run = jags(data = data, 
                parameters.to.save = c('alpha_est', 'beta_est', 'stdev_est'), 
                model.file = textConnection(my_model),
                n.chains=4, # Number of different starting positions
                n.iter=2000,
                n.thin=2) # Amount of thinning

alpha_est = jags.run$BUGSoutput$sims.list$alpha_est
beta_est = jags.run$BUGSoutput$sims.list$beta_est
stdev_est = jags.run$BUGSoutput$sims.list$stdev_est

traceplot(jags.run, mfrow = c(2, 2), ask = F)

# --- ploting the posterior distribution of estimated variables
posterior.dists = data.frame(alpha_est, beta_est, stdev_est)
alpha.est.plot = ggplot()+
  geom_histogram(data = posterior.dists, aes(alpha_est), bins = 20, color="black", fill="lightgreen", alpha = 0.5)+
  ggtitle(paste0("Posterior Distribution of Alpha\nmean = ", signif(mean(alpha_est),5) , ", true = ",alpha_0))+
  theme_minimal()

beta.est.plot = ggplot()+
  geom_histogram(data = posterior.dists, aes(beta_est), bins = 20, color="black", fill="lightgreen", alpha = 0.5)+
  ggtitle(paste0("Posterior Distribution of Beta\nmean = ", signif(mean(beta_est),5) , ", true = ",beta_0))+
  theme_minimal()

stdev.est.plot = ggplot()+
  geom_histogram(data = posterior.dists, aes(stdev_est), bins = 20, color="black", fill="lightgreen", alpha = 0.5)+
  ggtitle(paste0("Posterior Distribution of Std. Dev.\nmean = ", signif(mean(stdev_est),5) , ", true = ",stdev_0))+
  theme_minimal()

grid.arrange(alpha.est.plot, beta.est.plot, stdev.est.plot, nrow = 1)

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
  geom_line(data = results, aes(x = x, y=upper), colour = 'red', size = 1)+
  geom_line(data = results, aes(x = x, y=lower), colour = 'red', size = 1)+
  theme_minimal()








