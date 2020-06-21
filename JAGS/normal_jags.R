
# This script generates random data froma gaussian distribution 
# and then estimates the parameters of this distribution using JAGS 


# --- libraries 
library(R2jags)
library(ggplot2)
library(gridExtra)

# ------ model specifications
# - - - - - - - - - - - - - - 
# --- model
# y ~ Gaussian(mean = 12, standard deviation = 4)

# --- likelihood
# y_1, ..., y_N | mu_est, stdev_est ~ Gaussian(mean = mu_est, standard deviation = stdev_est)

# --- priors
#mu_est ~ Gaussian(mean = 5, standard deviation = 2)
#stdev_est ~ Gamma(shape = 1, scale = 1)


# --- generate data from normal distribution
mu = 12 
stdev = 4
y = rnorm(100, mean = mu, sd = stdev)
N = length(y)


# --- jags model
# note :  JAGS uses precision rather than standard deviation in the normal distribution
# precision = standard deviation ^ (-2)
my_model = '
  model{
    # --- likelihood (sampling density)
    for(i in 1:N){
      y[i] ~ dnorm(mean, standard_dev^-2)
    }
  
    # --- priors
    mean ~ dnorm(5, 2^-2)
    standard_dev ~ dgamma(1, 1)
  }
'

# run model with default jags settings
data = list("y" = y, "N" = N)
jags.run = jags(data = data, 
                parameters.to.save = c('mean','standard_dev'), 
                model.file = textConnection(my_model))

mean = jags.run$BUGSoutput$sims.list$mean
standard_dev = jags.run$BUGSoutput$sims.list$standard_dev

# --- ploting the posterior distribution of estimated variables
posterior.dists = data.frame(mean, standard_dev)
mean.plot = ggplot()+
  geom_histogram(data = posterior.dists, aes(mean), bins = 40, color="black", fill="lightgreen", alpha = 0.5)+
  ggtitle(paste0("Posterior Distribution of Mean\nmean = ", signif(mean(mean),5) , ", true = ",mu))+
  theme_minimal()

standard_dev.plot = ggplot()+
  geom_histogram(data = posterior.dists, aes(standard_dev), bins = 40, color="black", fill="lightgreen", alpha = 0.5)+
  ggtitle(paste0("Posterior Distribution of Std. Dev.\nmean = ", signif(mean(standard_dev),5) , ", true = ",stdev))+
  theme_minimal()

grid.arrange(mean.plot, standard_dev.plot, nrow = 1)

