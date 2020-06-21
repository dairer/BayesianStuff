
rm(list=ls()) # Clear the workspace
library(R2jags)
library(MASS)
library(ggplot2)


N = 20 
alpha = 0
sig = 0.05 # variance or each param
tau = 1
len = 1
set.seed(123)
xtrain = sort(runif(N))

covar <- function(x,y=x, tau, len, sig){
  Sigma = matrix(NA, length(x),length(x))
  for(i in seq(1, length(x))){
    for(j in seq(i, length(x))){
      Sigma[i,j] = Sigma[j,i] = (tau^2) * exp(-len*((x[i]-y[j])^2))
    }
    Sigma[i,i] = sig^2 + tau^2
  }
  Sigma
}

Sigma = covar(xtrain,xtrain, tau = tau, len= len, sig = sig)

ytrain = mvrnorm(1,rep(alpha,N), Sigma)
plot(xtrain,ytrain) 

# Jags code ---------------------------------------------------------------

model_code = '
model
{
  # Likelihood
  y ~ dmnorm.vcov(Mu, Sigma)
  
  # Set up mean and covariance matrix
  for(i in 1:N) {
    Mu[i] <- alpha
    Sigma[i,i] = pow(sig, 2) + pow(tau, 2)
    for(j in (i+1):N) {
      Sigma[i,j] = pow(tau, 2) * exp( - len * pow(x[i] - x[j], 2) )
      Sigma[j,i] = Sigma[i,j]
    }
    
  }
  alpha ~ dnorm(0, 10^-2)
  sig ~ dunif(0, 10)
  tau ~ dunif(0, 10)
  len ~ dunif(0.1, 5)
}
'

# Set up the data
model_data = list(N = N, y = ytrain, x = xtrain)

# Choose the parameters to watch
model_parameters =  c("alpha", "sig", "tau", "len")

# Run the model - can be slow
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code))

visualise_resilts(model_run)


# generate predictions ...
# we want to condition test points on priors
# ----- maths 
# let D = { (x1, y1), (x2, y2), ... (xn, yn)}
# We are interested in Y(x) | D where Y(x) ~ GP


#Y(X) | D ~ GP(mu, Sigma)
# let x = train input and y = train output
# mu = covar(test, x) * covar(train,train)^-1 * y
# Sigma = covar(test, test) - covar(test, x)* covar(train,train)^-1 * covar(test, x)^T


visualise_resilts = function(model){
  
  # get estimated parameters
  alpha_est = mean(model$BUGSoutput$sims.list$alpha)
  sig_est = mean(model$BUGSoutput$sims.list$sig)
  len_est = mean(model$BUGSoutput$sims.list$len)
  tau_est = mean(model$BUGSoutput$sims.list$tau)
  
  calc.covar = function(a,b, tau = tau_est, len = len_est){
    (tau^2) * exp(-len*((a-b)^2))
  }
  
  Sigma= outer(xtrain,xtrain,calc.covar) + diag(sig_est^2 , length(xtrain))
  x_test = seq(min(xtrain),max(xtrain),length.out = 100) # test input
  Sigma_test = outer(x_test,x_test,calc.covar) +  diag(sig_est^2 , length(x_test)) # covarince matrix for test data
  Sigma_test_train = outer(x_test,xtrain,calc.covar) 
  
  SigmaInv <- solve(Sigma) # invert covar matrix
  
  MeanPost <- Sigma_test_train %*% SigmaInv %*% ytrain # mean
  SigmaPost <- Sigma_test - Sigma_test_train %*% SigmaInv %*% t(Sigma_test_train)
  #test_output = rmvnorm(10, MeanPost, SigmaPost)
  
  q1 <- MeanPost + qnorm(0.05, 0, sqrt(diag(SigmaPost)))
  q2 <- MeanPost + qnorm(0.95, 0, sqrt(diag(SigmaPost)))
  
  results = data.frame(x_test, MeanPost, q1, q2)
  orig_data = data.frame(xtrain, ytrain)
  
  ggplot()+
    geom_line(data = results, aes(x_test, MeanPost))+
    geom_line(data = results, aes(x_test, q1))+
    geom_line(data = results, aes(x_test, q2))+
    geom_point(data = orig_data, aes(xtrain, ytrain))+
    theme_minimal()
}


