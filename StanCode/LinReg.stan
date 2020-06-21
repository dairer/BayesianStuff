data{
  int N; 
  vector[N] y;
  vector[N] x;
}

parameters {
  real alpha_est;
  real beta_est;
  real<lower=0> stdev_est;
}

model {
  y ~ normal(alpha_est + x*beta_est, stdev_est); 
   // Priors
   alpha_est ~ normal(0, 100);
   beta_est ~ normal(0, 100);
   stdev_est ~ normal(0, 1);
}
