functions{
    // cdf (copula) of Hüsler-Reiss bivariate distribution
    real HuslerReiss(real u, real v, real lambda){
      real arg1;
      real arg2;
      real arg3;
      arg1 = normal_cdf((1/lambda) + (lambda/2) * log(log(v)/log(u)), 0, 1);
      arg2 = normal_cdf((1/lambda) + (lambda/2) * log(log(u)/log(v)), 0, 1);
      arg3 =  (1/sqrt(2*pi())) * exp(-0.5*(((1/lambda) + (lambda/2) * log(log(u)/log(v)))^2));
      return log((1/(u*v)) * (arg1* arg2 - (lambda/(2*log(v)))*arg3) * exp(arg1*log(u) + arg2*log(v)));
    }
}

data {
  int<lower = 0> N;
  real u[N];
  real v[N];
}

parameters {
  real <lower=0> lambda;
}

model {
  lambda ~ lognormal(4,1);
  
  for(n in 1:N){
    target += HuslerReiss(u[n], v[n], lambda);
  }
}
