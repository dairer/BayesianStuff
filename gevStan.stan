functions{
  // log of the density function of a GEV
  real Gev_lpdf(real y, real location, real shape, real scale){
    real t;
    if(shape == 0)
      t = exp(-(y - location)/scale);
    if(shape != 0)
      t = (1 + shape*((y - location)/scale))^(-1/shape);
    return log((1/scale)*t^(shape+1)*exp(-t));
  }
}

data {
  int<lower=0> N;
  real y[N];
}

parameters {
  real location;
  real shape;
  real<lower=0> scale;
}

model {
  // priors
  location ~ normal(0, 1);
  shape ~ normal(0, 1);
  scale ~ normal(0, 1);
  
  // likelihood
  for(n in 1:N){
    target += Gev_lpdf(y[n] | location, shape, scale);
  }
}
