functions{
  // log of the density function of a GEV
  real GP_lpdf(real y, real shape, real scale, real threshold){
    real t;
    t = (y - threshold)/scale;
    return log((1/scale) * ((1 + shape*t)^(-(1/(shape+1)))));
  }
}

data {
  int<lower=0> N;
  real y[N];
  real threshold;
}

parameters {
  real location;
  real shape;
  real<lower=0> scale;
}

model {
  // priors
  shape ~ normal(0, 1);
  scale ~ normal(0, 1);
  
  // likelihood
  for(n in 1:N){
    target += GP_lpdf(y[n] | shape, scale, threshold);
  }
}
