functions{
  // log of the density function of a GEV
  real GP_lpdf(real y, real threshold, real shape, real scale){
    if (shape<0 && shape < (-scale/(y-threshold)))
      reject(shape, scale)
    if (scale<=0)
      reject(scale)
    return log(1 / (scale * ( 1 + shape * (y - threshold)/scale)^(1/shape + 1)));
  }
}

data {
  int<lower = 0> N;
  real threshold;
  real<lower = threshold> y[N];
}

transformed data {
  real maxObs = max(y);
}

parameters {
  real <lower=0> scale;
  real <lower=-scale/(maxObs-threshold)> shape;
}

model {
  // priors
  shape ~ uniform(-0.5, 0.5);
  //scale ~ exponential(1);

  // likelihood (can i vectorise this?)
  for(n in 1:N){
    target += GP_lpdf(y[n] | threshold, shape, scale);
  }
}
