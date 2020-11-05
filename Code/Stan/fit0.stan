
data {
  int<lower=0> N; // number of observations
  vector[N] x; // age (input)
  int<lower=0,upper=1> y[N]; // response variable
}

parameters {
  real alpha;
  real beta;
  
}

model {
  y ~ bernoulli_logit(alpha + beta*x);
}

