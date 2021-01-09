// generated with brms 2.13.5
functions {
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
}
transformed parameters {
}
model {
  // initialize linear predictor term
  vector[N] mu = Intercept + rep_vector(0, N);
  // priors including all constants
  target += normal_lpdf(Intercept | 0, 0.5);
  // likelihood including all constants
  if (!prior_only) {
    target += binomial_logit_lpmf(Y | trials, mu);
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
