// generated with brms 2.13.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> K_steep;  // number of population-level effects
  matrix[N, K_steep] X_steep;  // population-level design matrix
  int<lower=1> K_mid;  // number of population-level effects
  matrix[N, K_mid] X_mid;  // population-level design matrix
  // covariate vectors for non-linear functions
  int C_1[N];
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_steep] b_steep;  // population-level effects
  vector[K_mid] b_mid;  // population-level effects
}
transformed parameters {
}
model {
  // initialize linear predictor term
  vector[N] nlp_steep = X_steep * b_steep;
  // initialize linear predictor term
  vector[N] nlp_mid = X_mid * b_mid;
  // initialize non-linear predictor term
  vector[N] mu;
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = 0.6789804 * inv(1 + exp((nlp_mid[n] - C_1[n]) * exp(nlp_steep[n])));
  }
  // priors including all constants
  target += normal_lpdf(b_steep[1] | 3.28, 1.5);
  target += normal_lpdf(b_mid[1] | 6, 1);
  target += normal_lpdf(b_mid[2] | 0, 5);
  // likelihood including all constants
  if (!prior_only) {
    target += binomial_logit_lpmf(Y | trials, mu);
  }
}
generated quantities {
}
