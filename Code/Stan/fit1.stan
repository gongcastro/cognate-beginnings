// generated with brms 2.13.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_steep;  // number of population-level effects
  matrix[N, K_steep] X_steep;  // population-level design matrix
  int<lower=1> K_mid;  // number of population-level effects
  matrix[N, K_mid] X_mid;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_steep] b_steep;  // population-level effects
  vector[K_mid] b_mid;  // population-level effects
  real<lower=0> sigma;  // residual SD
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
    mu[n] = inv_logit(0.7490903) * inv(1 + exp((nlp_mid[n] - C_1[n]) * exp(nlp_steep[n])));
  }
  // priors including all constants
  target += normal_lpdf(b_steep[1] | 3.279774, 1.5);
  target += normal_lpdf(b_mid[1] | 5.541868, 1);
  target += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
}
