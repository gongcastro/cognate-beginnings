// generated with brms 2.12.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_asym;  // number of population-level effects
  matrix[N, K_asym] X_asym;  // population-level design matrix
  int<lower=1> K_steep;  // number of population-level effects
  matrix[N, K_steep] X_steep;  // population-level design matrix
  int<lower=1> K_mid;  // number of population-level effects
  matrix[N, K_mid] X_mid;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_mid_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_asym] b_asym;  // population-level effects
  vector[K_steep] b_steep;  // population-level effects
  vector[K_mid] b_mid;  // population-level effects
  real<lower=0> sigma;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_mid_1;  // actual group-level effects
  r_1_mid_1 = (sd_1[1] * (z_1[1]));
}
model {
  // initialize linear predictor term
  vector[N] nlp_asym = X_asym * b_asym;
  // initialize linear predictor term
  vector[N] nlp_steep = X_steep * b_steep;
  // initialize linear predictor term
  vector[N] nlp_mid = X_mid * b_mid;
  // initialize non-linear predictor term
  vector[N] mu;
  for (n in 1:N) {
    // add more terms to the linear predictor
    nlp_mid[n] += r_1_mid_1[J_1[n]] * Z_1_mid_1[n];
  }
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = nlp_asym[n] * inv(1 + exp((nlp_mid[n] - C_1[n]) * nlp_steep[n]));
  }
  // priors including all constants
  target += normal_lpdf(b_asym[1] | 0.7857192, 0.5);
  target += normal_lpdf(b_steep[1] | -1.757652, 1);
  target += normal_lpdf(b_mid[1] | 4.369435, 1);
  target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += cauchy_lpdf(sd_1 | 0, 5)
    - 1 * cauchy_lccdf(0 | 0, 5);
  target += normal_lpdf(z_1[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
}
