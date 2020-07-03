// generated with brms 2.13.0
functions {

  /* zero-one-inflated beta log-PDF of a single response 
   * Args: 
   *   y: response value 
   *   mu: mean parameter of the beta part
   *   phi: precision parameter of the beta part
   *   zoi: zero-one-inflation probability
   *   coi: conditional one-inflation probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
   real zero_one_inflated_beta_lpdf(real y, real mu, real phi,
                                    real zoi, real coi) {
     row_vector[2] shape = [mu * phi, (1 - mu) * phi]; 
     if (y == 0) { 
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(0 | coi); 
     } else if (y == 1) {
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(1 | coi);
     } else { 
       return bernoulli_lpmf(0 | zoi) + beta_lpdf(y | shape[1], shape[2]);
     } 
   }
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_asym;  // number of population-level effects
  matrix[N, K_asym] X_asym;  // population-level design matrix
  int<lower=1> K_mid;  // number of population-level effects
  matrix[N, K_mid] X_mid;  // population-level design matrix
  int<lower=1> K_steep;  // number of population-level effects
  matrix[N, K_steep] X_steep;  // population-level design matrix
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
  vector[K_mid] b_mid;  // population-level effects
  vector[K_steep] b_steep;  // population-level effects
  real Intercept_phi;  // temporary intercept for centered predictors
  real<lower=0,upper=1> zoi;  // zero-one-inflation probability
  real<lower=0,upper=1> coi;  // conditional one-inflation probability
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
  vector[N] nlp_mid = X_mid * b_mid;
  // initialize linear predictor term
  vector[N] nlp_steep = X_steep * b_steep;
  // initialize non-linear predictor term
  vector[N] mu;
  // initialize linear predictor term
  vector[N] phi = Intercept_phi + rep_vector(0, N);
  for (n in 1:N) {
    // add more terms to the linear predictor
    nlp_mid[n] += r_1_mid_1[J_1[n]] * Z_1_mid_1[n];
  }
  for (n in 1:N) {
    // apply the inverse link function
    phi[n] = exp(phi[n]);
  }
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = inv_logit(inv_logit(nlp_asym[n]) * inv(1 + exp((nlp_mid[n] - C_1[n]) * exp(nlp_steep[n]))));
  }
  // priors including all constants
  target += normal_lpdf(b_asym[1] | 0.7857192, 0.1);
  target += normal_lpdf(b_mid[1] | 5.669435, 1);
  target += normal_lpdf(b_mid[2] | 0, 1);
  target += normal_lpdf(b_mid[3] | 0, 1);
  target += normal_lpdf(b_steep[1] | 1.757652, 0.8);
  target += normal_lpdf(Intercept_phi | 1.5, 1);
  target += beta_lpdf(zoi | 1, 1);
  target += beta_lpdf(coi | 1, 1);
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += zero_one_inflated_beta_lpdf(Y[n] | mu[n], phi[n], zoi, coi);
    }
  }
}
generated quantities {
  // actual population-level intercept
  real b_phi_Intercept = Intercept_phi;
}
