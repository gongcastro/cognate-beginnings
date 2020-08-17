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
  vector[N] Z_1_mid_2;
  int<lower=1> NC_1;  // number of group-level correlations
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
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_mid_1;
  vector[N_1] r_1_mid_2;
  // compute actual group-level effects
  r_1 = (diag_pre_multiply(sd_1, L_1) * z_1)';
  r_1_mid_1 = r_1[, 1];
  r_1_mid_2 = r_1[, 2];
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
    nlp_mid[n] += r_1_mid_1[J_1[n]] * Z_1_mid_1[n] + r_1_mid_2[J_1[n]] * Z_1_mid_2[n];
  }
  for (n in 1:N) {
    // compute non-linear predictor values
    mu[n] = nlp_asym[n] * inv(1 + exp((nlp_mid[n] - C_1[n]) * nlp_steep[n]));
  }
  // priors including all constants
  target += normal_lpdf(b_asym[1] | 0.7631182, 0.5);
  target += normal_lpdf(b_steep[1] | 1.6859966, 1);
  target += normal_lpdf(b_mid[1] | 4.369435, 1);
  target += normal_lpdf(b_mid[2] | 0, 1);
  target += normal_lpdf(b_mid[3] | 0, 1);
  target += normal_lpdf(b_mid[4] | 0, 1);
  target += normal_lpdf(b_mid[5] | 0, 1);
  target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += cauchy_lpdf(sd_1 | 1.5, 1)
    - 2 * cauchy_lccdf(0 | 1.5, 1);
  target += normal_lpdf(to_vector(z_1) | 0, 1);
  target += lkj_corr_cholesky_lpdf(L_1 | 2);
  // likelihood including all constants
  if (!prior_only) {
    target += normal_lpdf(Y | mu, sigma);
  }
}
generated quantities {
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
}
