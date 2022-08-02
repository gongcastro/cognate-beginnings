// generated with brms 2.16.1
functions {
 /* compute correlated group-level effects
  * Args: 
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns: 
  *   matrix of scaled group-level effects
  */ 
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  /* cratio-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cratio_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     vector[nthres + 1] p;
     vector[nthres] q;
     int k = 1;
     while (k <= min(y, nthres)) {
       q[k] = log_inv_logit(disc * (mu - thres[k]));
       p[k] = log1m_exp(q[k]);
       for (kk in 1:(k - 1)) p[k] = p[k] + q[kk];
       k += 1;
     }
     if (y == nthres + 1) {
       p[nthres + 1] = sum(q);
     }
     return p[y];
   }
  /* cratio-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cratio_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return cratio_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  vector[N] Z_1_2;
  vector[N] Z_1_3;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  vector[N] Z_2_2;
  int<lower=1> NC_2;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K;
  matrix[N, Kc] Xc;  // centered version of X
  vector[Kc] means_X;  // column means of X before centering
  for (i in 1:K) {
    means_X[i] = mean(X[, i]);
    Xc[, i] = X[, i] - means_X[i];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  vector[nthres] Intercept;  // temporary thresholds for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  matrix[M_2, N_2] z_2;  // standardized group-level effects
  cholesky_factor_corr[M_2] L_2;  // cholesky factor of correlation matrix
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  vector[N_1] r_1_3;
  matrix[N_2, M_2] r_2;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_1;
  vector[N_2] r_2_2;
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];
  r_1_3 = r_1[, 3];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_1 = r_2[, 1];
  r_2_2 = r_2[, 2];
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Xc * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_1_2[J_1[n]] * Z_1_2[n] + r_1_3[J_1[n]] * Z_1_3[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_2_2[J_2[n]] * Z_2_2[n];
    }
    for (n in 1:N) {
      target += cratio_logit_lpmf(Y[n] | mu[n], disc, Intercept);
    }
  }
  // priors including constants
  target += normal_lpdf(b[1] | 1, 0.1);
  target += normal_lpdf(b[2] | 0, 0.1);
  target += normal_lpdf(Intercept | -0.25, 0.1);
  target += normal_lpdf(sd_1 | 1, 0.1)
    - 3 * normal_lccdf(0 | 1, 0.1);
  target += std_normal_lpdf(to_vector(z_1));
  target += lkj_corr_cholesky_lpdf(L_1 | 2);
  target += normal_lpdf(sd_2 | 1, 0.1)
    - 2 * normal_lccdf(0 | 1, 0.1);
  target += std_normal_lpdf(to_vector(z_2));
  target += lkj_corr_cholesky_lpdf(L_2 | 2);
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1,upper=1>[NC_2] cor_2;
  // additionally sample draws from priors
  real prior_b_1 = normal_rng(1,0.1);
  real prior_b_2 = normal_rng(0,0.1);
  real prior_Intercept = normal_rng(-0.25,0.1);
  real prior_sd_1 = normal_rng(1,0.1);
  real prior_cor_1 = lkj_corr_rng(M_1,2)[1, 2];
  real prior_sd_2 = normal_rng(1,0.1);
  real prior_cor_2 = lkj_corr_rng(M_2,2)[1, 2];
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_2) {
    for (j in 1:(k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
  // use rejection sampling for truncated priors
  while (prior_sd_1 < 0) {
    prior_sd_1 = normal_rng(1,0.1);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = normal_rng(1,0.1);
  }
}
