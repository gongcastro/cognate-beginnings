// generated with brms 2.16.1
functions {
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
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[nthres] Intercept;  // temporary thresholds for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    for (n in 1:N) {
      target += cratio_logit_lpmf(Y[n] | mu[n], disc, Intercept);
    }
  }
  // priors including constants
  target += normal_lpdf(Intercept | -0.25, 0.1);
  target += normal_lpdf(sd_1 | 1, 0.1)
    - 1 * normal_lccdf(0 | 1, 0.1);
  target += std_normal_lpdf(z_1[1]);
  target += normal_lpdf(sd_2 | 1, 0.1)
    - 1 * normal_lccdf(0 | 1, 0.1);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept;
  // additionally sample draws from priors
  real prior_Intercept = normal_rng(-0.25,0.1);
  real prior_sd_1 = normal_rng(1,0.1);
  real prior_sd_2 = normal_rng(1,0.1);
  // use rejection sampling for truncated priors
  while (prior_sd_1 < 0) {
    prior_sd_1 = normal_rng(1,0.1);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = normal_rng(1,0.1);
  }
}
