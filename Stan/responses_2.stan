// generated with brms 2.13.5
functions {
  /* sratio-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real sratio_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     vector[nthres + 1] p;
     vector[nthres] q;
     int k = 1;
     while (k <= min(y, nthres)) {
       q[k] = 1 - inv_logit(disc * (thres[k] - mu));
       p[k] = 1 - q[k];
       for (kk in 1:(k - 1)) p[k] = p[k] * q[kk];
       k += 1;
     }
     if (y == nthres + 1) {
       p[nthres + 1] = prod(q);
     }
     return log(p[y]);
   }
  /* sratio-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real sratio_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return sratio_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
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
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
}
model {
  // initialize linear predictor term
  vector[N] mu = Xc * b;
  // priors including all constants
  target += normal_lpdf(b | 0.5, 0.1);
  target += normal_lpdf(Intercept | 0.5, 0.5);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += sratio_logit_lpmf(Y[n] | mu[n], disc, Intercept);
    }
  }
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
}
