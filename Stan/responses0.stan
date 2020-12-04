// generated with brms 2.13.5
functions {
  /* acat-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real acat_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     vector[nthres + 1] p;
     p[1] = 0.0;
     for (k in 1:(nthres)) {
       p[k + 1] = p[k] + disc * (mu - thres[k]);
     }
     p = exp(p);
     return log(p[y] / sum(p));
   }
  /* acat-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real acat_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return acat_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> Kcs;  // number of category specific effects
  matrix[N, Kcs] Xcs;  // category specific design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[nthres] Intercept;  // temporary thresholds for centered predictors
  matrix[Kcs, nthres] bcs;  // category specific effects
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
}
model {
  // linear predictor for category specific effects
  matrix[N, nthres] mucs = Xcs * bcs;
  // initialize linear predictor term
  vector[N] mu = rep_vector(0, N);
  // priors including all constants
  target += normal_lpdf(Intercept | 0.5, 0.5);
  target += normal_lpdf(bcs[1] | 0.1, 0.15);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += acat_logit_lpmf(Y[n] | mu[n], disc, Intercept - mucs[n]');
    }
  }
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept;
  // additionally draw samples from priors
  real prior_Intercept = normal_rng(0.5,0.5);
  real prior_bcs_1 = normal_rng(0.1,0.15);
}
