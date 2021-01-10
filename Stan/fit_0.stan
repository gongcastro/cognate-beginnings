// generated with brms 2.13.5
functions {
  /* cumulative-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     real p;
     if (y == 1) {
       p = inv_logit(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       p = 1 - inv_logit(disc * (thres[nthres] - mu));
     } else {
       p = inv_logit(disc * (thres[y] - mu)) -
           inv_logit(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
  /* cumulative-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
  /* ordered-logistic log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real ordered_logistic_merged_lpmf(int y, real mu, vector thres, int[] j) {
     return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
}
model {
  // initialize linear predictor term
  vector[N] mu = rep_vector(0, N);
  // priors including all constants
  target += normal_lpdf(Intercept | 0, 0.5);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
    }
  }
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept;
}
