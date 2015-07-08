data {
  int<lower=0> N;  // sample size
  int<lower=2> K;  // number of possible outcomes of y
  int<lower=1> D;  // number of predictors
  int y[N];  // IUCN status of each sample
  row_vector[D] x[N];  // row_vector of covariates per sample, incl intercept
  matrix[N, N] vcv;  // phylogenetic covariance matrix
}
transformed data {
  matrix[N, N] vcv_inv;  // inverse of phylogenetic covariance matrix

  vcv_inv <- inverse(vcv);
}
parameters {
  vector[D] beta;  // vector of regression coeficients, incl intercept
  ordered[K-1] c;  // defined K-1 guarantees c[k] < c[k + 1]
  vector[N] phy;  // vector of phylogenetic effect
  real<lower=0> sigma;  // scale of phy effect
}
transformed parameters {
  real<lower=0> sig_sq;  // Brownian motion rate

  sig_sq <- sigma^2;
}
model {
  for(d in 1:D) {
    if(d == 1) {
      beta[d] ~ normal(0, 5);
    } else {
      beta[d] ~ normal(0, 1);
    }
  }
      

  
  // phylogeny
  sigma ~ cauchy(0, 1);
  // non-constant part of log(det(sigma_phy * vcv) ^ -0.5
  increment_log_prob(-0.5 * N * log(sig_sq));
  // log of kernal of mulinorm
  increment_log_prob(-(transpose(phy) * vcv_inv * phy) / (2 * sig_sq));

  for(n in 1:N) {  // for each sample...
    // multiply covariates by coefs 
    y[n] ~ ordered_logistic(x[n] * beta + phy[n], c);  
  }
}
generated quantities {
  int y_new[N];
  vector[N] log_lik;

  for(n in 1:N) {
    y_new[n] <- ordered_logistic_rng(x[n] * beta, c);
    log_lik[n] <- ordered_logistic_log(y[n], x[n] * beta, c);
  }
}
