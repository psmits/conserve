data {
  int<lower=0> N;  // sample size
  int<lower=2> K;  // number of possible outcomes of y
  int<lower=1> D;  // number of predictors
  int y[N];  // IUCN status of each sample
  row_vector[D] x[N];  // row_vector of covariates per sample, incl intercept
}
parameters {
  vector[D] beta;  // vector of regression coeficients, incl intercept
  ordered[K-1] c;  // defined K-1 guarantees c[k] < c[k + 1]
}
model {
  beta ~ normal(0, 1);

  for(n in 1:N) {  // for each sample...
    // multiply covariates by coefs 
    y[n] ~ ordered_logistic(x[n] * beta, c);  
  }
}
generated quantities {
  int y_new[N];
  vector[N] log_lik;
  vector[N] eta;

  for(n in 1:N) {
    y_new[n] <- ordered_logistic_rng(x[n] * beta, c);
    log_lik[n] <- ordered_logistic_log(y[n], x[n] * beta, c);
    eta[n] <- x[n] * beta;
  }
}
