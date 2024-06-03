data {
  int<lower = 0 > N;    // Number of observations
  int<lower = 0 > N_mis; // Number of missings
  int pos_missing[N_mis];  // position of missings
  int<lower = 1 > p;    // Predictors + Intercept
  int<lower = 0, upper = 1> y[N]; // y = y_obs + y_mis
  matrix[N,p] X;        // Observations
}

parameters {
  real<lower = 0, upper = 1> theta_mis[N_mis]; // estimated missings
  vector[p] beta; // Regression parameters to be estimated
}
transformed parameters{
  //int y_mis;
  //y_mis = bernoulli_rng(theta_mis);
}
model {
  int ytot[N] = y;
  int y_mis[N_mis];

  // Compute probability, theta_mis, to observer either 1 or 0
  for( i in 1:N_mis){
    if( theta_mis[i] >= 0.5 ){
      y_mis[i] = 1;
    } else {
      y_mis[i] = 0;
    }
  }
  ytot[pos_missing] = y_mis;
  
  // Prior
  theta_mis ~ beta(1,1);
  to_vector( beta ) ~ normal(0, 3);
  // Likelihood
  ytot ~ bernoulli_logit(X * beta );
}

generated quantities {
  int y_rep[N];
  real log_lik[N];
  y_rep = bernoulli_logit_rng( X * beta );
  for(n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | X[n] * beta );
  }
}

