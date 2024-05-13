// Paste the model1.stan code here:
data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  vector[1] mu;
  real<lower=0> sigma;
}

model {
  // Prior
  mu ~ normal(100, 10);
  sigma ~ normal(15, 0);
  // likelihood
  y ~ normal(mu, sigma);
  
}

generated quantities {
  real y_rep[N];
  real log_lik[N];
  for( i in 1:N ){
	y_rep[i] = normal_rng( mu, sigma );
  }
  for(n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | mu, sigma);
  }
}