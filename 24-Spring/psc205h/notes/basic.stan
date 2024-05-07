data{
   int<lower=0> n;
   vector[n] y;
   vector[n] x;
}
parameters{
   vector[2] beta;
   real<lower=0> sigma;
}
model{
   // Priors
   beta[1] ~ normal(0, 10);
   beta[2] ~ normal(-0.5, 5);
   sigma ~ gamma(2, 1);
   // likelihood
   y ~ normal(beta[1] + x*beta[2], sigma);
}
