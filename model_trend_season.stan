data {
  int<lower=1> T;
  real Y[T];
}
parameters{
  real mu[T];
  real s[T];
  real<lower=0> s_mu;
  real<lower=0> s_s;
  real<lower=0> s_y;
}
model {
  for(t in 2:T){
    mu[t] ~ normal(mu[t-1], s_mu);
  }
  for(t in 7:T){
    s[t] ~ normal(-s[t-1]-s[t-2]-s[t-3]-s[t-4]-s[t-5]-s[t-6], s_s);
  }
  for(t in 1:T){
    Y[t] ~ normal(mu[t]+s[t], s_y);
  }
}
