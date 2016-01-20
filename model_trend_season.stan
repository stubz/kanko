data {
  int<lower=1> T;
  real<lower=0> Y[T];
  int<lower=0, upper=1> D1[T];
  int<lower=0, upper=1> D2[T];
  int<lower=0, upper=6> wday[T];
  int<lower=0, upper=1> shinkansen[T];
}
parameters{
  real mu[T];
  real s[T];
  real<lower=0, upper=1> b1;
  real<lower=0, upper=1> b2;
  real<lower=0, upper=1> b3;
  real<lower=0, upper=1000> s_mu;
  real<lower=0, upper=1000> s_s;
  #real<lower=0, upper=1000> s_y;
  real<lower=0, upper=1000> s_ar;
  real<lower=0> s_r;
  real ar[T];
  real c_ar[2];
  real c_shinkansen;
}
model {
  real sales_mu[T];
  real week[T];
  real shinkansen_effect[T];

  for(t in 3:T){
    mu[t] ~ normal(2*mu[t-1]-mu[t-2], s_mu);
  }
  for(t in 7:T){
    s[t] ~ normal(-s[t-1]-s[t-2]-s[t-3]-s[t-4]-s[t-5]-s[t-6], s_s);
  }
  # holiday impact
  for(t in 1:7){
    week[t] <- s[t] + D1[t]*b1*(s[t-wday[t]]-s[t]);
  }
  for(i in 8:T){
    week[i] <- s[i] + D1[i]*b1*(s[i-wday[i]]-s[i])
                    + D2[i]*(b2*(s[i-wday[i]-2]-s[i]) + b3*(s[i-wday[i]-1]-s[i]));
  }
  # shinkansen effect
  for(i in 1:T){
    shinkansen_effect[i] <- c_shinkansen * shinkansen[i];
  }
  for(t in 3:T){
    ar[t] ~ normal(c_ar[1]*ar[t-1]+c_ar[2]*ar[t-2], s_ar);
  }
  for(t in 1:T){
    sales_mu[t] <- mu[t] + s[t] + ar[t] + week[t] + shinkansen_effect[t];
  }
  for(t in 1:T){
    Y[t] ~ normal(sales_mu[t], s_r);
  }
}
