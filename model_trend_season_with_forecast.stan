data {
  int<lower=1> T;
	int<lower=1> T_next;
  real<lower=0> Y[T];
  int<lower=0, upper=1> D1[T+T_next];
  int<lower=0, upper=1> D2[T+T_next];
  int<lower=0, upper=6> wday[T+T_next];
  int<lower=0, upper=1> shinkansen[T+T_next];
}
parameters{
  real mu[T];
  real s[T];
  real<lower=0, upper=1> b1;
  real<lower=0, upper=1> b2;
  real<lower=0, upper=1> b3;
  real<lower=0, upper=1000> s_mu;
  real<lower=0, upper=1000> s_s;
  real<lower=0, upper=1000> s_ar;
  real<lower=0> s_r;
  real ar[T];
  real c_ar[2];
  real c_shinkansen;
}
model {
  real sales_mu[T];
  real shinkansen_effect[T];
  real week[T];

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
generated quantities {
	real mu_all[T+T_next];
  real s_all[T+T_next];
	real ar_all[T+T_next];
	real visit_all[T+T_next];
  real y_next[T_next];
	real week_all[T+T_next];
	real shinkansen_effect_all[T+T_next];

  for (t in 1:T){
		mu_all[t] <- mu[t];
		s_all[t] <- s[t];
	}
	ar_all[1] <- 0;
	ar_all[2] <- 0;
	for (t in 3:T){
		ar_all[t] <- ar[t];
	}
	for (t in (T+1):(T+T_next)){
		mu_all[t] <- normal_rng(mu_all[t-1], s_mu);
		s_all[t] <- normal_rng(-s_all[t-1]-s_all[t-2]-s_all[t-3]-s_all[t-4]-s_all[t-5]-s_all[t-6], s_s);
		ar_all[t] <- normal_rng(c_ar[1]*ar_all[t-1]+c_ar[2]*ar_all[t-2], s_ar); 
	}
	# holiday impact 
  for(t in 1:7){
    week_all[t] <- s_all[t] + D1[t]*b1*(s_all[t-wday[t]]-s_all[t]);
  }
  for(t in 8:(T+T_next)){
    week_all[t] <- s_all[t] + D1[t]*b1*(s_all[t-wday[t]]-s_all[t])
                    + D2[t]*(b2*(s_all[t-wday[t]-2]-s_all[t]) + b3*(s_all[t-wday[t]-1]-s_all[t]));
  }
  # shinkansen effect
  for(t in 1:(T+T_next)){
    shinkansen_effect_all[t] <- c_shinkansen * shinkansen[t];
  }
	for (t in 1:T_next){
		y_next[t] <- normal_rng(mu_all[T+t]+s_all[T+t]+ar_all[T+t] + week_all[T+t] + shinkansen_effect_all[T+t], s_r);
	}
	for (t in 8:(T+T_next)){
		visit_all[t] <- normal_rng(mu_all[t]+s_all[t]+ar_all[t] + week_all[t] + shinkansen_effect_all[t], s_r);
	}
}
