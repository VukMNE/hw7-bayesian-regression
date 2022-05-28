data {
  int<lower=1> n; // number of shots
  vector<lower=0>[n] xa;       // angle
  vector<lower=0>[n] xd;       // distance
  int<lower=0, upper=1> y[n];    // target variable
}

parameters {
  real alpha;              // intercept
  real b_angle; // angle coefficient
  real b_dist; // distance coefficient
}

model {  
  y ~ bernoulli_logit(alpha + b_angle * xa + b_dist * xd);
}
