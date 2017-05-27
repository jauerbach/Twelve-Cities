functions {
  int poisson_log_trunc_rng(real log_rate) {
    int draw;
    draw = poisson_log_rng(log_rate);
    while (draw == 0)
      draw = poisson_log_rng(log_rate);
    return draw;
  }
}
data {
  int<lower=1> N_train;// number of training data points
  int<lower=1> N;      // number of total data points
  int<lower=1> G;      // number of groupings
  int<lower=1> G_ints;      // number of groupings
  int<lower=3> J[G];   // group sizes
  int<lower=1> J_ints[G_ints];
  int COND[N];         // index for surface condition/inclement weather
  int CITY[N];         // index for city
  int YEAR[N];         // index for year
  int SLIM[N];         // index for posted speed limit
  int SIGN[N];         // index for signs and signals i.e. school zone/work zone
  int LGHT[N];         // index for light and time
  int BLTE[N];         // index for built environment
  int TFFC[N];         // index for traffic volume
  vector[N] EXPR;      // population exposed
  int count[N];        // number of pedestrian deaths
  int ints[N,G_ints]; // index for interaction terms
}
transformed data {
  vector[N] offset;
  vector[G + G_ints + 1] prior_counts;
  int n_vars;
  offset = log(EXPR);
  n_vars = rows(prior_counts);
  for(g in 1:n_vars)
    prior_counts[g] = 2.0;
}
parameters {
  real offset_e;
  vector[J[1]] COND_eta;
  vector[J[2]] CITY_eta;
  vector[J[3]] YEAR_eta;
  vector[J[4]] SLIM_eta;
  vector[J[5]] SIGN_eta;
  vector[J[6]] LGHT_eta;
  vector[J[7]] BLTE_eta;
  vector[J[8]] TFFC_eta;
  vector[J_ints[1]] CITY_YEAR_eta;
  vector[J_ints[2]] CITY_SLIM_eta;
  vector[J_ints[3]] CITY_SIGN_eta;
  vector[J_ints[4]] CITY_LGHT_eta;
  vector[J_ints[5]] YEAR_SLIM_eta;
  vector[J_ints[6]] YEAR_SIGN_eta;
  vector[J_ints[7]] YEAR_LGHT_eta;
  vector[J_ints[8]] SLIM_SIGN_eta;
  vector[J_ints[9]] SLIM_LGHT_eta;
  vector[J_ints[10]] SIGN_LGHT_eta;
  vector[N_train] cell_eta;
  simplex[n_vars] prop_var;
  real<lower=0> tau;
  real mu;
}
transformed parameters {
  vector[J[1]] COND_e;
  vector[J[2]] CITY_e;
  vector[J[3]] YEAR_e;
  vector[J[4]] SLIM_e;
  vector[J[5]] SIGN_e;
  vector[J[6]] LGHT_e;
  vector[J[7]] BLTE_e;
  vector[J[8]] TFFC_e; 
  vector[J_ints[1]] CITY_YEAR_e;
  vector[J_ints[2]] CITY_SLIM_e;
  vector[J_ints[3]] CITY_SIGN_e;
  vector[J_ints[4]] CITY_LGHT_e;
  vector[J_ints[5]] YEAR_SLIM_e;
  vector[J_ints[6]] YEAR_SIGN_e;
  vector[J_ints[7]] YEAR_LGHT_e;
  vector[J_ints[8]] SLIM_SIGN_e;
  vector[J_ints[9]] SLIM_LGHT_e;
  vector[J_ints[10]] SIGN_LGHT_e;
  vector[N_train] cell_e;
  vector[N_train] mu_indiv;
  vector[n_vars] sds;
  
  {
    vector[n_vars] vars;
    vars = (n_vars) * square(tau) * prop_var;
    for (g in 1:n_vars)
      sds[g] = sqrt(vars[g]);
  }
  COND_e = sds[1] * COND_eta;
  CITY_e = sds[2] * CITY_eta;
  YEAR_e = sds[3] * YEAR_eta;
  SLIM_e = sds[4] * SLIM_eta;
  SIGN_e = sds[5] * SIGN_eta;
  LGHT_e = sds[6] * LGHT_eta;
  BLTE_e = sds[7] * BLTE_eta; 
  TFFC_e = sds[8] * TFFC_eta; 
  cell_e = sds[9] * cell_eta;
  CITY_YEAR_e = sds[10] * CITY_YEAR_eta;
  CITY_SLIM_e = sds[11] * CITY_SLIM_eta;
  CITY_SIGN_e = sds[12] * CITY_SIGN_eta;
  CITY_LGHT_e = sds[13] * CITY_LGHT_eta;
  YEAR_SLIM_e = sds[14] * YEAR_SLIM_eta;
  YEAR_SIGN_e = sds[15] * YEAR_SIGN_eta;
  YEAR_LGHT_e = sds[16] * YEAR_LGHT_eta;
  SLIM_SIGN_e = sds[17] * SLIM_SIGN_eta;
  SLIM_LGHT_e = sds[18] * SLIM_LGHT_eta;
  SIGN_LGHT_e = sds[19] * SIGN_LGHT_eta;

  for(n in 1:N_train)
  mu_indiv[n] = mu + offset_e * offset[n]
                   + COND_e[COND[n]]
                   + CITY_e[CITY[n]]
                   + YEAR_e[YEAR[n]]
                   + SLIM_e[SLIM[n]]
                   + SIGN_e[SIGN[n]]
                   + LGHT_e[LGHT[n]]
                   + BLTE_e[BLTE[n]]
                   + TFFC_e[TFFC[n]]
                   + cell_e[n]
                   + CITY_YEAR_eta[ints[n,1]]
                   + CITY_SLIM_eta[ints[n,2]]
                   + CITY_SIGN_eta[ints[n,3]]
                   + CITY_LGHT_eta[ints[n,4]]
                   + YEAR_SLIM_eta[ints[n,5]]
                   + YEAR_SIGN_eta[ints[n,6]]
                   + YEAR_LGHT_eta[ints[n,7]]
                   + SLIM_SIGN_eta[ints[n,8]]
                   + SLIM_LGHT_eta[ints[n,9]]
                   + SIGN_LGHT_eta[ints[n,10]];
}
model {
  COND_eta ~ normal(0,1);
  CITY_eta ~ normal(0,1);
  YEAR_eta ~ normal(0,1);
  SLIM_eta ~ normal(0,1);
  SIGN_eta ~ normal(0,1);
  LGHT_eta ~ normal(0,1);
  BLTE_eta ~ normal(0,1);
  TFFC_eta ~ normal(0,1);
  cell_eta ~ normal(0,1);
  offset_e ~ normal(0,1);
  CITY_YEAR_eta ~ normal(0,1);
  CITY_SLIM_eta ~ normal(0,1);
  CITY_SIGN_eta ~ normal(0,1);
  CITY_LGHT_eta ~ normal(0,1);
  YEAR_SLIM_eta ~ normal(0,1);
  YEAR_SIGN_eta ~ normal(0,1);
  YEAR_LGHT_eta ~ normal(0,1);
  SLIM_SIGN_eta ~ normal(0,1);
  SLIM_LGHT_eta ~ normal(0,1);
  SIGN_LGHT_eta ~ normal(0,1);
  tau ~ gamma(4,40);
  prop_var ~ dirichlet(prior_counts);
  mu       ~ normal(-10, 3);
  
 for (n in 1:N_train){
    target += poisson_log_lpmf(count[n] | mu_indiv[n]);
    target += -log1m_exp(-exp(mu_indiv[n]));
 }
}
generated quantities {
  real COND_sd;
  real CITY_sd;
  real YEAR_sd;
  real SLIM_sd;
  real SIGN_sd;
  real LGHT_sd;
  real BLTE_sd;
  real TFFC_sd;
  real cell_sd;
  vector[N - N_train] y_pred25;
  vector[N - N_train] y_pred30;
  vector[N - N_train] mu_indiv_pred;
  vector[N - N_train] cell_e_pred;
  vector[N_train] y_pred;
  
  COND_sd = sd(COND_e);
  CITY_sd = sd(CITY_e);
  YEAR_sd = sd(YEAR_e);
  SLIM_sd = sd(SLIM_e);
  SIGN_sd = sd(SIGN_e);
  LGHT_sd = sd(LGHT_e);
  BLTE_sd = sd(BLTE_e);
  TFFC_sd = sd(TFFC_e);
  cell_sd = sd(cell_e);
  
  for (n in 1:(N - N_train)){
    cell_e_pred[n]   = normal_rng(0, sds[n_vars]);
    mu_indiv_pred[n] = mu + offset_e * offset[N_train + n] 
                          + COND_e[COND[N_train + n]]
                          + CITY_e[CITY[N_train + n]]
                          + YEAR_e[YEAR[N_train + n]]
                          + SIGN_e[SIGN[N_train + n]]
                          + LGHT_e[LGHT[N_train + n]]
                          + BLTE_e[BLTE[N_train + n]]
                          + TFFC_e[TFFC[N_train + n]]
                          + cell_e_pred[n]
                         + CITY_YEAR_eta[ints[N_train + n,1]]
                         + CITY_SLIM_eta[ints[N_train + n,2]]
                         + CITY_SIGN_eta[ints[N_train + n,3]]
                         + CITY_LGHT_eta[ints[N_train + n,4]]
                         + YEAR_SLIM_eta[ints[N_train + n,5]]
                         + YEAR_SIGN_eta[ints[N_train + n,6]]
                         + YEAR_LGHT_eta[ints[N_train + n,7]]
                         + SLIM_SIGN_eta[ints[N_train + n,8]]
                         + SLIM_LGHT_eta[ints[N_train + n,9]]
                         + SIGN_LGHT_eta[ints[N_train + n,10]];
//ifelse since during warmup large means cause prediction overflow and stan to terminate
    if(mu_indiv_pred[n] > 5) {
      y_pred25[n] = .5;
      y_pred30[n] = .5;} else{
      y_pred25[n] = poisson_log_trunc_rng(mu_indiv_pred[n] + SLIM_e[6]);
      y_pred30[n] = poisson_log_trunc_rng(mu_indiv_pred[n] + SLIM_e[7]);
    }
  } 
  for (n in 1:(N_train)){
    if(mu_indiv[n] > 5) {
      y_pred[n] = .5;
    } else{
      y_pred[n] = poisson_log_trunc_rng(mu_indiv[n]);
    }
  } 
}
