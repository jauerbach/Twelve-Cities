data {
  int<lower=1> N_train;// number of training data points
  int<lower=1> N;      // number of total data points
  int<lower=1> G;      // number of groupings
  int<lower=3> J[G];   // group sizes
  int ints[N,G]; // group indices
  vector[N] EXPR;      // population exposed
  int count[N];        // number of pedestrian deaths
}
transformed data {
  vector[N] offset;
  vector[G + 1] prior_counts;
  int count_train[N_train];
  int n_vars;
  offset = log(EXPR);
  count_train = count[1:N_train];
  n_vars = rows(prior_counts);
  for(g in 1:n_vars)
    prior_counts[g] = 2.0;
}
parameters {
  real offset_e;
  vector[J[1]] eta_1;
  vector[J[2]] eta_2;
  vector[J[3]] eta_3;
  vector[J[4]] eta_4;
  vector[J[5]] eta_5;
  vector[J[6]] eta_6;
  vector[J[7]] eta_7;
  vector[J[8]] eta_8;
  vector[J[9]] eta_9;
  vector[J[10]] eta_10;
  vector[J[11]] eta_11;
  vector[J[12]] eta_12;
  vector[J[13]] eta_13;
  vector[J[14]] eta_14;
  vector[J[15]] eta_15;
  vector[J[16]] eta_16;
  vector[J[17]] eta_17;
  vector[J[18]] eta_18;
  vector[J[19]] eta_19;
  vector[J[20]] eta_20;
  vector[J[21]] eta_21;
  real<lower=0> tau;
  simplex[n_vars] prop_var;
  vector[N_train] cell_eta;
  real mu;
}
transformed parameters {
  vector[J[1]] e_1;
  vector[J[2]] e_2;
  vector[J[3]] e_3;
  vector[J[4]] e_4;
  vector[J[5]] e_5;
  vector[J[6]] e_6;
  vector[J[7]] e_7;
  vector[J[8]] e_8;
  vector[J[9]] e_9;
  vector[J[10]] e_10;
  vector[J[11]] e_11;
  vector[J[12]] e_12;
  vector[J[13]] e_13;
  vector[J[14]] e_14;
  vector[J[15]] e_15;
  vector[J[16]] e_16;
  vector[J[17]] e_17;
  vector[J[18]] e_18;
  vector[J[19]] e_19;
  vector[J[20]] e_20;
  vector[J[21]] e_21;
  vector[n_vars] sds;
  vector[N_train] cell_e;
  vector[N_train] mu_indiv;

  {
    vector[n_vars] vars;
    vars = n_vars * square(tau) * prop_var;
    for (g in 1:n_vars)
      sds[g] = sqrt(vars[g]);
  }
  
  e_1 = sds[1] * eta_1;
  e_2 = sds[2] * eta_2;
  e_3 = sds[3] * eta_3;
  e_4 = sds[4] * eta_4;
  e_5 = sds[5] * eta_5;
  e_6 = sds[6] * eta_6;
  e_7 = sds[7] * eta_7;
  e_8 = sds[8] * eta_8;
  e_9 = sds[9] * eta_9;
  e_10 = sds[10] * eta_10;
  e_11 = sds[11] * eta_11;
  e_12 = sds[12] * eta_12;
  e_13 = sds[13] * eta_13;
  e_14 = sds[14] * eta_14;
  e_15 = sds[15] * eta_15;
  e_16 = sds[16] * eta_16;
  e_17 = sds[17] * eta_17;
  e_18 = sds[18] * eta_18;
  e_19 = sds[19] * eta_19;
  e_20 = sds[20] * eta_20;
  e_21 = sds[21] * eta_21;
  cell_e = sds[G+1] * cell_eta;

  for (n in 1:N_train)
    mu_indiv[n] = mu + offset_e * offset[n]
               + e_1[ints[n,1]]
               + e_2[ints[n,2]]
               + e_3[ints[n,3]]
               + e_4[ints[n,4]]
               + e_5[ints[n,5]]
               + e_6[ints[n,6]]
               + e_7[ints[n,7]]
               + e_8[ints[n,8]]
               + e_9[ints[n,9]]
               + e_10[ints[n,10]]
               + e_11[ints[n,11]]
               + e_12[ints[n,12]]
               + e_13[ints[n,13]]
               + e_14[ints[n,14]]
               + e_15[ints[n,15]]
               + e_16[ints[n,16]]
               + e_17[ints[n,17]]
               + e_18[ints[n,18]]
               + e_19[ints[n,19]]
               + e_20[ints[n,20]]
               + e_21[ints[n,21]]
               + cell_e[n];
}
model {
  // priors
  eta_1 ~ normal(0, 1);
  eta_2 ~ normal(0, 1);
  eta_3 ~ normal(0, 1);
  eta_4 ~ normal(0, 1);
  eta_5 ~ normal(0, 1);
  eta_6 ~ normal(0, 1);
  eta_7 ~ normal(0, 1);
  eta_8 ~ normal(0, 1);
  eta_9 ~ normal(0, 1);
  eta_10 ~ normal(0, 1);
  eta_11 ~ normal(0, 1);
  eta_12 ~ normal(0, 1);
  eta_13 ~ normal(0, 1);
  eta_14 ~ normal(0, 1);
  eta_15 ~ normal(0, 1);
  eta_16 ~ normal(0, 1);
  eta_17 ~ normal(0, 1);
  eta_18 ~ normal(0, 1);
  eta_19 ~ normal(0, 1);
  eta_20 ~ normal(0, 1);
  eta_21 ~ normal(0, 1);
  cell_eta ~ normal(0,1);
  offset_e ~ normal(0,1);
  mu ~ normal(-10,3);
  
  tau ~ gamma(4,40);
  prop_var ~ dirichlet(prior_counts);
  
  //likelihood
  for (n in 1:N_train)
    target += -log1m_exp(-exp(mu_indiv[n]));
  count_train ~ poisson_log(mu_indiv);
}
generated quantities {
  real sd_1;
  real sd_2;
  real sd_3;
  real sd_4;
  real sd_5;
  real sd_6;
  real sd_7;
  real sd_8;
  real sd_9;
  real sd_10;
  real sd_11;
  real sd_12;
  real sd_13;
  real sd_14;
  real sd_15;
  real sd_16;
  real sd_17;
  real sd_18;
  real sd_19;
  real sd_20;
  real sd_21;
  real cell_sd;
  vector[N - N_train] mu_indiv_pred;
  vector[N - N_train] cell_e_pred;

  sd_1 = sd(e_1);
  sd_2 = sd(e_2);
  sd_3 = sd(e_3);
  sd_4 = sd(e_4);
  sd_5 = sd(e_5);
  sd_6 = sd(e_6);
  sd_7 = sd(e_7);
  sd_8 = sd(e_8);
  sd_9 = sd(e_9);
  sd_10 = sd(e_10);
  sd_11 = sd(e_11);
  sd_12 = sd(e_12);
  sd_13 = sd(e_13);
  sd_14 = sd(e_14);
  sd_15 = sd(e_15);
  sd_16 = sd(e_16);
  sd_17 = sd(e_17);
  sd_18 = sd(e_18);
  sd_19 = sd(e_19);
  sd_20 = sd(e_20);
  sd_21 = sd(e_21);
  cell_sd = sd(cell_e);
  
  for (n in 1:(N - N_train)){
    cell_e_pred[n] = normal_rng(0, sds[G+1]);
    mu_indiv_pred[n] = mu + offset_e * offset[N_train + n] 
                       + e_1[ints[N_train + n,1]]
                       + e_2[ints[N_train + n,2]]
                       + e_3[ints[N_train + n,3]]
                       + e_4[ints[N_train + n,4]]
                       + e_5[ints[N_train + n,5]]
                       + e_6[ints[N_train + n,6]]
                       + e_7[ints[N_train + n,7]]
                       + e_8[ints[N_train + n,8]]
                       + e_9[ints[N_train + n,9]]
                       + e_10[ints[N_train + n,10]]
                       + e_11[ints[N_train + n,11]]
                       + e_12[ints[N_train + n,12]]
                       + e_13[ints[N_train + n,13]]
                       + e_14[ints[N_train + n,14]]
                       + e_15[ints[N_train + n,15]]
                       + e_16[ints[N_train + n,16]]
                       + e_17[ints[N_train + n,17]]
                       + e_18[ints[N_train + n,18]]
                       + e_19[ints[N_train + n,19]]
                       + e_20[ints[N_train + n,20]]
                       + e_21[ints[N_train + n,21]]
                       + cell_e_pred[n];
 }
}
