data {
  int<lower=1> N_train;// number of training data points
  int<lower=1> N;      // number of total data points
  int<lower=1> G;      // number of groupings
  int<lower=3> J[G];   // group sizes
  int ints[N,G]; // group indices
  vector[N] EXPR;      // population exposed
  int count[N];        // number of pedestrian deaths
  int<lower=1> n_main;
  int<lower=1> n_inter;
}
transformed data {
  vector[N] offset = log(EXPR);
  int count_train[N_train] = count[1:N_train];
  int n_vars = G + 1;
}
parameters {
  real offset_e;
  vector[J[1]] eta_1;
  vector[J[2] - 1] eta_2;
  real nyc_e;
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
  vector[J[22]] eta_22;
  vector[J[23]] eta_23;
  vector[J[24]] eta_24;
  vector[J[25]] eta_25;
  vector[J[26]] eta_26;
  vector[J[27]] eta_27;
  vector[J[28]] eta_28;
  vector[J[29]] eta_29;
  vector[J[30]] eta_30;
  vector[J[31]] eta_31;
  vector[J[32]] eta_32;
  vector[J[33]] eta_33;
  vector[J[34]] eta_34;
  vector[J[35]] eta_35;
  vector[J[36]] eta_36;
  // vector[J[37]] eta_37;
  // cell-by-cell effect
  vector[N_train] cell_eta;
  // standard deviation of random effects parameters
  vector[n_main] eta_main;
  vector[n_inter] eta_inter;
  real<lower=0> sigma_sigma_main;
  real<lower=0> sigma_sigma_inter;
  real<lower=0> sigma_cell;
  real tau_main;
  real tau_inter;
  // overall mean
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
  vector[J[22]] e_22;
  vector[J[23]] e_23;
  vector[J[24]] e_24;
  vector[J[25]] e_25;
  vector[J[26]] e_26;
  vector[J[27]] e_27;
  vector[J[28]] e_28;
  vector[J[29]] e_29;
  vector[J[30]] e_30;
  vector[J[31]] e_31;
  vector[J[32]] e_32;
  vector[J[33]] e_33;
  vector[J[34]] e_34;
  vector[J[35]] e_35;
  vector[J[36]] e_36;
  // vector[J[37]] e_37;
  vector[n_vars] sds;
  vector[N_train] cell_e;
  vector[N_train] mu_indiv;
  vector[n_inter] sigma_inter;
  vector[n_main] sigma_main;
  
  sigma_main = exp(-1 + 0.5 * tau_main + 0.3 * sigma_sigma_main * eta_main);
  sigma_inter = exp(-2 + 0.5 * tau_inter + 0.3 * sigma_sigma_inter * eta_inter);
  sds[1:n_main] = sigma_main;
  sds[(n_main + 1):(n_inter + n_main)] = sigma_inter;
  sds[n_vars] = 0.3 * sigma_cell;
  
  e_1 = sds[1] * eta_1;
  e_2[1:(J[2] - 1)] = sds[2] * eta_2;
  e_2[J[2]] = nyc_e;
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
  e_22 = sds[22] * eta_22;
  e_23 = sds[23] * eta_23;
  e_24 = sds[24] * eta_24;
  e_25 = sds[25] * eta_25;
  e_26 = sds[26] * eta_26;
  e_27 = sds[27] * eta_27;
  e_28 = sds[28] * eta_28;
  e_29 = sds[29] * eta_29;
  e_30 = sds[30] * eta_30;
  e_31 = sds[31] * eta_31;
  e_32 = sds[32] * eta_32;
  e_33 = sds[33] * eta_33;
  e_34 = sds[34] * eta_34;
  e_35 = sds[35] * eta_35;
  e_36 = sds[36] * eta_36;
  // e_37 = sds[37] * eta_37;
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
               + e_22[ints[n,22]]
               + e_23[ints[n,23]]
               + e_24[ints[n,24]]
               + e_25[ints[n,25]]
               + e_26[ints[n,26]]
               + e_27[ints[n,27]]
               + e_28[ints[n,28]]
               + e_29[ints[n,29]]
               + e_30[ints[n,30]]
               + e_31[ints[n,31]]
               + e_32[ints[n,32]]
               + e_33[ints[n,33]]
               + e_34[ints[n,34]]
               + e_35[ints[n,35]]
               + e_36[ints[n,36]]
               // + e_37[ints[n,37]]
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
  eta_22 ~ normal(0, 1);
  eta_23 ~ normal(0, 1);
  eta_24 ~ normal(0, 1);
  eta_25 ~ normal(0, 1);
  eta_26 ~ normal(0, 1);
  eta_27 ~ normal(0, 1);
  eta_28 ~ normal(0, 1);
  eta_29 ~ normal(0, 1);
  eta_30 ~ normal(0, 1);
  eta_31 ~ normal(0, 1);
  eta_32 ~ normal(0, 1);
  eta_33 ~ normal(0, 1);
  eta_34 ~ normal(0, 1);
  eta_35 ~ normal(0, 1);
  eta_36 ~ normal(0, 1);
  // eta_37 ~ normal(0, 1);
  cell_eta ~ normal(0, 1);
  offset_e ~ normal(0, 1);
  mu ~ normal(-10,3);
  nyc_e ~ normal(0, 2);
  
  sigma_sigma_inter ~ normal(0, 1);
  sigma_sigma_main ~ normal(0, 1);
  
  eta_inter ~ normal(0, 1);
  eta_main ~ normal(0, 1);
  
  tau_inter ~ normal(0, 1);
  tau_main ~ normal(0, 1);
  
  sigma_cell ~ normal(0, 1);
  
  //likelihood
  for (n in 1:N_train)
    target += -log1m_exp(-exp(mu_indiv[n])) + poisson_log_lpmf(count_train[n] | mu_indiv[n]);
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
  real sd_22;
  real sd_23;
  real sd_24;
  real sd_25;
  real sd_26;
  real sd_27;
  real sd_28;
  real sd_29;
  real sd_30;
  real sd_31;
  real sd_32;
  real sd_33;
  real sd_34;
  real sd_35;
  real sd_36;
  // real sd_37;
  real cell_sd;
  vector[N - N_train] mu_indiv_pred;
  vector[N - N_train] mu_indiv_pred30;
  vector[N - N_train] mu_indiv_pred25;
  vector[N_train] log_lik;

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
  sd_22 = sd(e_22);
  sd_23 = sd(e_23);
  sd_24 = sd(e_24);
  sd_25 = sd(e_25);
  sd_26 = sd(e_26);
  sd_27 = sd(e_27);
  sd_28 = sd(e_28);
  sd_29 = sd(e_29);
  sd_30 = sd(e_30);
  sd_31 = sd(e_31);
  sd_32 = sd(e_32);
  sd_33 = sd(e_33);
  sd_34 = sd(e_34);
  sd_35 = sd(e_35);
  sd_36 = sd(e_36);
  // sd_37 = sd(e_37);
  cell_sd = sd(cell_e);
  
  for (n in 1:N_train)
    log_lik[n] = -log1m_exp(-exp(mu_indiv[n]))
                + poisson_log_lpmf(count[n] | mu_indiv[n]);
  
  for (n in 1:(N - N_train)){
    real year_pred = normal_rng(0, sds[7]);
    real cell_e_pred = normal_rng(0, sds[G+1]);
    mu_indiv_pred[n] = mu + offset_e * offset[N_train + n] 
                       + e_1[ints[N_train + n,1]]
                       + e_2[ints[N_train + n,2]]
                       + e_3[ints[N_train + n,3]]
                       + e_4[ints[N_train + n,4]]
                       + e_5[ints[N_train + n,5]]
                       + e_6[ints[N_train + n,6]]
                       + year_pred
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
                       + e_22[ints[N_train + n,22]]
                       + e_23[ints[N_train + n,23]]
                       + e_24[ints[N_train + n,24]]
                       + e_25[ints[N_train + n,25]]
                       + e_26[ints[N_train + n,26]]
                       + e_27[ints[N_train + n,27]]
                       + e_28[ints[N_train + n,28]]
                       + e_29[ints[N_train + n,29]]
                       + e_30[ints[N_train + n,30]]
                       + e_31[ints[N_train + n,31]]
                       + e_32[ints[N_train + n,32]]
                       + e_33[ints[N_train + n,33]]
                       + e_34[ints[N_train + n,34]]
                       + e_35[ints[N_train + n,35]]
                       + e_36[ints[N_train + n,36]]
                       // + e_37[ints[N_train + n,37]]
                       + cell_e_pred;
    mu_indiv_pred30[n] = mu + offset_e * offset[N_train + n] 
                       + e_1[ints[N_train + n,1]]
                       + e_2[ints[N_train + n,2]]
                       + e_3[ints[N_train + n,3]]
                       + e_4[ints[N_train + n,4]]
                       + e_5[ints[N_train + n,5]]
                       + e_6[7]
                       + year_pred
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
                       + e_22[ints[N_train + n,22]]
                       + e_23[ints[N_train + n,23]]
                       + e_24[ints[N_train + n,24]]
                       + e_25[ints[N_train + n,25]]
                       + e_26[ints[N_train + n,26]]
                       + e_27[ints[N_train + n,27]]
                       + e_28[ints[N_train + n,28]]
                       + e_29[ints[N_train + n,29]]
                       + e_30[ints[N_train + n,30]]
                       + e_31[ints[N_train + n,31]]
                       + e_32[ints[N_train + n,32]]
                       + e_33[ints[N_train + n,33]]
                       + e_34[ints[N_train + n,34]]
                       + e_35[ints[N_train + n,35]]
                       + e_36[ints[N_train + n,36]]
                       // + e_37[ints[N_train + n,37]]
                       + cell_e_pred;
    mu_indiv_pred25[n] = mu + offset_e * offset[N_train + n] 
                       + e_1[ints[N_train + n,1]]
                       + e_2[ints[N_train + n,2]]
                       + e_3[ints[N_train + n,3]]
                       + e_4[ints[N_train + n,4]]
                       + e_5[ints[N_train + n,5]]
                       + e_6[6]
                       + year_pred
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
                       + e_22[ints[N_train + n,22]]
                       + e_23[ints[N_train + n,23]]
                       + e_24[ints[N_train + n,24]]
                       + e_25[ints[N_train + n,25]]
                       + e_26[ints[N_train + n,26]]
                       + e_27[ints[N_train + n,27]]
                       + e_28[ints[N_train + n,28]]
                       + e_29[ints[N_train + n,29]]
                       + e_30[ints[N_train + n,30]]
                       + e_31[ints[N_train + n,31]]
                       + e_32[ints[N_train + n,32]]
                       + e_33[ints[N_train + n,33]]
                       + e_34[ints[N_train + n,34]]
                       + e_35[ints[N_train + n,35]]
                       + e_36[ints[N_train + n,36]]
                       // + e_37[ints[N_train + n,37]]
                       + cell_e_pred;
 }
}
