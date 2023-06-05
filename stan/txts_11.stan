data {
  int n_years;
  int Year[n_years];
  int<lower=0> N_Txi_Pub[n_years];
  int<lower=0> N_Txi_Eng[n_years];
  int<lower=0> Pop[n_years];
  
  real<lower=0> Drug[n_years];
  real<lower=0> Drug_Std[n_years];
  
  int<lower=0> Tx;
  int<lower=0> Tx_Pub;
  
  real<lower=0.5, upper=1> ppv_pub;
  
  real<lower=0.5> dur_upper;
}
parameters {
  real<lower=0.05, upper=1> ppm;
  real<lower=0.2, upper=ppv_pub> ppv_pri;
  
  real<lower=0, upper=0.1> txi_pub[n_years];
  real<lower=0, upper=0.1> txi_eng[n_years];
  
  
  real<lower=0.04166667, upper=dur_upper> dur_pri;
  real<lower=0, upper=1> p_pri_on_pub;
}
transformed parameters {
  real<lower=0, upper=1> txi_pri[n_years];

  real<lower=0, upper=1> ppv_eng;
  
  real tx_pri[n_years];
  real drug_time[n_years];
  real p_pub;

  ppv_eng = ppv_pri;
  
  for (i in 1:n_years) {
    txi_pri[i] = txi_eng[i] * (1 - ppm) / ppm;
    tx_pri[i] = txi_eng[i] * (1 - p_pri_on_pub) / ppv_eng + txi_pri[i] / ppv_pri;
    drug_time[i] = tx_pri[i] * dur_pri;
  
  }
  
  p_pub = txi_pub[2] / (txi_pub[2] + txi_eng[2] + txi_pri[2]);
}
model {
  txi_pub ~ uniform(0, 0.1);
  txi_eng ~ uniform(0, 0.1);
  
  ppm ~ uniform(0, 1);

  p_pri_on_pub ~ beta(1.5, 3.5);
    
  for (i in 1:n_years) {
    target += binomial_lpmf(N_Txi_Pub[i] | Pop[i], txi_pub[i] / ppv_pub);
    target += binomial_lpmf(N_Txi_Eng[i] | Pop[i], txi_eng[i] / ppv_eng);
    
    target += normal_lpdf(Drug[i] | drug_time[i], Drug_Std[i]);
  }

  target += binomial_lpmf(Tx_Pub | Tx, p_pub);
  
}
generated quantities {
  real<lower=0, upper=1> p_under;
  real<lower=0> tp_pri_drug[n_years];
  real<lower=0> tp_pri_drug_time[n_years];
  real<lower=0> tp_pri_txi[n_years];
  
  for (i in 1:n_years) {
    tp_pri_drug[i] = (txi_eng[i] * (1 - p_pri_on_pub) + txi_pri[i]) * Pop[i];
    tp_pri_txi[i] = (txi_eng[i] + txi_pri[i]) * Pop[i];
    tp_pri_drug_time[i] = tp_pri_drug[i] * dur_pri;
    
    
  }
  p_under = txi_pri[2] / (txi_pub[2] + txi_eng[2] + txi_pri[2]);
}

