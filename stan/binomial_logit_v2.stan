data {
  // int<lower=2> nk; // number of categories
  int<lower=1> np; // number of predictors + intecept
  int<lower=0> ntrain; // number of observations
  int<lower=0> ntest; // number of observations
  
  int Ytrain[ntrain];  // response array
  int Ntrain[ntrain];  // trials
  matrix[ntrain, np] Xtrain; // predictors
  
  // int[ntest] Ytest;  // response test
  int Ntest[ntest];  // trials test
  matrix[ntest, np] Xtest; // predictors test
  
  // priors on regression coefficients
  vector[np] beta_priors_mn;
  vector[np] beta_priors_sd;

}
parameters {
  // real alpha; //intercept
  vector[np] betas; // intercept + slopes
}
model {
  // priors
  betas ~ normal(beta_priors_mn, beta_priors_sd);

  vector[ntrain] eta;
  eta = Xtrain * betas;
  
  // likelihood
  for (n in 1:ntrain){
      target += binomial_logit_lpmf(Ytrain[n] | Ntrain[n], eta[n]);
  }
}
generated quantities{ //predict responses test set
  int Ytest[ntest];  // response array
  vector[ntest] eta_test;
  eta_test = Xtest * betas;

  for (i in 1:ntest) {
    Ytest[i] = binomial_rng(Ntest[i], inv_logit(eta_test[i]));
  }
}

