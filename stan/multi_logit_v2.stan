functions {
  /* multinomial-logit log-PMF
   * Args:
   *   y: array of integer response values
   *   mu: vector of category logit probabilities
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real multinomial_logit2_lpmf(int[] y, vector mu) {
     return multinomial_lpmf(y | softmax(mu));
   }

}


data {
  int<lower=2> nk; // number of categories
  int<lower=2> np; // number of predictors
  int<lower=0> ntrain; // number of observations
  int<lower=0> ntest; // number of observations
  matrix[ntrain, np] Xtrain; // predictors
  array[ntrain, nk] int Ytrain;  // response array
  matrix[ntest, np] Xtest; // predictors
  array[ntest] int Ytest_trails;  // response array

}
transformed data {
  row_vector[np] zeros = rep_row_vector(0, np);
}
parameters {
  matrix[nk-1, np] beta_raw; // slopes
}
transformed parameters {
  matrix[nk, np] beta;
  beta = append_row(beta_raw, zeros);
  // real lprior = 0;
}
model {
  matrix[ntrain, nk] eta;
  eta = Xtrain * beta';
  // for (n in 1:nk)
     // print("eta = ", eta[n]');
  // prior
  to_vector(beta) ~ normal(0, 10);
  
  // likelihood
  for (n in 1:ntrain) {
      target += multinomial_logit2_lpmf(Ytrain[n] | eta[n]');
  }
}

generated quantities{ //predict responses test set
  array[ntest, nk] int Ytest;  // response array
	for(i in 1:ntest){
		Ytest[i] = multinomial_logit_rng(to_vector(Xtest[i,] * beta'), Ytest_trails[i]);
	}
}
