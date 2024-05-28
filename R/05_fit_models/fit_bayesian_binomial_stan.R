library(terra)
library(rstan)

habdat <- read.csv("data/tidy/NESP-2.1_broad-habitat_skip_edits_v2.csv")
Y <- subset(habdat,select=c("broad.macroalgae","broad.seagrass","broad.unconsolidated","broad.consolidated","broad.sessile.inverts"))
rscaled <-rast( "data/spatial/rasters/NESP-2.1_preds_v2.tif")

covars <- terra::extract(rscaled,habdat[,3:4])[,-1]
missing_data_idx <- which(!complete.cases(covars))
covars_clean <- covars[-missing_data_idx,]

scaled_covars <- scale(covars_clean)
covars_clean <- as.data.frame(scaled_covars)

form <- ~ poly(longitude,degree=2,raw=TRUE) +
  poly(latitude,degree=2,raw=TRUE) +
  poly(depth,2,raw=TRUE) +
  poly(slope,2,raw=TRUE) +
  poly(aspect,2,raw=TRUE) +
  poly(TPI,2,raw=TRUE) +
  poly(TRI,2,raw=TRUE) +
  # poly(detrended,2,raw=TRUE) +
  poly(bottomstress_mn,2,raw=TRUE)#+
# poly(bottomstress_lintrend,2,raw=TRUE)

mf <- model.frame(form, covars_clean)
mt <- terms(mf)
Xin <- model.matrix(mt,mf)
Yin <- as.matrix(Y[-missing_data_idx,])

Nin <- rowSums(Yin)
Reefin <- rowSums(subset(Yin,select=c(broad.macroalgae,broad.consolidated,broad.sessile.inverts)))

## compare to glm 
# test_glm <- glm(cbind(Reefin,Nin)~ -1 +  Xin, family = binomial())
# coef(test_glm)

# fit stan
np <- ncol(Xin);  # predictors including intercept
nobs <- nrow(Yin); # observations 

beta_priors_mn <- as.numeric(rep(0,ncol(Xin)))
beta_priors_sd <- as.numeric(rep(10,ncol(Xin)))#c(10,2.5 * apply(Xin[,-1], 2, sd)))

set.seed(0)
train_test_split <- sample(c(TRUE, FALSE), nrow(Yin), replace = TRUE, prob = c(0.8, 0.2))

dat <- list(np=np,
            ntrain=nrow(Yin[train_test_split,]),
            ntest=nrow(Yin[!train_test_split,]),
            Ytrain=as.integer(Reefin[train_test_split]),
            Ntrain=as.integer(Nin[train_test_split]),
            Xtrain=Xin[train_test_split,],
            Ytest=as.integer(Reefin[!train_test_split]),
            Ntest=as.integer(Nin[!train_test_split]),
            Xtest=Xin[!train_test_split,],
            beta_priors_mn = beta_priors_mn,
            beta_priors_sd = beta_priors_sd)

saveRDS(dat,file="data/stan_data/stan_binomial_w_cv_220324.rds")

fit2 <- stan("stan/binomial_logit_v2.stan", data=dat, chains=3, iter=5000,cores = 3)
saveRDS(fit2,file = "model_outputs/stan_binomial_200324_run2.rds")


