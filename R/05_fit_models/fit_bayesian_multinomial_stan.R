library(terra)
library(rstan)

habdat <- read.csv("data/tidy/NESP-2.1_broad-habitat_skip_edits_v2.csv")
Y <- subset(habdat,select=c("broad.macroalgae","broad.seagrass","broad.unconsolidated","broad.consolidated","broad.sessile.inverts"))
rscaled <-rast( "data/spatial/rasters/NESP-2.1_preds_v2.tif")

covars <- terra::extract(rscaled,habdat[,3:4])[,-1]
# covars <- terra::extract(rscaled,habdat[,3:4],method='bilinear',na.rm=TRUE)[,-1]
missing_data_idx <- which(!complete.cases(covars))
covars_clean <- covars[-missing_data_idx,]
# covars_clean$longitude <- covars_clean$longitude*2

scaled_covars <- scale(covars_clean)
covars_clean <- as.data.frame(scaled_covars)


form <- ~ poly(longitude,degree=2,raw=TRUE) +
  poly(latitude,degree=2,raw=TRUE) +
  poly(depth,2,raw=TRUE) +
  poly(slope,2,raw=TRUE) +
  # poly(aspect,2,raw=TRUE) +
  poly(TPI,2,raw=TRUE) +
  poly(TRI,2,raw=TRUE) +
  # poly(detrended,2,raw=TRUE) +
  poly(bottomstress_mn,2,raw=TRUE)#+
# poly(bottomstress_lintrend,2,raw=TRUE)

mf <- model.frame(form, covars_clean)
mt <- terms(mf)
Xin <- model.matrix(mt,mf)
Yin <- as.matrix(Y[-missing_data_idx,])

## seagrass sites
# seagrass_sites <- which(Yin[,2]>0)
# Yin2 <- Yin[-seagrass_sites,-2]
# rowSums(Yin2)
# Xin2 <- Xin[-seagrass_sites,]

# zero_class_idx <- as.numeric(which(rowSums(Yin)==0))
# Yin <- Yin[-zero_class_idx,]
# Xin <- Xin[-zero_class_idx,]

# write.csv(Yin,"response.csv")
# write.csv(Xin,"model_matrix.csv")
# write.csv(covars_clean,"covariates.csv")
# write.csv(habdat,"site_data.csv")

## compare to multinomial
nnmlt <- nnet::multinom(as.matrix(Yin)~as.matrix(Xin)-1,maxit = 1000)
coef(nnmlt)
## fit stan
nk <- ncol(Yin);  # classes
np <- ncol(Xin);  # predictors including intercept
nobs <- nrow(Yin); # observations 

set.seed(0)
train_test_split <- sample(c(TRUE, FALSE), nrow(Yin), replace = TRUE, prob = c(0.8, 0.2))

dat <- list(nk=nk,
            np=np,
            ntrain=nrow(Yin[train_test_split,]),
            ntest=nrow(Yin[!train_test_split,]),
            Ytrain=Yin[train_test_split,],
            Xtrain=Xin[train_test_split,],
            Ytest=Yin[!train_test_split,],
            Ytest_trails=rowSums(Yin[!train_test_split,]),
            Xtest=Xin[!train_test_split,])

saveRDS(dat,file = "data/stan_data/stan_multinomial_5class_w_cv.rds")

# fit <- stan("stan/multi_logit_v2.stan", data=dat, chains=3, iter=5000, cores = 3, control = list(max_treedepth=20))
# saveRDS(fit,file = "model_outputs/stan_multinomial_v2_w_cv_200224_run3.rds")
fit <- readRDS("model_outputs/stan_multinomial_v2_w_cv_200224_run2.rds")
# plot(fit)
# pairs(fit)

# fit <- readRDS("model_outputs/stan_multinomial_v2_w_cv_160224_run3.rds")
betas <- matrix(get_posterior_mean(fit,"beta")[,4],nk,np,byrow=TRUE)
# plot(fit,pars=paste0("beta_raw[1,",1:13,"]"))
# plot(fit,pars=paste0("beta_raw[2,",1:13,"]"))
# plot(fit,pars=paste0("beta_raw[3,",1:13,"]"))
# plot(fit,pars=paste0("beta_raw[4,",1:13,"]"))

print(fit)

softmax <- function(linpred){
  if(is.null(dim(linpred))) linpred <- matrix(linpred,ncol= length(linpred))
  exp(linpred)/apply(linpred,1, function(x) sum(exp(x)))
}


vec2mat_param <- function(params,np){
  matrix(params,ncol=np)
}

predfun <- function(model,data){
  tmat <- t(vec2mat_param(model,np=ncol(data)+1))
  tmp_eta <- cbind(1,as.matrix(data))%*%tmat
  pis <- t(apply(tmp_eta,1,softmax))
  pis
}

predfun_parallel <- function(model, data) {
  ncls <- length(cls)
  nr <- nrow(data)
  s <- split(data, rep(1:ncls, each=ceiling(nr/ncls), length.out=nr))
  out <- parallel::parLapply(cls, s, function(x) predfun(model, x))
  do.call(rbind,out)
}

# generate covariates for prediction each time you change model formula.
# mns <- attributes(scaled_covars)$`scaled:center`
# sds <- attributes(scaled_covars)$`scaled:scale`
# 
# covar_names <- c("longitude","latitude","depth","slope","TPI","TRI","bottomstress_mn")
# rscaled[[covar_names]]
# 
# rscaled <- rast( "data/spatial/rasters/NESP-2.1_preds_v3.tif")
# rcovars_scaled <- lapply(covar_names,function(x)((rscaled[[x]]-mns[x])/sds[x]))
# rcovars_scaled2 <- lapply(rcovars_scaled,function(x){out <- c(x,x^2);names(out)[2]<-paste0(names(out)[1],".2");return(out)})
# rcovars_in <- do.call(c,rcovars_scaled2)
writeRaster(rcovars_in,"data/spatial/rasters/multinomial_pred_rasters.tif")

library(parallel)
# cls <- parallel::makeCluster(8)
# parallel::clusterExport(cls, c("vec2mat_param", "predfun","predfun_parallel","betas","softmax"))
rpred_class_probs_mn <- predict(rcovars_in,betas,fun=predfun)#,filename="model_outputs/predictions/model2_preds.tif")
# parallel::stopCluster(cls)

plot(rpred_class_probs_mn)
names(rpred_class_probs_mn)<-colnames(Y)
writeRaster(rpred_class_probs_mn,filename = "model_outputs/predictions/multinomial_preds_mns_220224.tif")

## quantiles
betas_samples <- rstan::extract(object = fit, pars="beta")
beta_quants <- lapply(c(0.025,0.25,0.5,0.75,0.975),function(x)apply(betas_samples$beta,c(2,3),quantile,x))

## lower quartile
betas <- beta_quants[[2]]
# cls <- parallel::makeCluster(8)
# parallel::clusterExport(cls, c("vec2mat_param", "predfun","predfun_parallel","betas","softmax"))
rpred_class_probs_lwr_qrt <- predict(rcovars_in,betas,fun=predfun)
# parallel::stopCluster(cls)

plot(rpred_class_probs_lwr_qrt)
names(rpred_class_probs_lwr_qrt)<-colnames(Y)
writeRaster(rpred_class_probs_lwr_qrt,filename = "model_outputs/predictions/multinomial_preds_lwr_qrt_220224.tif")

## upper quartile
betas <- beta_quants[[4]]
# cls <- parallel::makeCluster(8)
# parallel::clusterExport(cls, c("vec2mat_param", "predfun","predfun_parallel","betas","softmax"))
rpred_class_probs_upr_qrt <- predict(rcovars_in,betas,fun=predfun)
# parallel::stopCluster(cls)

plot(rpred_class_probs_upr_qrt)
names(rpred_class_probs_upr_qrt)<-colnames(Y)
writeRaster(rpred_class_probs_upr_qrt,filename = "model_outputs/predictions/multinomial_preds_upr_qrt_220224.tif")

rpred_class_probs_error <- abs(rpred_class_probs_lwr_qrt-rpred_class_probs_upr_qrt) 
writeRaster(rpred_class_probs_error,filename = "model_outputs/predictions/multinomial_preds_error_220224.tif")

