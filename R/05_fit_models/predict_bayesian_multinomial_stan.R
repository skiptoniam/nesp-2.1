library(rstan)
library(terra)

fit <- readRDS("model_outputs/stan_multinomial_v2_w_cv_200224_run2.rds")
betas <- matrix(get_posterior_mean(fit,"beta")[,4],nk,np,byrow=TRUE)

## link function for the multinomial model
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
habdat <- read.csv("data/tidy/NESP-2.1_broad-habitat_skip_edits_v2.csv")
Y <- subset(habdat,select=c("broad.macroalgae","broad.seagrass","broad.unconsolidated","broad.consolidated","broad.sessile.inverts"))
rscaled <-rast( "data/spatial/rasters/NESP-2.1_preds_v2.tif")
covars <- terra::extract(rscaled,habdat[,3:4])[,-1]
missing_data_idx <- which(!complete.cases(covars))
covars_clean <- covars[-missing_data_idx,]

scaled_covars <- scale(covars_clean)

mns <- attributes(scaled_covars)$`scaled:center`
sds <- attributes(scaled_covars)$`scaled:scale`

covar_names <- c("longitude","latitude","depth","slope","TPI","TRI","bottomstress_mn")
rscaled[[covar_names]]

rscaled <- rast( "data/spatial/rasters/NESP-2.1_preds_v3.tif")
rcovars_scaled <- lapply(covar_names,function(x)((rscaled[[x]]-mns[x])/sds[x]))
rcovars_scaled2 <- lapply(rcovars_scaled,function(x){out <- c(x,x^2);names(out)[2]<-paste0(names(out)[1],".2");return(out)})
rcovars_in <- do.call(c,rcovars_scaled2)
writeRaster(rcovars_in,"data/spatial/rasters/multinomial_pred_rasters.tif")


## predict the mean prob
rpred_class_probs_mn <- predict(rcovars_in,betas,fun=predfun)#,filename="model_outputs/predictions/model2_preds.tif")
plot(rpred_class_probs_mn)
names(rpred_class_probs_mn)<-colnames(Y)
writeRaster(rpred_class_probs_mn,filename = "model_outputs/predictions/multinomial_preds_mns_220224.tif")

## quantiles
betas_samples <- rstan::extract(object = fit, pars="beta")
beta_quants <- lapply(c(0.025,0.25,0.5,0.75,0.975),function(x)apply(betas_samples$beta,c(2,3),quantile,x))

## lower quartile
betas <- beta_quants[[2]]
rpred_class_probs_lwr_qrt <- predict(rcovars_in,betas,fun=predfun)

plot(rpred_class_probs_lwr_qrt)
names(rpred_class_probs_lwr_qrt)<-colnames(Y)
writeRaster(rpred_class_probs_lwr_qrt,filename = "model_outputs/predictions/multinomial_preds_lwr_qrt_220224.tif")

## upper quartile
betas <- beta_quants[[4]]
rpred_class_probs_upr_qrt <- predict(rcovars_in,betas,fun=predfun)

plot(rpred_class_probs_upr_qrt)
names(rpred_class_probs_upr_qrt)<-colnames(Y)
writeRaster(rpred_class_probs_upr_qrt,filename = "model_outputs/predictions/multinomial_preds_upr_qrt_220224.tif")

# error absolute interquantile range
rpred_class_probs_error <- abs(rpred_class_probs_lwr_qrt-rpred_class_probs_upr_qrt) 
writeRaster(rpred_class_probs_error,filename = "model_outputs/predictions/multinomial_preds_error_220224.tif")
