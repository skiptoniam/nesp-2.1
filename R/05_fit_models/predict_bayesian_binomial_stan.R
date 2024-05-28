library(rstan)
## Extract the posterior estimates from the binomial model
fit2 <- readRDS("model_outputs/stan_binomial_200324_run2.rds")
betas_samples <- rstan::extract(object = fit2,pars="betas")
betas <- apply(betas_samples$betas,c(2),quantile,0.5)

predfun <- function(model,data){
  tmp_eta <- cbind(1,as.matrix(data))%*%as.vector(model)
  pis <- plogis(tmp_eta)
  pis
}

## if you want to run in parallel (I don't use this as the moment)
predfun_parallel <- function(model, data) {
  ncls <- length(cls)
  nr <- nrow(data)
  s <- split(data, rep(1:ncls, each=ceiling(nr/ncls), length.out=nr))
  out <- parallel::parLapply(cls, s, function(x) predfun(model, x))
  do.call(rbind,out)
}

## set up the covariates for prediction do this once
habdat <- read.csv("data/tidy/NESP-2.1_broad-habitat_skip_edits_v2.csv")
Y <- subset(habdat,select=c("broad.macroalgae","broad.seagrass","broad.unconsolidated","broad.consolidated","broad.sessile.inverts"))
rscaled <-rast( "data/spatial/rasters/NESP-2.1_preds_v2.tif")

covars <- terra::extract(rscaled,habdat[,3:4])[,-1]
missing_data_idx <- which(!complete.cases(covars))
covars_clean <- covars[-missing_data_idx,]

scaled_covars <- scale(covars_clean)
covars_clean <- as.data.frame(scaled_covars)
mns <- attributes(scaled_covars)$`scaled:center`
sds <- attributes(scaled_covars)$`scaled:scale`
 
covar_names <- c("longitude","latitude","depth","slope","aspect","TPI","TRI","bottomstress_mn")
rscaled[[covar_names]]

rscaled <- rast( "data/spatial/rasters/NESP-2.1_preds_v3.tif")
rcovars_scaled <- lapply(covar_names,function(x)((rscaled[[x]]-mns[x])/sds[x]))
rcovars_scaled2 <- lapply(rcovars_scaled,function(x){out <- c(x,x^2);names(out)[2]<-paste0(names(out)[1],".2");return(out)})
rcovars_in <- do.call(c,rcovars_scaled2)
writeRaster(rcovars_in,"data/spatial/rasters/reef_pred_rasters.tif")
# rcovars_in <- rast("data/spatial/rasters/reef_pred_rasters.tif")

## Predict the reef means
rpred_reef_probs_mn <- predict(rcovars_in,betas,fun=predfun)
plot(rpred_reef_probs_mn)
names(rpred_reef_probs_mn) <- "Reef"
writeRaster(rpred_reef_probs_mn,filename = "model_outputs/predictions/binomial_preds_reef_mns_220324.tif",overwrite=TRUE)

## quantiles
betas_samples <- rstan::extract(object = fit2,pars="betas")
beta_quants <- apply(betas_samples$betas,c(2),quantile,c(0.025,0.25,0.5,0.75,0.975))

## lower quartile
betas <- beta_quants[2,]
rpred_reef_probs_lwr_qrt <- predict(rcovars_in,betas,fun=predfun)# parallel::stopCluster(cls)

plot(rpred_reef_probs_lwr_qrt)
names(rpred_reef_probs_lwr_qrt) <- "Reef"
writeRaster(rpred_reef_probs_lwr_qrt,filename = "model_outputs/predictions/binomial_preds_reef_lwr_qrt_220324.tif",overwrite=TRUE)

## upper quartile
betas <- beta_quants[4,]
rpred_reef_probs_upr_qrt <- predict(rcovars_in,betas,fun=predfun)

plot(rpred_reef_probs_upr_qrt)
names(rpred_reef_probs_upr_qrt) <- "Reef"
writeRaster(rpred_reef_probs_upr_qrt,filename = "model_outputs/predictions/binomial_preds_reef_upr_qrt_220324.tif")

## Compute the error as the absolute difference between the lower and upper quartiles
rpred_reef_probs_error <- abs(rpred_reef_probs_lwr_qrt-rpred_reef_probs_upr_qrt) 
plot(rpred_reef_probs_error)
writeRaster(rpred_class_probs_error,filename = "model_outputs/predictions/binomial_preds_error_220324.tif")

