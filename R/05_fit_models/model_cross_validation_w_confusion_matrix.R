# Confusion matrix from mean-only cross validation. 
# We are only doing a single hold-out set here so this could be updated to multiple hold-out sets (k-fold) or block sets if that was of interest.
# import libraries for extracting data from stan
library(rstan)

## Function for creating a confusion matrix (assumes inputs are integer classes or factors, e.g. class 1, 2 and 3)
ctable = function(...) {
  tab = table(...)
  class(tab) = c("confusion.table", class(tab))
  tab
}

## produce confusion matrix for binomial reef model.
## binomial
dat <- readRDS("/home/woo457/Dropbox/nesp-2.1/data/stan_data/stan_binomial_w_cv.rds")
Ytest <- dat$Ytest/dat$Ntest
Ytest_class <- ifelse(Ytest>0.5,2,1)
modfit <- readRDS("/home/woo457/Dropbox/nesp-2.1/model_outputs/stan_binomial_200324_run2.rds")
e <- extract(modfit)
Ypred <- apply(e$Ytest,c(2),mean)/dat$Ntest
Ypred_class <- ifelse(Ypred>0.5,2,1)

classes <- c("non-reef","reef")
predfact <- factor(classes[Ypred_class],levels = classes)
class <- factor(classes[Ytest_class],levels = classes)

confuse_mat <- lapply(data.frame(class, stringsAsFactors = FALSE, 
                                 check.names = FALSE), function(x, y) ctable(True = x, 
                                                                             Predicted = y), y = predfact)
confuse_mat$class
rownames(confuse_mat$class) <- paste0("True ",classes)
colnames(confuse_mat$class) <- paste0("Predicted ",classes)
write.csv(confuse_mat$class,"model_outputs/cross_validation/binomial_cv.csv")

con_mat <- confuse_mat$class
sapply(1:2,function(i)precision = con_mat[i,i]/(con_mat[i,i]+sum(con_mat[i,-i])))


## produce confusion matrix for multi class ecosystem model. 
## multinomial
dat <- readRDS("/home/woo457/Dropbox/nesp-2.1/data/stan_data/stan_multinomial_5class_w_cv.rds")
Ytest <- dat$Ytest
modfit <- readRDS("/home/woo457/Dropbox/nesp-2.1/model_outputs/stan_multinomial_v2_w_cv_200224_run2.rds")
e <- rstan::extract(modfit)
Ypred <- apply(e$Ytest,c(2,3),mean)

# Ytest
binary_fun <- function(y){
row_maxes <- apply(y, 1, max)
binary_matrix <- y == row_maxes
binary_matrix[] <- as.integer(binary_matrix)
binary_matrix
}

Predbin <- binary_fun(Ypred)
Ybin <- binary_fun(Ytest)
colnames(Predbin) <- colnames(Ybin) <- colnames(Ytest)

pred <- Predbin %*% (1:5)
y  <- Ybin %*% (1:5)

predfact <- factor(colnames(Ytest)[pred],levels=colnames(Ytest))
class <- factor(colnames(Ytest)[y],levels=colnames(Ytest))

confuse_mat <- lapply(data.frame(class, stringsAsFactors = FALSE, 
                  check.names = FALSE), function(x, y) ctable(True = x, 
                                                              Predicted = y), y = predfact)
confuse_mat$class

con_mat <- confuse_mat$class[1:5,1:5]
rownames(con_mat) <- gsub("broad.","Predicted ",rownames(con_mat))
colnames(con_mat) <- gsub("broad.","True ",colnames(con_mat))
con_mat <- rbind(con_mat,colSums(con_mat))
con_mat <- cbind(con_mat,c(sapply(1:5,function(i)precision = con_mat[i,i]/(con_mat[i,i]+sum(con_mat[i,-i]))),NA))

write.csv(con_mat,"model_outputs/cross_validation/multinomial_cv.csv")

