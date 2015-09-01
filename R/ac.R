
# code to implement the Available Cases method (also called Pairwise
# Complete) for handling missing data

########################  linear regression  ##########################

# arguments:

# xy: data, with predictors in the first columns and the
#     response variable in the last column
# nboot:  if nonzero, this requests bootstrapped computation of the
#         estimated covariance matrix of the estimated vector of
#         regression coefficiengts

# value:  an object of class 'lmac', with components 'coefficients' and
# 'cov', i.e. beta-hat and its estimated covariance matrix

lmac <- function(xy,nboot=0) {
   p1 <- ncol(xy)
   p <- p1 - 1
   tmp <- cov(xy,use='pairwise.complete.obs')
   upu <- tmp[1:p,1:p]
   upv <- tmp[1:p,p+1]
   bhat <- solve(upu,upv)
   lmacout <- list()
   class(lmacout) <- 'lmac'
   # bhat0 <- mean(y) - colMeans(x) %*% bhat
   bhat0 <- colMeans(xy,na.rm=TRUE) %*% c(bhat,-1)
   bhat <- c(bhat0,bhat)
   lmacout$coefficients <- bhat
   if (nboot > 0)  {
      n <- nrow(xy)
      bootonce <- function() {
         idxs <- sample(1:n,n,replace=TRUE)
         lmac(xy[idxs,],nboot=0)$coefficients
      }
      bootout <- replicate(nboot,bootonce())
      lmacout$cov<- cov(t(bootout))
   }
   lmacout
}

coef.lmac <- function(lmacout) {
   lmacout$coefficients
}

vcov.lmac <- function(lmacout) {
   lmacout$cov
}
