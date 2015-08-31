
# code to implement the Available Cases method (also called Pairwise
# Complete) for handling missing data

########################  linear regression  ##########################

# arguments:

# xy: data, with predictors in the first columns and the
#     response variable in the last column
# nboot:  if nonzero, this requests bootstrapped computation of the
#         estimated covariance matrix of the estimated vector of
#         regression coefficiengts

# value:  beta-hat vector; if boot nonzero, the cov. matrix is
#         included as the attribute 'cov'

lmac <- function(xy,nboot=0) {
   p1 <- ncol(xy)
   p <- p1 - 1
   tmp <- cov(xy,use='pairwise.complete.obs')
   upu <- tmp[1:p,1:p]
   upv <- tmp[1:p,p+1]
   bhat <- solve(upu,upv)
   # bhat0 <- mean(y) - colMeans(x) %*% bhat
   bhat0 <- colMeans(xy,na.rm=TRUE) %*% c(bhat,-1)
   bhat <- c(bhat0,bhat)
   if (nboot)  {
      n <- nrow(xy)
      bootonce <- function() {
         idxs <- sample(1:n,n,replace=TRUE)
         lmac(xy[idxs,],nboot=0)
      }
      bootout <- replicate(nboot,bootonce())
      attr(bhat,'cov') <- cov(t(bootout))
   }
   bhat
}

vcov <- function() {

}
