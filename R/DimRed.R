
# under construction

## # uniform wrapper for various dimension reduction methods, including
## # predict() functions
## 
## dimRed <- function(dat,method='prcomp',nComps) 
## {
##    compSizes <- NULL  # eigenvalues etc.
##    if (method == 'prcomp') {
##       tmp <- prcomp(dat)
##       tmp$method <- 'prcomp'
##       tmp$rotation <- tmp$rotation[,1:nComps]
##    } else if (method == 'svd') {
##       tmp <- svd(dat,nu=nComps,nv=nComps)
##       tmp$method <- 'svd'
##    } else if (method == 'nmf') {
##       require(NMF)
##    } else stop('no such method')
##    tmp$compSizes <- compSizes
##    class(tmp) <- c('dimRed',class(tmp))
##    tmp
## }
## 
## # apply the same transformation to new X data
## dimRecNewX <- function(object,newxs) 
## {
##    method <- object$method
##    if (method == 'prcomp') {
##       predict(object,newxs)
##    } 
## }
## 
## # ask for further reduction in the number of components
## reduceComps <- function(object,nNewComps) 
## {
##    method <- object$method
##    if (method == 'prcomp') {
##       object$rotation <- object$rotation[,1:nNewComps]
##    }
##    object
## }
## 
