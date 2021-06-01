
# under construction

##  # uniform wrapper for various dimension reduction methods, including
##  # predict() functions
##  
##  # no centering/scaling is done; user may do separately
##  
##  # example
##  
##  # tg <- ToothGrowth
##  # tg$supp <- as.numeric(tg$supp)
##  # tg <- as.matrix(tg)
##  # tgsvd <- dimRed(tg,method='svd',2)  # 2 PCs out of a possible 3
##  # newx <- c(8.8,1,0.5)
##  # dimRedNewX(tgsvd,newx)  # -8.860902 0.4095568, new coordinates
##  # tg1 <- reduceComps(tgsvd,1)  # go down to just 1 PC
##  # dimRedNewX(tg1,newx)  # -8.860902
##  
##  dimRed <- function(dat,method='prcomp',nComps) 
##  {
##     compSizes <- NULL  # eigenvalues etc.
##     if (method == 'prcomp') {
##        tmp <- prcomp(dat,center=FALSE,scale.=FALSE)
##        tmp$method <- 'prcomp'
##        tmp$rotation <- tmp$rotation[,1:nComps]
##     } else if (method == 'svd') {
##        tmp <- svd(dat,nu=nComps,nv=nComps)
##        tmp$method <- 'svd'
##        tmp$rotation <- tmp$v  # equiv to PCA $rotation
##     } else if (method == 'irlba') {
##        require(irlba)
##        tmp <- irlba(dat,nComps)
##        tmp$method <- 'irlba'
##        tmp$rotation <- tmp$v
##     } else stop('no such method')
##     tmp$compSizes <- compSizes
##     class(tmp) <- c('dimRed',class(tmp))
##     tmp
##  }
##  
##  # apply the same transformation to new X data
##  dimRedNewX <- function(object,newxs) 
##  {
##     method <- object$method
##     if (method == 'prcomp' || method == 'svd' || method == 'irlba') {
##        if (!is.matrix(newxs)) {
##           newxs <- as.matrix(newxs)
##           if (ncol(newxs) == 1) newxs <- t(newxs)
##        }
##        newxs %*% object$rotation
##     } 
##  }
##  
##  # ask for further reduction in the number of components
##  reduceComps <- function(object,nNewComps) 
##  {
##     method <- object$method
##     if (method == 'prcomp' || method == 'svd' || method == 'irlba') {
##        object$rotation <- object$rotation[,1:nNewComps]
##     }
##     object
##  }
##  
