
# code to implement the Available Cases method (also called Pairwise
# Complete) for handling missing data

########################  linear regression  ##########################

# arguments:

# xy: data, with predictors in the first columns and the
#     response variable in the last column
# nboot:  if nonzero, this requests bootstrapped computation of the
#         estimated covariance matrix of the estimated vector of
#         regression coefficients

# value:  an object of class 'lmac', with components 
#
#         coefficients:  estimated regression coefficients
#         fitted.values:  est. regression ftn. values at 
#                         complete cases (but with full coefs.)
#         residuals:  residuals at complete cases (but with full coefs.)
#         r2:  R-squared
#         cov:  optional est. covariance matrix of the coefs.

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
   bhat0 <- colMeans(xy,na.rm=TRUE) %*% c(-bhat,1)
   bhat <- c(bhat0,bhat)
   lmacout$coefficients <- bhat
   xycc <- na.omit(xy)
   yhat <- cbind(1,xycc[,-p1]) %*% bhat
   lmacout$fitted.values <- yhat
   lmacout$residuals <- xycc[,p1] - yhat
   lmacout$r2 <- (cor(yhat,xycc[,p1]))^2
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

coef.lmac <- function(object,...) {
   object$coefficients
}

vcov.lmac <- function(object,...) {
   object$cov
}

#############################  PCA  ###############################

#  arguments:
#
#     indata: data frame or matrix
#
#  value: list with components 'values' and 'vectors', as with eigen()

pcac <- function(indata,scale=FALSE) {
   covcor <- if(scale) cor else cov
   cvr <- covcor(indata,use='pairwise.complete.obs')
   tmp <- eigen(cvr)
   res <- list()
   if (any(tmp$values < 0)) 
      stop('at least one negative eigenvalue')
   res$sdev <- sqrt(tmp$values)
   res$rotation <- tmp$vectors
   res
}

######################  log-linear model`  ##########################

# log-linear model; at present, handles only the 3-factor casea
#
# arguments:
#
#    x: data frame/matrix, one row per observation; use tbltofakedf()
#       if data is in table form
#    margin: a list of vectors specifying the model, 
#            as in loglin()
#
# value:  $param and $fit components in the value emitted from R's loglin()

loglinac <- function(x,margin) {
   # find lengths of the elements in the model, to determine what
   # situtation we are in
   termlengths <- Map(length,margin)
   n1 <- sum(termlengths == 1)  # singletons
   n2 <- sum(termlengths == 2)  # 2-way interactions
   # mdlf() ("model function") will find the right cell means 
   # for the specified 'margin'
   # fully independent?
   if (n1 == 3) mdlf <- mindep else
      # one var. independent of the other 2?
      if (n2 == 1) mdlf <- mxindyz else
      # 2 vars. conditionally independent, given the 3rd?
      if (n2 == 2) mdlf <-myzcondindx else
      # case of all possible 2-way interactions not implemented, for
      # lack of a closed-form solution
      stop('case of all 2-way terms not implemented')
   # need an appropriate shell, with the right dimensions, labels etc.;
   # the contents here are irrelevant and will be overwritten
   x <- as.data.frame(x)
   tbl <- table(x)
   tbl <- mdlf(x,margin,tbl,termlengths)
   loglin(tbl,margin,param=TRUE,fit=TRUE)
}

# fully independent case
mindep <- function(x,margin,tbl,termlengths) {
   nc <- ncol(x)  # currently must be 3
   probs <- list()
   # find number of distinct values found in each variable, and the
   # estimated marginal probabilities of each value
   nvals <- vector(length=nc)
   for (i in 1:nc) {
      tmp <- table(x[,i])
      probs[[i]] <- tmp / sum(tmp)
      nvals[i] <- length(tmp)
   }
   # now find estimated cell probabilities
   for (i in 1:nvals[1]) 
      for (j in 1:nvals[2]) 
         for (k in 1:nvals[3]) {
            tbl[i,j,k] <- 
               probs[[1]][i] *
               probs[[2]][j] *
               probs[[3]][k] 
         }
    # convert to estimated expected cell counts
    tbl <- nrow(x) * tbl
}

# case of 1 variable, X, being independent of the other 2, Y and Z
mxindyz <- function(x,margin,tbl,termlengths) {
   # which ones are Y and Z?
   iyz <- margin[[1]]
   nc <- ncol(x)  # 3
   # which variable is X?
   ix <- setdiff((1:nc),iyz)
   # find number of distinct values found in each variable, and the
   # estimated marginal probabilities of each value
   probs <- list()
   nvals <- vector(length=nc)
   nvals[1] <- length(table(x[,ix]))
   nvals[2] <- length(table(x[,iyz[1]]))
   nvals[3] <- length(table(x[,iyz[2]]))
   tmp <- table(x[,ix])
   probs[[1]] <- tmp / sum(tmp)
   tmp <- table(x[,iyz])
   probs[[2]] <- tmp / sum(tmp)
   for (i in 1:nvals[1]) 
      for (j in 1:nvals[2]) 
         for (k in 1:nvals[3]) {
            if (ix == 1) {
            tbl[i,j,k] <- 
               probs[[1]][i] *
               probs[[2]][j,k] 
            } else if (ix == 2) {
               tbl[i,j,k] <- 
                  probs[[1]][j] *
                  probs[[2]][i,k] 
            } else {  # ix = 3
               tbl[i,j,k] <- 
                  probs[[1]][k] *
                  probs[[2]][i,j] 
            }
         }
    tbl <- nrow(x) * tbl
}

# case of 2 variables being conditionally independent, given the 3rd
myzcondindx <- function(x,margin,tbl,termlengths) {
   # which variable is X?
   ix <- intersect(margin[[1]],margin[[2]])
   # which ones are Y and Z?
   iyz <- setdiff(union(margin[[1]],margin[[2]]),ix)
   iy <- iyz[1]
   iz <- iyz[2]
   # easier to keep track of all if iy < iz
   if (iy > iz) {
      tmp <- iz
      iz <- iy
      iy <- tmp
   }
   nc <- ncol(x)  # currently 3
   # find number of distinct values found in each variable, and the
   # estimated marginal probabilities of each value
   probs <- list()
   nvals <- vector(length=nc)
   # nvals[1] <- length(table(x[,ix]))
   # nvals[2] <- length(table(x[,iy]))
   # nvals[3] <- length(table(x[,iz]))
   nvals[ix] <- length(table(x[,ix]))
   nvals[iy] <- length(table(x[,iy]))
   nvals[iz] <- length(table(x[,iz]))
   tmp <- table(x[,ix])
   probs[[1]] <- tmp / sum(tmp)
   tmp <- table(x[,c(ix,iy)])
   probs[[2]] <- tmp / sum(tmp)
   tmp <- table(x[,c(ix,iz)])
   probs[[3]] <- tmp / sum(tmp)
   for (i in 1:nvals[1]) 
      for (j in 1:nvals[2]) 
         for (k in 1:nvals[3]) {
            if (ix == 1) {
               tbl[i,j,k] <- 
                  probs[[3]][i,k] *
                  probs[[2]][i,j] /
                  probs[[1]][i] 
            } else if (ix == 2) {
               tbl[i,j,k] <- 
                  probs[[3]][j,k] *
                  probs[[2]][j,i] /
                  probs[[1]][j] 
               
            } else {  # ix == 3
               tbl[i,j,k] <- 
                  probs[[3]][k,j] *
                  probs[[2]][k,i] /
                  probs[[1]][k] 
            }
         }
    tbl <- nrow(x) * tbl
}

# converts an R table to a fake data frame; the number of rows will be
# the number of cases in the table, i.e. sum(tbl), and the number of
# columns will be the dimension of the table, i.e. length(dim(tbl));
# if a cell has frequency k, it will appear k times in the output
tbltofakedf <- function(tbl) {
   adf <- as.data.frame(tbl)
   nc <- ncol(adf)
   onecell <- function(adfrow) {
      freq <- as.numeric(adfrow[nc])
      if (freq == 0) return(NULL)
      remainingrow <- adfrow[-nc]
      matrix(rep(remainingrow,freq),byrow=TRUE,nrow=freq)
   }
   m <- Reduce(rbind,apply(adf,1,onecell))
   as.data.frame(m)
}

#############################  misc.  ###############################

# for testing purposes; randomly replacing each element of matrix m by 

makeNA <- function(m,probna) {
   if (!is.matrix(m)) stop('m must be a matrix')
   n <- length(m)
   nmiss <- rbinom(1,n,probna)
   naidxs <- sample(1:n,nmiss,replace=FALSE)
   m[naidxs] <- NA
   m
}
