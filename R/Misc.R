
#########################  unscale()  #################################

# undoes 'scale()'

# reverses scaling on scaledx, dividing its columns by sds and adding
# ctrs; if either of the latter 2 is NULL, it is obtained via attr(), on
# the assumption that scaledx was produced from x by scale() or similar

# returns the original x; if scaledx 

unscale <- function(scaledx,ctrs=NULL,sds=NULL) {
   if (is.null(ctrs)) ctrs <- attr(scaledx,'scaled:center')
   if (is.null(sds)) sds <- attr(scaledx,'scaled:scale')
   origx <- scaledx
   for (j in 1:ncol(scaledx)) {
      origx[,j] <- origx[,j] * sds[j]
      origx[,j] <- origx[,j] + ctrs[j]
   }
   origx
}

#################  convert between factors and dummies  ##################

# these routines are useful in that some regression packages insist that
# predictor be factors, while some require dummy variables

# for each column in dfr, if factor then replace by dummies,
# else just copy column; if omitLast, then dummy for last level of
# factor is not included in output

factorsToDummies <- function(dfr,omitLast=TRUE) 
{
   outDF <- data.frame(rep(0,nrow((dfr))))  # filler start
   for (i in 1:ncol(dfr)) {
      dfi <- dfr[,i]
      if (!is.factor(dfi)) {
         outDF <- cbind(outDF,dfi) 
         names(outDF)[ncol(outDF)] <- names(dfr)[i]
      } else {
            if (length(levels(dfi)) == 1) {
               msg <- paste(names(dfr)[i],'constant, not included')
               warning(msg)
               next
            }
         dumms <- factorToDummies(dfi,names(dfr)[i],omitLast)
         outDF <- cbind(outDF,dumms)
      }
   }
   outDF[,1] <- NULL  # delete filler
   outDF
}

# converts just a single factor; def of omitLast is in comments above;
# easier to have both f, fname required
factorToDummies <- function (f,fname,omitLast=TRUE) 
{
    n <- length(f)
    fl <- levels(f)
    ndumms <- length(fl) - omitLast
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == fl[i])
    if (omitLast) fl <-  fl[-(length(fl))]
    colnames(dms) <- paste(fname,'.', fl, sep = "")
    dms
}

# makes a factor dms from a single related set of dummies; returns a
# 1-col data frame; if the variable has k levels, inclLast = FALSE means
# there are only k-1 dummies provided; dms will have either k-1 or k
# columns

dummiesToFactor <- function(dms,inclLast=FALSE) {
   dms <- as.matrix(dms)
   lastCol <- 1 - apply(dms,1,sum)
   dms <- cbind(dms,lastCol)
   where1s <- apply(dms,1,function(rw) which(rw == 1))
   nms <- colnames(dms)
   f <- nms[where1s]
   as.factor(f)
}


