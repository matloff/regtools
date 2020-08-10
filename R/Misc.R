
#########################  scaling  #################################

# undoes R 'scale()'

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

# scale to [0,1]

# arguments:

#    m: a vector or matrix
#    scalePars: if not NULL, a 2-row matrix, with column storing
#       the min and max values to be used in scaling column i of m

# value: a matrix, with column i consisting of the scaled version
#    of column i of m, and attribute as in scalePars (either copied from
#    the latter or if null, generated fresh

mmscale <- function (m,scalePars=NULL)
{
   if (is.vector(m)) m <- matrix(m,ncol=1)
   if (is.null(scalePars)) {
      rngs <- apply(m,2,range)
      mins <- rngs[1,]
      maxs <- rngs[2,]
   } else {
      mins <- scalePars[1,]
      maxs <- scalePars[2,]
      rngs <- scalePars
   }
   ranges <- maxs - mins
   tmm <- function(i) m[,] <- (m[,i] - mins[i]) / ranges[i]
   m <- sapply(1:ncol(m),tmm)
   attr(m,'minmax') <- rngs
   m
}

################### factors and dummy variables########################

# x is a data frame; returns TRUE if at least one column is a factor
hasFactors <- function(x) 
{
   for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) return(TRUE)
   }
   FALSE
}

# these routines are useful in that some regression packages insist that
# predictor be factors, while some require dummy variables

# for each column in dfr, if factor then replace by dummies,
# else just copy column; if omitLast, then dummy for last level of
# factor is not included in output

# a key point is that, for later prediction after fitting a model, one
# needs to use the same transformations; otherwise, the data to be
# predicted may be missing a level of some factor; this of course is
# especially true if one is predicting a single case

# thus the factor names and levels are save in attributes, and can be
# used as input, via factorsInfo

# arguments

#    dfr: a data frame
#    omitLast: if TRUE, make m-1 dummies for an m-level factor
#    factorsInfo: use factor levels found earlier
#    dfOut: if TRUE, output a data frame rather than a matrix

factorsToDummies <- function(dfr,omitLast=FALSE,factorsInfo=NULL,
   dfOut=FALSE)
{
   if (!is.null(factorsInfo)) stop('factorsInfo not yet implemented')
   if (is.factor(dfr)) dfr <- as.data.frame(dfr)
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
         dumms <- factorToDummies(dfi,names(dfr)[i],omitLast=omitLast)
         outDF <- cbind(outDF,dumms)
      }
   }
   outDF[,1] <- NULL  # delete filler
   if (!dfOut) as.matrix(outDF) else outDF
}

# converts just a single factor 

# def of omitLast is in comments above

# factorInfo is used if we are converting a factor that we've already
# converted on previous data; this argument is used to ensure that the
# conversion on the new data is consistent with the old

# easier to have both f, fname required

factorToDummies <- function (f,fname,omitLast=TRUE,factorInfo=NULL) 
{
    n <- length(f)
    fl <- levels(f)
    if (!is.null(factorInfo)) {
       ol <- factorInfo$omitLast
       if (ol != omitLast) warning('mismatched omitLast')
       fLevelsOrig <- factorInfo$lvls
       if (length(setdiff(fl,fLevelsOrig))) 
          stop(paste('new factor level found'))
    }
    fl.orig <- fl
    lfl <- length(fl)
    if (omitLast) fl <- fl[-lfl]
    ndumms <- lfl - omitLast
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == fl[i])
    colnames(dms) <- paste(fname,'.', fl, sep = "")
    tmp <- list()
    tmp$omitLast <- omitLast
    tmp$lvls <- fl.orig
    attr(dms,'factorInfo') <- tmp
    dms
}

# makes a factor from a single related set of dummies dms; if the
# variable has k levels, inclLast = FALSE means there are only k-1
# dummies provided, so the k-th must be generated

dummiesToFactor <- function(dms,inclLast=FALSE) {
   dms <- as.matrix(dms)
   if (!inclLast) {
      lastCol <- 1 - apply(dms,1,sum)
      dms <- cbind(dms,lastCol)
   }
   where1s <- apply(dms,1,function(rw) which(rw == 1))
   colnames(dms) <- paste0('V',1:ncol(dms),sep='')
   nms <- colnames(dms)
   f <- nms[where1s]
   as.factor(f)
}

dummiesToInt <- function(dms,inclLast=FALSE) {
  as.numeric(dummiesToFactor(dms=dms,inclLast=inclLast))
}

# maps a factor to 0,1,2,...,m-1 where m is the number of levels of f
factorTo012etc <- function(f) as.numeric(f)-1

# inputs an integer vector x and creates
intToDummies <- function(x,fname,omitLast=TRUE) 
{
   tmp <- as.factor(x)
   factorToDummies(tmp,fname,omitLast=omitLast)
}

# inputs a data frame intended for regression/classification, with X in
# the first cols and Y in the last; converts all factors to dummies, and
# outputs a matrix; in creating dummies, r-1 are retained for r levels,
# except for Y

xyDataframeToMatrix <- function(xy) {
   p <- ncol(xy)
   x <- xy[,1:(p-1)]
   y <- xy[,p]
   xd <- factorsToDummies(x,omitLast=TRUE)
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   as.matrix(cbind(xd,yd))
}

###################  misc. data frame/matrix ops  ######################

# multiply x[,cols] by vals, e.g. x[,cols[1]] * vals[1]
# code by Bochao Xin
multCols <- function(x,cols,vals) {
   tx <- t(x[,cols])
   x[,cols] <- t(tx*vals)
   x
}

# check for constant cols  

# d is a matrix or data frame; returns empty vector (i.e. length == 0)
# if no cols are constant, otherwise indices of those that are constant

constCols <- function(d) {
   if (is.matrix(d)) d <- as.data.frame(d)
   nDistinct <- sapply(lapply(d, unique), length)
   return(which(nDistinct == 1))
}

#######################  printing ##### #######################

catDFRow <- function(dfRow) {
  for (i in 1:ncol(dfRow)) {
     cat(as.character(dfRow[1,i]),' ')
  }
}

######################  misc. lm() routines  #######################

# computes the standard error of the predicted Y for X = xnew

stdErrPred <- function(regObj,xnew) {
   xx <- c(1,xnew)  # the 1 accounts for the intercept term
   xx <- as.numeric(xx)  # in case xnew was a row in a data frame
   as.numeric(sqrt(xx %*% vcov(regObj) %*% xx))
}

######################  misc. list ops ################################

# assign list components to individual variables of the same names

# similar to unpack() in zeallot pkg

ulist <- function(lst) 
{
   nms <- names(lst)
   if (any(nms == '')) stop('missing list name')
   tmp <- substitute(for (nm in nms) assign(nm,lst[[nm]]))
   eval(tmp,parent.frame())
}

#########################  other misc.  ################################

# convenience wrapper for cut() 

# arguments:

#    x: numeric vector
#    endpts: endpoints for the desired intervals, treated as open on the
#       left and closed on the right; to avoid NA values, make sure all
#       of x is accommodated

# value:

#    discrete version of x, with values 1,2,3,...; will have an R
#    attribute, 'endpts', so as to remember which ones we used

discretize <- function(x,endpts)
{
   xc <- cut(x,endpts,labels=1:(length(endpts)-1))
   attr(xc,'endpts') <- endpts
   xc
}
