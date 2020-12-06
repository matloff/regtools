
#######################################################################
#########################  scaling  #################################
#######################################################################

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

#######################################################################
################### factors and dummy variables########################
#######################################################################

# these routines are useful in that some regression packages insist that
# predictor be factors, while some require dummy variables

# for each column in dfr, if factor then replace by dummies,
# else just copy column; if omitLast, then dummy for last level of
# factor is not included in output

# a key point is that, for later prediction after fitting a model, one
# needs to use the same transformations; otherwise, the data to be
# predicted may be missing a level of some factor; this of course is
# especially true if one is predicting a single case

# thus the factor names and levels are saved in attributes, and can be
# used as input, via factorInfo and factorsInfo for factorToDummies()
# and factorsToDummies(): 

# factorToDummies() outputs and later inputs factorInfo
# factorsToDummies() outputs and later inputs factorsInfo

####################  factorsToDummies()  ######################

# inputs a data frame, outputs same but with all factor cols expanded to
# dummies

# arguments

#    dfr: a data frame
#    omitLast: if TRUE, make m-1 dummies for an m-level factor
#    factorsInfo: factor levels found earlier, R list, element nm
#       is levels of factor named nm
#    dfOut: if TRUE, output a data frame rather than a matrix

# if the input has cols not numeric or factor, ftn will quit

factorsToDummies <- function(dfr,omitLast=FALSE,factorsInfo=NULL,
   dfOut=FALSE)
{
   if (is.factor(dfr)) dfr <- as.data.frame(dfr)

   # for now, no input cols other than numeric, factor allowed
   ## nnf <- function(i)  (!is.numeric(dfr[,i]) && !is.factor(dfr[,i]))
   ## notnumfact <- sapply(1:ncol(dfr),nnf)
   ## if (any(notnumfact)) 
   ##    stop('non-numeric, non-factor columns encountered')

   outDF <- NULL
   nullFI <- is.null(factorsInfo)
   if (nullFI) factorsInfoOut <- list()
   for (i in 1:ncol(dfr)) {
      dfi <- dfr[,i]
      if (length(levels(dfi)) == 1 && length(dfi) > 1) {
         msg <- paste(names(dfr)[i],'constant column: ',i) 
         warning(msg)
      }
      colName <- names(dfr)[i]
      if (!is.factor(dfi)) {
         if (!is.numeric(dfi)) 
            dfi <- as.factor(dfi)
         outDF <- cbind(outDF,dfi) 
         colnames(outDF)[ncol(outDF)] <- colName
      } else {
         dumms <- factorToDummies(dfi,colName,omitLast=omitLast,
            factorInfo=factorsInfo[[colName]])
         if (nullFI) {
            factorInfo <- attr(dumms,'factorInfo')
            factorsInfoOut[[colName]] <- factorInfo
         }
         outDF <- cbind(outDF,dumms)
      }
   }
   res <- if (!dfOut) as.matrix(outDF) else outDF
   if (nullFI) {
      attr(res,'factorsInfo') <- factorsInfoOut
   }  else
      attr(res,'factorsInfo') <- factorsInfo
   res
}

# converts just a single factor 

# def of omitLast is in comments above

# factorInfo is used if we are converting a factor that we've already
# converted on previous data; this argument is used to ensure that the
# conversion on the new data is consistent with the old, important for
# prediction settings

# easier to have both f, fname required

factorToDummies <- function (f,fname,omitLast=FALSE,factorInfo=NULL) 
{
    n <- length(f)
    fl <- levels(f)
    if (!is.null(factorInfo)) {
       fn <- factorInfo$fname
       if (fn != fname) stop('mismatched fname')
       ol <- factorInfo$omitLast
       if (ol != omitLast) stop('mismatChed omitLast')
       fullLevels <- factorInfo$fullLvls
       if (length(setdiff(fl,fullLevels))) 
          stop(paste('new factor level found'))
    } else fullLevels <- fl
    useLevels <- 
       if(omitLast) fullLevels[-length(fullLevels)] else fullLevels
    ndumms <- length(useLevels)
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == useLevels[i])
    colnames(dms) <- paste(fname,'.', useLevels, sep = "")
    tmp <- list()
    tmp$fname <- fname
    tmp$omitLast <- omitLast
    tmp$fullLvls <- fullLevels  # all levels even last
    attr(dms,'factorInfo') <- tmp
    dms
}

# makes a factor from a single related set of dummies dms; if the
# variable has k levels, inclLast = FALSE means there are only k-1
# dummies provided, so the k-th must be generated

dummiesToFactor <- function(dms,inclLast=FALSE) 
{
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

# inputs an integer vector x and creates dummies for the various values
intToDummies <- function(x,fname,omitLast=TRUE) 
{
   tmp <- as.factor(x)
   factorToDummies(tmp,fname,omitLast=omitLast)
}

# inputs a data frame and converts all character columns to factors
charsToFactors <- function(dtaf) 
{
   for (i in 1:ncol(dtaf)) {
      cli <- dtaf[,i]
      if (is.character(cli)) {
         dtaf[,i] <- as.factor(cli)
      }
   }
   dtaf
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

# x is a data frame; returns TRUE if at least one column is a factor
hasFactors <- function(x) 
{
   for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) return(TRUE)
   }
   FALSE
}

# say we have a factor f1, then encounter f2, with levels a subset of
# those of f1; we want to change f2 to have the same levels as f1; seems
# that this can NOT be done via levels(f2) <- levels(f1); typically used
# in predict() functions
toSuperFactor <- function(inFactor,superLevels) 
{
   inFactorChars <- as.character(inFactor)
   extraChars <- setdiff(superLevels,inFactorChars)
   nExtra <- length(extraChars)
   if (nExtra == 0) return(inFactor)
   newInFactorChars <- c(inFactorChars,extraChars)
   nExtra <- length(extraChars)
   tmp <- as.factor(newInFactorChars)
   nTmp <- length(tmp)
   start <- nTmp - nExtra + 1
   end <- nTmp
   tmp[-(start:end)]
}

# here we have a factor f with various levels, but want to lump all
# levelsl but the ones in saveLevels to a new level Other
toSubFactor <- function(f,saveLevels,lumpedLevel='other') 
{
   lvls <- levels(f)
   fChar <- as.character(f)
   other <- setdiff(lvls,saveLevels)
   whichOther <- which(fChar %in% other)
   fChar[whichOther] <- lumpedLevel
   as.factor(fChar)
}

# w: 
#    change character variables to factors, then all factors to dummies,
#    recording factorInfo for later use in prediction; put result in wm
# factorsInfo:
#    will be set to the byproduct of factorsToDummies(), if any
toAllNumeric <- function(w,r)
{
   w <- charsToFactors(w)
   if (hasFactors(w)) {
      wm <- factorsToDummies(w,omitLast=TRUE)
   } else {
      wm <- w
   }
   
} 

#######################################################################
###################  misc. data frame/matrix ops  ######################
#######################################################################

# multiply x[,cols] by vals, e.g. x[,cols[1]] * vals[1]
# code by Bochao Xin
multCols <- function(x,cols,vals) {
#     tx <- t(x[,cols])
#     x[,cols] <- t(tx*vals)
#    for (i in 1:length(cols)) {
#       cl <- cols[i]
#       x[,cl] <- x[,cl] * vals[1,i]
#    }
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

# print a data frame row

catDFRow <- function(dfRow) {
  for (i in 1:ncol(dfRow)) {
     cat(as.character(dfRow[1,i]),' ')
  }
}

# print the classes of a data frame

getDFclasses <- function(dframe) {
   tmp <- sapply(1:ncol(dframe),function(i) class(dframe[,i]))
   names(tmp) <- names(dframe)
   tmp
}

# check whether all elements of a list, including a data frame, are
# numeric

allNumeric <- function(lst) 
{
   tmp <- sapply(lst,is.numeric)
   all(tmp) 
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


#######################  loss functions  ###############################

# mean absolute prediction error
MAPE <- function(yhat,y) 
{
   if (!is.vector(y) || !is.vector(yhat)) 
      stop('inputs must be vectors')
   mean(abs(yhat-y))
}



# overall error rate

# either 

#    y is a vector of 0s and 1s, 
#    yhat a vector of estimated probabilities of 1

# or

#    y is a vector of numeric class labels, starting at 1 or 0
#    yhat is a matrix, with y[i,j] = prob of Y = j+startAt1-1

probIncorrectClass <- function(yhat,y,startAt1=TRUE
) 
{
   if (is.factor(y) || is.factor(yhat)) 
      stop('vector/matrix inputs only')
   if (is.vector(yhat)) {
      yhat <- round(yhat)
      return(mean(yhat != y))
   }
   classPred <- apply(yhat,1,which.max) 
   classActual <- y - startAt1 + 1
   mean(classPred != classActual)
}

# proportion misclassified; deprecated in favor of probIncorrectClass
propMisclass <- function(y,yhat) 
{
   if (!is.vector(y) && !is.factor(y)) 
      stop('predicted classes must be a vector or factor')
   mean(y != yhat)
}

# included lossFtn choices are MAPE and probIncorrectClass; user may
# supply others
findOverallLoss <- function (regests, y, lossFtn = MAPE) 
{
   loss1row <- function(regestsRow) lossFtn(y, regestsRow)
   apply(regests, 1, loss1row)
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

require(gtools)

# use this after doing error checking, giving the user the choice of
# leaving, or continuing in the debugger
stopBrowser <- defmacro(msg,expr=
   {
   cat(msg,'\n')
   d <- readline('hit Enter to leave, or d to enter debugger: ')
   if (d == '') stop('')
   browser()
   }
)

# call prcomp(x,pcaProp), transform x to the PCs for the top pcaProp
# proportion of variance

doPCA <- function(x,pcaProp) 
{
   tmp <- prcomp(x,scale.=TRUE)
   newx <- predict(tmp,x)
   pcVars <- tmp$sdev^2
   ncx <- ncol(x)
   csums <- cumsum(pcVars) 
   csums <- csums / csums[ncx]
   numPCs <- min(which(csums >= pcaProp))
   newx <- newx[,1:numPCs]
   list(newx=newx,pcaout=tmp)
}
