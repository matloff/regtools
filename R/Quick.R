
##################################################################
##################################################################

# this q*() series is inspired ggplot2::qplot; here 'q' is for "quick."
# as in qplot() and the old C library function qsort()

# the functions provide wrappers with a uniform interface, for
# quick and convenient fitting and prediction; for any given method, 
# not all options are offered in the wrapper version

# intended for quick exploration only;l sophisticated use
# should use the original functions, not these

# each has a predict() method, again with a fairly uniform interface

# q*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    possible options

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    possible options
 
# value:  R list with components as follows:
 
#    classification case:

#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#    regression case:

#       vector of predicted values

##################################################################
##################################################################

##################################################################
# qLogit: generate estimated regression functions
##################################################################

# arguments:  see above

# value:

#    list of glm() output objects, one per class, and some misc.

qLogit <- function(data,yName) 
{
   classif <- is.factor(data[[yName]])
   if (!classif) stop('for classification problems only')
   xyc <- getXY(data,yName,classif=TRUE) 
   xy <- xyc$xy
   x <- xyc$x
   yDumms <- xyc$yDumms
   y <- xyc$y
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx
   empirClassProbs <- colMeans(yDumms)
   outlist <- 
      list(x=x,y=y,classNames=classNames,empirClassProbs=empirClassProbs)
   doGlm <- function(colI) 
   {
      tmpDF <- cbind(x,yDumms[,colI])
      names(tmpDF)[nx+1] <- 'yDumm'
      glmout <- glm(yDumm ~ .,data=tmpDF,family=binomial)
   }
   outlist$glmOuts <- lapply(1:nydumms,doGlm)
   outlist$classif <- classif
   class(outlist) <- c('qLogit')
   outlist
}

##################################################################
# predict.qLogit: predict Ys from new Xs
##################################################################

# arguments:  see above

# value:  object of class 'qLogit'; see above for components
 
predict.qLogit <- function(object,newx) 
{
   # get probabilities for each class
   glmOuts <- object$glmOuts
   g <- function(glmOutsElt) predict(glmOutsElt,newx,type='response') 
   probs <- sapply(glmOuts,g)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)
   classNames <- object$classNames
   colnames(probs) <- classNames
   # separate logits for the m classes will not necessrily sum to 1, so
   # normalize
   sumprobs <- apply(probs,1,sum)  
   probs <- (1/sumprobs) * probs
   predClasses <- apply(probs,1,which.max) 
   predClasses <- classNames[predClasses]
   list(predClasses=predClasses,probs=probs)
}

#######################  qLin()  ################################

# in regression case, simply wraps ordinary lm()

# in classification case, uses multivariate (i.e. vector Y) lm() for
# classification; faster than glm(), and may be useful as a rough tool
# if the goal is prediction, esp. if have some quadratic terms, which
# would make the linear approximation better 

# arguments:  see above
# value:  object of class 'qLin' -- lm() output object, plus misc.

qLin <- function(data,yName) 
{
   classif <- is.factor(data[[yName]])
   if (classif) {
      xyc <- getXY(data,yName,classif=TRUE)
      xy <- xyc$xy
      classNames <- xyc$classNames
      # check for numeric class names
      checkNumericNames(classNames)
      yNames <- paste0(classNames,collapse=',')
   } else {
      xy <- data
      yNames <- yName
   }
   cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .,data=xy)')
   eval(parse(text=cmd))
   lmout$classif <- classif 
   class(lmout) <- c('qLin',class(lmout))
   lmout
}

# arguments:  see above

# value:  see above

predict.qLin <- function(object,newx) {
   class(object) <- class(object)[-1]
   preds <- predict(object,newx)
   if (!object$classif) return(preds)
   probs <- pmax(preds,0)
   probs <- pmin(probs,1)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)
   probsums <- apply(probs,1,sum)
   probs <- probs * 1/probsums
   predClasses <- apply(preds,1,which.max)
   predClasses <- colnames(preds)[predClasses]
   list(predClasses=predClasses,probs=probs)
}

#########################  qKNN()  #################################

# arguments:  see above, plus

#     k: number of nearest neighbors
#     scaleX: if TRUE, features will be centered and scaled; note that
#        this means the features must be numeric

# value:  see above
 
qKNN <- function(data,yName,k,scaleX=TRUE) 
{
stop('under construction')
   classif <- is.factor(data[[yName]])
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif)
   x <- xyc$x
   xm <- as.matrix(x)
   y <- xyc$y
   if (classif) {
      xy <- xyc$xy
      y <- xyc$yDumms
      classNames <- xyc$classNames
   } 

   knnout <- kNN(xm,y,newx=NULL,k,scaleX=scaleX,classif=classif)
   if (classif) knnout$classNames <- classNames
   knnout$classif <- classif
   class(knnout) <- c('knnClass','kNN')
   knnout

}

predict.qKNN <- function(object,newx)
{
   class(object) <- 'kNN'
   newx <- as.matrix(newx)
   preds <- predict(object,newx)
   if (!object$classif) return(preds)
   if (is.vector(preds)) preds <- matrix(preds,nrow=1)
   collectForReturn(object,preds)
}

#########################  qRF()  #################################

# random forests

# arguments:  see above, plus

#     ntree: number of treesprobsto generate
#     minNodeSize: minimum number of data points in a node

# value:  see above
 
qRF <- function(data,yName,nTree=500,minNodeSize=10,classif) 
{
stop('under construction')
   classif <- is.factor(data[[yName]])
   require(randomForest)
   xyc <- getXY(data,yName,xMustNumeric=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   rfout <- randomForest(frml,data=data,ntree=nTree,nodesize=minNodeSize)
   rfout$classNames <- xyc$classNames
   rfout$classif <- classif
   class(rfout) <- c('qRF','randomForest')
   rfout
}

predict.qRF <- function(object,newx)
{
   class(object) <- 'randomForest'
   probs <- predict(object,newx,type='prob')
   collectForReturn(object,probs)
}

#########################  qSVM()  #################################

# SVM

# arguments:  see above, plus

#     gamma: scale param, e.g. sd of radial basis ftn
#     cost: the SVM "C" parameter penalizing nonseparable data

# value:  see above
 
qSVM <- function(data,yName,gamma=1.0,cost=1.0) 
{
stop('under construction')
   classif <- is.factor(data[[yName]])
   if (!classif) stop('for classification problems only')
   require(e1071)
   xyc <- getXY(data,yName,xMustNumeric=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   svmout <- svm(frml,data=data,cost=cost,gamma=gamma)
   svmout$classNames <- xyc$classNames
   ycol <- which(names(data) == yName)
   svmout$x <- data[,-ycol,drop=FALSE]
   svmout$classif <- classif
   class(svmout) <- c('qSVM','svm')
   svmout
}

predict.qSVM <- function(object,newx,k=25)
{
   class(object) <- 'svm'
   preds <- predict(object,newx)
   res <- list(predClasses=preds)
   classNames <- object$classNames
   x <- object$x
   probs <- labelsToProbs(x,newx,svmout$fitted,classNames,k)
   res$probs <- probs
   res
}

#########################  qBoost()  #################################

# gradient boosting

# arguments:  see above, plus

#     nTrees: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qBoost <- function(data,yName,
   nTrees=100,minNodeSize=10,learnRate=0.1)
{
stop('under construction')
   classif <- is.factor(data[[yName]])
   require(gbm)
   xyc <- getXY(data,yName) 
   xy <- xyc$xy
   x <- xyc$x
   yDumms <- xyc$yDumms
   y <- xyc$y
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx
   empirClassProbs <- colMeans(yDumms)
   outlist <- list(x=x,y=y,classNames=classNames,
      empirClassProbs=empirClassProbs,classif=classif)
   doGbm <- function(colI) 
   {
      tmpDF <- cbind(x,yDumms[,colI])
      names(tmpDF)[nx+1] <- 'yDumm'
      gbmout <- gbm(yDumm ~ .,data=tmpDF,
         n.trees=nTrees,n.minobsinnode=minNodeSize,shrinkage=learnRate)
   }
   outlist$gbmOuts <- lapply(1:nydumms,doGbm)
   class(outlist) <- c('qBoost')
   outlist
}

####################  predict.qBoost  ######################

# arguments:  see above

# value:  object of class 'qBoost'; see above for components
 
predict.qBoost <- function(object,newx) 
{
   # get probabilities for each class
   gbmOuts <- object$gbmOuts
   g <- function(gbmOutsElt) predict(gbmOutsElt,newx,type='response') 
   probs <- sapply(gbmOuts,g)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)
   classNames <- object$classNames
   colnames(probs) <- classNames
   # separate runs for the m classes will not necessrily sum to 1, so
   # normalize
   sumprobs <- apply(probs,1,sum)  
   probs <- (1/sumprobs) * probs
   predClasses <- apply(probs,1,which.max) 
   predClasses <- classNames[predClasses]
   list(predClasses=predClasses,probs=probs)
}

# some predict.*Class() functions call this for cleanup at end; see
# list() below for values; intended for settings in which the base
# algorithm returns probabilities, from which this function will
# computed predicted classes
collectForReturn <- function(object,probs) 
{
   classNames <- object$classNames
   colnames(probs) <- classNames
   predClasses <- apply(probs,1,which.max)
   predClasses <- classNames[predClasses]
   list(predClasses=predClasses,probs=probs)
}

# common code for qLogit(), qLin() etc.; preprocesses the input,
# returning new data frame xy, same x but y changing to dummies if
# classif

getXY <- function(data,yName,xMustNumeric=FALSE,classif) 
{
   if (!is.data.frame(data))
      stop('data must be a data frame')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
   # if (xMustNumeric && hasFactors(x))
   #    stop('"X" must be numeric')
   if (classif) {
      yDumms <- factorsToDummies(y,omitLast=FALSE)
      classNames <- levels(y)
      colnames(yDumms) <- classNames
      xy <- cbind(x,yDumms)
   } else {
      yDumms <- NULL
      classNames <- NULL
   }
   list(xy=xy, x=x, y=y, yDumms=yDumms, classNames=classNames)

}
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

# thus the factor names and levels are saved in attributes, and can be
# used as input, via factorsInfo

# arguments

#    dfr: a data frame
#    omitLast: if TRUE, make m-1 dummies for an m-level factor
#    factorsInfo: factor levels found earlier
#    dfOut: if TRUE, output a data frame rather than a matrix

# value

#    matrix/df of dummy variables

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
# conversion on the new data is consistent with the old, important for
# prediction settings

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


#######################  loss functions  ###############################

# mean absolute prediction error
MAPE <- function(y,yhat) 
{
   if (!is.vector(y)) 
      stop('predicted classes must be a vector')
   mean(abs(y-yhat))
}



# overall probability of correct classification, y as a vector of 0s and
# 1s, yhat a vector of estimated probabilities of 1
probIncorrectClass <- function(y,yhat) 
{
   if (is.vector(y)) {
      yhat <- round(yhat)
      return(mean(yhat != y))
   }
   classPred <- apply(yhat,1,which.max) 
   classActual <- apply(y,1,which.max)
   mean(classPred != classActual)
}

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

   if (!is.data.frame(data)) 
      stop('data must be a data frame')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (classif && !is.factor(y)) stop('Y must be a factor')
   x <- data[,-ycol,drop=FALSE]
   # check for non-numeric cols in x, if necessary
   if (xMustNumeric) {
      xModes <- sapply(1:ncol(x),function(i) class(d[,i]))
      if (any(is.logical(xModes)) || any(is.character(xModes)))
         print('character or logical variables currently not allowed')
         stop('change to factors')
   }
   yDumms <- factorsToDummies(y,omitLast=FALSE)
   classNames <- levels(y)
   colnames(yDumms) <- classNames
   xy <- cbind(x,yDumms)
   list(xy=xy, x=x, y=y, yDumms=yDumms, classNames=classNames)
}

checkNumericNames <- function(nms) 
{
   for (nm in nms) {
      s <- substr(nm,1,1)
      if (s >= '0' && s <= '9') {
         stop('factor level begins with a digit')
      }
   }
}

# prepend the string s to each element of the character vector v
prepend <- function(s,v) 
{
   v <- as.character(v)
   for (i in 1:length(v)) {
      v[i] <- paste0(s,v[i])
   }
   as.factor(v)
}
