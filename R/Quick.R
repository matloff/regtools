
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

# see note in kNN() man pg
 
qKNN <- function(data,yName,k,scaleX=TRUE) 
{
   classif <- is.factor(data[[yName]])
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif)
   x <- xyc$x
   xm <- as.matrix(x)
   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y
   if (classif) {
      xy <- xyc$xy
      y <- xyc$yDumms
      classNames <- xyc$classNames
   } 

   knnout <- kNN(xm,y,newx=NULL,k,scaleX=scaleX,classif=classif)
   if (classif) knnout$classNames <- classNames
   knnout$classif <- classif
   knnout$factorsInfo <- factorsInfo
   class(knnout) <- c('qKNN','kNN')
   knnout

}

predict.qKNN <- function(object,newx)
{
   class(object) <- 'kNN'
   classif <- object$classif
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo)
   newx <- as.matrix(xyc$x)
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
 
qRF <- function(data,yName,nTree=500,minNodeSize=10) 
{
   classif <- is.factor(data[[yName]])
   require(randomForest)
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
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
   classif <- object$classif
   if (classif) {
      probs <- predict(object,newx,type='prob')
      res <- collectForReturn(object,probs)
   } else {
      res <- predict(object,newx,type='response')
   }
   res
}

#########################  qSVM()  #################################

# SVM

# arguments:  see above, plus

#     gamma: scale param, e.g. sd of radial basis ftn
#     cost: the SVM "C" parameter penalizing nonseparable data

# value:  see above
 
qSVM <- function(data,yName,gamma=1.0,cost=1.0) 
{
   classif <- is.factor(data[[yName]])
   if (!classif) stop('for classification problems only')
   require(e1071)
   # xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   svmout <- svm(frml,data=data,cost=cost,gamma=gamma)
   ycol <- which(names(data) == yName)
   svmout$x <- data[,-ycol,drop=FALSE]
   y <- data[,ycol]
   # svmout$classNames <- xyc$classNames
   svmout$classNames <- levels(y)
   svmout$classif <- classif
   class(svmout) <- c('qSVM',class(svmout))
   svmout
}

predict.qSVM <- function(object,newx,k=25,scaleX=TRUE)
{
   class(object) <- class(object)[-1]
   preds <- predict(object,newx)
   res <- list(predClasses=preds)
   classNames <- object$classNames
   x <- object$x
   probs <- labelsToProbs(x,newx,svmout$fitted,classNames,k,
      scaleX=scaleX)
   res$probs <- probs
   res
}

#########################  qGBoost()  #################################

# gradient boosting

# arguments:  see above, plus

#     nTrees: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qGBoost <- function(data,yName,
   nTrees=100,minNodeSize=10,learnRate=0.1)
{
   classif <- is.factor(data[[yName]])
   if (!classif) stop('classification only')
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
   class(outlist) <- c('qGBoost')
   outlist
}

####################  predict.qGBoost  ######################

# arguments:  see above

# value:  object of class 'qGBoost'; see above for components
 
predict.qGBoost <- function(object,newx) 
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

#########################  qNeural()  #################################

# gradient boosting

# arguments:  see above, plus

#     hidden
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qNeural <- function(data,yName,hidden,nEpoch=30)
{
   classif <- is.factor(data[[yName]])
   ycol <- which(names(data) == yName)
   x <- data[,-ycol]
   if (!is.numeric(x)) {
      x <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   y <- data[,ycol]
   if (classif) {
      classNames <- levels(y)
      y <- as.numeric(as.factor(y)) - 1
   } else classNames <- NULL
   krsout <- krsFit(x,y,hidden,classif=classif,nClass=length(classNames),
      nEpoch=nEpoch)
   krsout$classif <- classif
   krsout$classNames=classNames
   krsout$factorsInfo=factorsInfo
   krsout$x <- x
   class(krsout) <- c('qNeural',class(krsout))
   krsout
}

predict.qNeural <- function(object,newx)
{
   class(object) <- class(object)[-1]
   if (nrow(newx) == 1) {  # kludge!; Tensorflow issue
      kludge1row <- TRUE
      newx <- rbind(newx,newx)
   } else kludge1row <- FALSE
   if (!is.null(object$factorsInfo)) {
      newx <- factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   preds <- predict(object,newx)
   if (!object$classif) {
      if (kludge1row) preds <- preds[1]
      preds
   } else {
      classNames <- object$classNames
      preds <- classNames[preds+1]
      preds
      # not implementing class probs for now
   } 
}

###################  utilities for q*()  #########################

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
# classif; if yName is null, check features only

getXY <- function(data,yName,xMustNumeric=FALSE,classif,
   factorsInfo=NULL) 
{
   if (!is.data.frame(data))
      stop('data must be a data frame')
   if (!is.null(yName)) {
      ycol <- which(names(data) == yName)
      y <- data[,ycol]
   } else y <- ycol <- NULL
   if (classif && !is.factor(y)) stop('Y must be a factor')
   if (!is.null(y)) {
      x <- data[,-ycol,drop=FALSE]
   } else x <- data
   # check for non-numeric cols in x, if necessary
   if (xMustNumeric) {
      xClasses <- getDFclasses(x)
      if (any(xClasses=='logical') || any(xClasses=='character')) {
         print('character or logical variables currently not allowed')
         stop('change to factors')
      }
      x <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   if (classif && !is.null(yName)) {
      yDumms <- factorsToDummies(y,omitLast=FALSE,factorsInfo=NULL)
      classNames <- levels(y)
      colnames(yDumms) <- classNames
      xy <- cbind(x,yDumms)
   } else {
      yDumms <- NULL
      classNames <- NULL
      xy <- NULL
   }
   list(xy=xy,x=x,y=y,yDumms=yDumms,classNames=classNames,
      factorsInfo=factorsInfo)

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

