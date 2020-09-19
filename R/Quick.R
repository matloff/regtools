
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

#    data:  dataframe, training set; class labels col is a factor 
#    yName:  name of the class labels column
#    classif:  TRUE for classification problems
#    possible options

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    possible options
 
# value:  R list with components
 
#    ypreds:  R factor instance of predicted class labels, one element f
#       for each row of newx 
#    conditprobs:  vector/matrix of class probabilities; in the 2-class
#       case, a vector, the probabilities of Y = 1

##################################################################
##################################################################

##################################################################
# qLogit: generate estimated regression functions
##################################################################

# arguments:  see above

# value:

#    list of glm() output objects, one per class, and some misc.

qLogit <- function(data,yName,classif=TRUE) 
{
stop('under construction')
   if (!classif) stop('for classification problems only')
   xyc <- xClassGetXY(data,yName) 
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

# uses multivariate (i.e. vector Y) lm() for classification; faster than
# glm(), and may be useful as a rough tool if the goal is prediction, 
# esp. if have some quadratic terms, which would make the linear
# approximation better

# arguments:  see above

# value:  object of class 'qLin'; lm() output object, plus misc.

qLin <- function(data,yName,classif) 
{
stop('under construction')
   xyc <- xClassGetXY(data,yName)
   xy <- xyc$xy
   classNames <- xyc$classNames
   yNames <- paste0(classNames,collapse=',')
   cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .,data=xy)')
   eval(parse(text=cmd))
   lmout$classif <- classif 
   class(lmout) <- c('qLin',class(lmout))
   lmout
}

# arguments:  see above

# value:  see above

predict.qLin <- function(linClassObj,newx) {
   class(linClassObj) <- class(linClassObj)[-1]
   preds <- predict(linClassObj,newx)
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
 
knnClass <- function(data,yName,k,scaleX=TRUE,classif) 
{
   
stop('under construction')
   xyc <- xClassGetXY(data,yName,xMustNumeric=TRUE)
   x <- xyc$x
   xm <- as.matrix(x)
   y <- xyc$y
   xy <- xyc$xy
   yDumms <- xyc$yDumms
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)

   knnout <- kNN(xm,yDumms,newx=NULL,k,scaleX=scaleX,classif=TRUE)
   knnout$classNames <- classNames
   knnout$classif <- classif
   class(knnout) <- c('knnClass','kNN')
   knnout

}

predict.knnClass <- function(object,newx)
{
   class(object) <- 'kNN'
   newx <- as.matrix(newx)
   probs <- predict(object,newx)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)
   collectForReturn(object,probs)
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
   require(randomForest)
   xyc <- xClassGetXY(data,yName,xMustNumeric=TRUE)
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
 
qSVM <- function(data,yName,gamma=1.0,cost=1.0,classif=TRUE) 
{
stop('under construction')
   if (!classif) stop('for classification problems only')
   require(e1071)
   xyc <- xClassGetXY(data,yName,xMustNumeric=TRUE)
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
   nTrees=100,minNodeSize=10,learnRate=0.1,claasif=TRUE)
{
stop('under construction')
   require(gbm)
   xyc <- xClassGetXY(data,yName) 
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

# common code for logitClass(), linClass() etc.; preprocesses the input,
# returning new data frame xy, same x but dummies for y now
xClassGetXY <- function(data,yName,xMustNumeric=FALSE) 
{
   if (!is.data.frame(data)) 
      stop('data must be a data frame')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   x <- data[,-ycol,drop=FALSE]
   if (xMustNumeric && hasFactors(x))
      stop('"X" must be numeric')
   yDumms <- factorsToDummies(y,omitLast=FALSE)
   classNames <- levels(y)
   colnames(yDumms) <- classNames
   xy <- cbind(x,yDumms)
   list(xy=xy, x=x, y=y, yDumms=yDumms, classNames=classNames)
}
