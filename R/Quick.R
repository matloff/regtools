
################################################################## 
################################################################## 

# this qe*() series is inspired ggplot2::qplot; here 'qe' is for
# "quick-explore"

# the functions provide wrappers with a uniform interface, for
# quick and convenient fitting and prediction; for any given method, 
# not all options are offered in the wrapper version

# intended for quick exploration only;l sophisticated use
# should use the original functions, not these

# each has a predict() method, again with a fairly uniform interface

# some have plot() methods

# qe*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    possible algorithm-specific options

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

# a note on predictors/features that are R factors:

# In e.g. lm(), suppose some of the predictors are factors. Internally
# lm() will convert these to dummy variables, alleviating the user of
# that burden.  However, it can create problems in using the fitted
# model to predict new cases, say newx.

# Here newx must be a data frame (even if it has only one row), with
# column names matching those of the the training data frame.  But the
# types must match too, and in particular, for factors the levels must
# match.  This can be tricky.

# Our solution here is to have the qe*() functions include one row of
# the input data in the output object; the utility getRow1() does this.
# Then in the paired predict.qe*(), we call another utility,
# setTrainFactors() to make the factor levels match.  This is done by
# temporarily tacking newx onto the saved input row, resulting in a data
# frame that preserves the structure of the original data, then deleted
# that saved row.

##################################################################
####################  the qe*() functions  #######################
##################################################################

# qeLogit: generate estimated regression functions

# arguments:  see above

# value:

#    list of glm() output objects, one per class, and some misc.

qeLogit <- function(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   if (!classif) {print('for classification problems only'); return(NA)}
   if (!is.null(holdout)) splitData(holdout,data)
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
   outlist$trainRow1 <- getRow1(data,yName)
   class(outlist) <- c('qeLogit')
   if (!is.null(holdout)) {
      predictHoldout(outlist)
   }
   outlist
}

# predict.qeLogit: predict Ys from new Xs

# arguments:  see above

# value:  object of class 'qeLogit'; see above for components
 
predict.qeLogit <- function(object,newx) 
{
   newx <- setTrainFactors(object,newx)
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

#######################  qeLin()  ################################

# in regression case, simply wraps ordinary lm()

# in classification case, uses multivariate (i.e. vector Y) lm() for
# classification; faster than glm(), and may be useful as a rough tool
# if the goal is prediction, esp. if have some quadratic terms, which
# would make the linear approximation better 

# arguments:  see above
# value:  object of class 'qeLin' -- lm() output object, plus misc.

qeLin <- function(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
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
   lmout$trainRow1 <- getRow1(data,yName)
   class(lmout) <- c('qeLin',class(lmout))
   if (!is.null(holdout)) {
      predictHoldout(lmout)
      if (!classif) {
         summ <- summary(lmout)
         lmout$R2 <- summ$r.squared
         lmout$adjR2 <- summ$adj.r.squared
         lmout$holdoutR2 <- cor(preds,tst[,ycol])^2
      }
   }
   lmout
}

# arguments:  see above

# value:  see above

predict.qeLin <- function(object,newx) {
   class(object) <- class(object)[-1]
   newx <- setTrainFactors(object,newx)
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

#########################  qeKNN()  #################################

# arguments:  see above, plus

#     k: number of nearest neighbors
#     scaleX: if TRUE, features will be centered and scaled; note that
#        this means the features must be numeric

# value:  see above

# see note in kNN() man pg
 
qeKNN <- function(data,yName,k=25,scaleX=TRUE,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
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
   knnout$trainRow1 <- trainRow1
   class(knnout) <- c('qeKNN','kNN')
   if (!is.null(holdout)) {
      predictHoldout(knnout)
      knnout$holdIdxs <- holdIdxs
   } else knnout$holdIdxs <- NULL
   
   knnout
}

predict.qeKNN <- function(object,newx,newxK=1)
{
   class(object) <- 'kNN'
   if (!allNumeric(newx)) newx <- setTrainFactors(object,newx)
   classif <- object$classif
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo)
   if (is.vector(newx)) {
      nr <- 1
   } else nr <- nrow(newx)
   newx <- matrix(xyc$x,nrow=nr)
   preds <- predict(object,newx,newxK)
   if (!object$classif) return(preds)
   if (is.vector(preds)) preds <- matrix(preds,nrow=1)
   collectForReturn(object,preds)
}

#########################  qeRF()  #################################

# random forests

# arguments:  see above, plus

#     ntree: number of treesprobsto generate
#     minNodeSize: minimum number of data points in a node

# value:  see above
 
qeRF <- function(data,yName,nTree=500,minNodeSize=10,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
   require(randomForest)
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   frml <- as.formula(paste(yName,'~ .'))
   rfout <- randomForest(frml,data=data,ntree=nTree,nodesize=minNodeSize)
   rfout$classNames <- xyc$classNames
   rfout$classif <- classif
   rfout$trainRow1 <- getRow1(data,yName)
   class(rfout) <- c('qeRF','randomForest')
   if (!is.null(holdout)) predictHoldout(rfout)
   rfout
}

predict.qeRF <- function(object,newx)
{
   class(object) <- 'randomForest'
   newx <- setTrainFactors(object,newx)
   classif <- object$classif
   if (classif) {
      probs <- predict(object,newx,type='prob')
      res <- collectForReturn(object,probs)
   } else {
      res <- predict(object,newx,type='response')
   }
   res
}

plot.qeRF <- function(object) 
{
   genericPlot(object)
}

#########################  qeSVM()  #################################

# SVM

# arguments:  see above, plus

#     gamma: scale param, e.g. sd of radial basis ftn
#     cost: the SVM "C" parameter penalizing nonseparable data

# value:  see above
 
qeSVM <- function(data,yName,gamma=1.0,cost=1.0,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   if (!classif) {print('for classification problems only'); return(NA)}
   if (!is.null(holdout)) splitData(holdout,data)
   require(e1071)
   # xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   svmout <- svm(frml,data=data,cost=cost,gamma=gamma,decision.values=TRUE)
   ycol <- which(names(data) == yName)
   svmout$x <- data[,-ycol,drop=FALSE]
   y <- data[,ycol]
   svmout$data <- data
   svmout$yName <- yName
   svmout$ycol <- ycol
   svmout$classNames <- levels(y)
   svmout$classif <- classif
   svmout$formula <- frml
   svmout$trainRow1 <- getRow1(data,yName)
   class(svmout) <- c('qeSVM',class(svmout))
   if (!is.null(holdout)) predictHoldout(svmout)
   svmout
}

predict.qeSVM <- function(object,newx,k=NULL,scaleX=TRUE)
{
   require(e1071)
   class(object) <- class(object)[-1]
   newx <- setTrainFactors(object,newx)
   preds <- predict(object,newx,decision.values=TRUE)
   dvals <- attr(preds,'decision.values')
   colnames(dvals) <- colnames(object$decision.values)
   res <- list(predClasses=preds,dvals=dvals)
   classNames <- object$classNames
   ycol <- object$ycol
   x <- object$data[,-ycol]
   y <- object$data[,ycol]
   if (!is.null(k)) {
      trnScores <- object$decision.values
      newScores <- getDValsE1071(object,newx)
      probs <- knnCalib(y,trnScores,newScores,k)
      res$probs <- probs
   }
   res
}

# plot.qeSVM <- function(object,formula) 
# {
#    classNames <- object$classNames
#    class(object) <- class(object)[-1]
#    formula <- object$formula
#    formula <- as.formula(formula)
#    plot(object,object$data,formula)
# }

#########################  qeGBoost()  #################################

# gradient boosting

# arguments:  see above, plus

#     nTree: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qeGBoost <- function(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
   require(gbm)
   outlist <- list(classif=classif)
   if (classif) {   # classification case
      xyc <- getXY(data,yName,classif=classif) 
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
      outlist <- c(outlist,list(x=x,y=y,classNames=classNames,
         empirClassProbs=empirClassProbs))
      doGbm <- function(colI) 
      {
         tmpDF <- cbind(x,yDumms[,colI])
         names(tmpDF)[nx+1] <- 'yDumm'
         gbmout <- gbm(yDumm ~ .,data=tmpDF,distribution='bernoulli',
            n.trees=nTree,n.minobsinnode=minNodeSize,shrinkage=learnRate)
      }
      outlist$gbmOuts <- lapply(1:nydumms,doGbm)
   } else {   # regression case
      cmd <- paste0('gbmout <- gbm(',yName)
      cmd <- paste0(cmd,' ~ .,data=data,distribution="gaussian",')
      cmd <- paste0(cmd,'n.trees=',nTree,',')
      cmd <- paste0(cmd,'n.minobsinnode=',minNodeSize,',')
      cmd <- paste0(cmd,'shrinkage=',learnRate,')')
      eval(parse(text=cmd))
      outlist$gbmOuts <- gbmout
   }
   outlist$nTree <- nTree
   outlist$trainRow1 <- getRow1(data,yName)
   class(outlist) <- c('qeGBoost')
   if (!is.null(holdout)) predictHoldout(outlist)
   outlist
}

# arguments:  see above
# value:  object of class 'qeGBoost'; see above for components
predict.qeGBoost <- function(object,newx,newNTree=NULL) 
{
   newx <- setTrainFactors(object,newx)
   gbmOuts <- object$gbmOuts
   if (is.null(newNTree)) {
      nTree <- object$nTree
   } else nTree <- newNTree
   if (object$classif) {
      # get probabilities for each class
      g <- function(gbmOutsElt) 
         predict(gbmOutsElt,newx,n.trees=nTree,type='response') 
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
      res <- list(predClasses=predClasses,probs=probs)
   } else {
      res <- predict(object$gbmOuts,newx,n.trees=nTree)
   }
   class(res) <- 'qeGBoost'
   res
}

# graph to explore best number of trees

plot.qeGBoost <- function(object) 
{
   gbm.perf(object$gbmOuts)
}


#########################  qeNeural()  #################################

# neural networks 

# arguments:  see above, plus

#     hidden
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qeNeural <- function(data,yName,hidden=c(100,100),nEpoch=30,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   require(keras)
   if (!is.null(holdout)) splitData(holdout,data)
   ycol <- which(names(data) == yName)
   x <- data[,-ycol]
   if (!is.numeric(x)) {
      x <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   y <- data[,ycol]
   if (classif) {
      classNames <- levels(y)
      yFactor <- y
      y <- as.numeric(as.factor(y)) - 1
   } else {
      classNames <- NULL
      yFactor <- NULL
   }
   krsout <- krsFit(x,y,hidden,classif=classif,nClass=length(classNames),
      nEpoch=nEpoch)
   krsout$classif <- classif
   krsout$classNames=classNames
   krsout$factorsInfo=factorsInfo
   krsout$x <- x
   krsout$y <- y
   krsout$yFactor <- yFactor
   krsout$trainRow1 <- getRow1(data,yName)
   class(krsout) <- c('qeNeural',class(krsout))
   if (!is.null(holdout)) predictHoldout(krsout)
   krsout
}

predict.qeNeural <- function(object,newx=NULL,k=NULL)
{
   class(object) <- class(object)[-1]
   newx <- setTrainFactors(object,newx)
   if (nrow(newx) == 1) {  # kludge!; Tensorflow issue
      kludge1row <- TRUE
      newx <- rbind(newx,newx)
   } else kludge1row <- FALSE
   if (!is.null(object$factorsInfo)) {
      newx <- factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   preds <- predict(object,newx)
   probs <- attr(preds,'probs')  # may be NULL
   if (kludge1row) preds <- preds[1]
   if (!object$classif) {
      preds
   } else {
      classNames <- object$classNames
      preds <- classNames[preds+1]
      if (kludge1row) probs <- probs[1,]

      origProbs <- probs
      if (!is.null(k)) {
         # not ideal, but no apparent easy way to get this during 
         # training phases
         trnScores <- predict.krsFit(object,object$x)
         trnScores <- attr(trnScores,'probs')
         newScores <- matrix(probs,ncol=length(classNames))
         probs <- knnCalib(object$yFactor,trnScores,newScores,k)
      }

      outlist <- list(predClasses=preds,probs=probs,origProbs=origProbs)
      outlist
   } 
}

#########################  qePoly()  #################################

qePoly <- function(data,yName,deg=2,maxInteractDeg=deg,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   # will need all either numeric or factors; change character cols
   if (classif) {print('currently not for classification problems'); return(NA)}
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
##    data <- charsToFactors(data)
##    if (hasFactors(x)) {
##       xm <- factorsToDummies(x,omitLast=TRUE)
##       factorsInfo <- attr(xm,'factorsInfo')
##    } else {
##       xm <- as.matrix(x)
##       factorsInfo <- NULL
##    }
##    if (!is.numeric(xm)) stop('X must be numeric')
   makeAllNumeric(x,data)
   data <- cbind(xm,y)
   data <- as.data.frame(data)
   names(data)[ncol(data)] <- yName
   if (!is.null(holdout)) splitData(holdout,data)

   require(polyreg)
   qeout <- penrosePoly(d=data,yName=yName,deg=deg,maxInteractDeg)
   qeout$x <- x
   qeout$y <- y
   qeout$classif <- classif
   qeout$factorsInfo <- factorsInfo
   qeout$trainRow1 <- getRow1(data,yName)
   class(qeout) <- c('qePoly',class(qeout))
   if (!is.null(holdout)) predictHoldout(qeout)
   qeout
}

predict.qePoly <- function(object,newx)
{
   class(object) <- 'penrosePoly'
   newx <- charsToFactors(newx)
   newx <- factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)
   predict(object,newx)
}

prdPoly <- predict.qePoly

#########################  qePolyLog()  #################################

# logit form of qePoly

qePolyLog <- function(data,yName,deg=2,maxInteractDeg=deg,
   holdout=floor(min(1000,0.1*nrow(data))))
{
# stop('under construction')

   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
   classif <- is.factor(data[[yName]])
   origY <- y
   if (classif) {
      y <- factorTo012etc(y)
      earlierLevels <- attr(y,'earlierLevels')
   } else earlierLevels <- NULL
   dataSave <- data
   data <- cbind(x,y)
   names(data)[ncol(data)] <- yName

   if (!is.null(holdout)) {
      splitData(holdout,data)
      tst[ncol(tst)] <- origY[idxs]
   }

   require(polyreg)
   qeout <- polyFit(data,deg,use='glm')
   qeout$x <- x
   qeout$y <- y
   qeout$classif <- classif
   qeout$earlierLevels <- attr(y,'earlierLevels')
   qeout$trainRow1 <- getRow1(data,yName)
   class(qeout) <- c('qePolyLog',class(qeout))
   if (!is.null(holdout)) {
      # need original ycol
      data <- dataSave
      data[,ycol] <- y
      predictHoldout(qeout)
   }
   qeout
}

predict.qePolyLog <- function(object,newx)
{
   class(object) <- 'polyFit'
   predCode <- predict(object,newx)
   # map back to original Y names
   tmp <- object$earlierLevels[predCode+1]
   list(predClasses=tmp)
}

#########################  qeLASSO()  #################################

# for now, "X" must be numeric; if "Y" is a factor, we have a
# classification problem, otherwise regression

qeLASSO <- function(data,yName,alpha=1,holdout=floor(min(1000,0.1*nrow(data))))
{
   require(glmnet)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol]
   if (!all(sapply(x,is.numeric))) {
      makeAllNumeric(x,data)
   } else factorsInfo <- NULL
   
   classif <- is.factor(y)
   if (!is.null(holdout)) splitData(holdout,data)
   fam <- if (classif) 'multinomial' else 'gaussian'
   ym <- as.matrix(y)
   qeout <- cv.glmnet(x=xm,y=ym,alpha=alpha,family=fam)
   qeout$x <- x
   qeout$y <- y
   qeout$classif <- classif
   qeout$factorsInfo <- factorsInfo
   if (classif) qeout$classNames <- levels(y)
   class(qeout) <- c('qeLASSO',class(qeout))
   if (!is.null(holdout)) predictHoldout(qeout)
   qeout
}

predict.qeLASSO <- function(object,newx) 
{
   class(object) <- class(object)[-1]
   newx <- charsToFactors(newx)
   newx <- factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)

   if (!object$classif) return(predict(object,newx))
   # classif case
   classNames <- object$classNames
   tmp <- predict(object,newx,type='response')
   tmp <- tmp[,,1,drop=TRUE]
   # dropped too far?
   if (is.vector(tmp)) tmp <- matrix(tmp,ncol=ncol(object$x))
   colnames(tmp) <- classNames
   maxCols <- apply(tmp,1,which.max)
   predClasses <- object$classNames[maxCols]
   list(predClasses=predClasses,probs=tmp)
}

prdLASSO <- predict.qeLASSO

plot.qeLASSO <- function(object) 
{
   genericPlot(object)
}

# pca*-series, PCA wrappers for the qe*-series, including for prediction

# could instead make PCA an argument in each qe*(), but this is cleaner

# the additional argument is pcaProp, the proportion of variance desired
# for the principal components

pcaQE <- function(pcaProp,data,yName,qeName,...,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   # eventual return value
   res <- list()
   res$scaleX <- FALSE  # already scaled via prcomp()
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol]
   if (!allNumeric(x)) {
      x <- toAllNumeric(x)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   res$factorsInfo <- factorsInfo
   res$classif <- is.factor(y)
   
   tmp <- doPCA(x,pcaProp)
   newData <- tmp$newData
   pcaout <- tmp$pcaout
   numPCs <- tmp$numPCs
   y <- data[[yName]]
   newData[[yName]] <- y

   # now call the request
   # we've already scaled during PCA, so don't now 
   elipArgs <- ulist(list(...))
   # unpack the ...
   ulist(elipArgs)
   switch(qeName,
      'qeKNN' = qeOut <-qeKNN(newData,yName,k=k,holdout=holdout,scaleX=FALSE),
      'qeSVM' = qeOut <-qeSVM(newData,yName,gamma=gamma,cost=cost,
         holdout=holdout),
      stop('invalid qe function name')
   )

   ### qeKNNout <-qeKNN(newData,yName,k=k,holdout=holdout,scaleX=FALSE)
  
   res$qeOut <- qeOut
   res$pcaout <- pcaout
   res$numPCs <- numPCs
   res$trainRow1 <- qeOut$trainRow1
   class(res) <- 'pcaQE'
   res
}

predict.pcaQE <- function(object,newx)
{
   class(object) <- class(object)[-1]
   if (!allNumeric(newx)) {
      newx <- charsToFactors(newx)
      newx <- factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   newx <- predict(object$pcaout,newx)
   if (is.vector(newx)) {
      newxnames <- names(newx)
      newx <- matrix(newx,nrow=1)
   } else newxNames <- colnames(newx)
   numPCs <- object$numPCs
   newx <- newx[,1:numPCs,drop=FALSE]
   newx <- as.data.frame(newx)
   colnames(newx) <- newxNames[1:numPCs]
   predict(object$qeOut,newx=newx)
}

###################  utilities for qe*()  #########################

# see note on factor features at top of this file
setTrainFactors <- function(object,newx) 
{
   tmp <- rbind(object$trainRow1,newx)
   newx <- tmp[-1,,drop=FALSE]
   newx
}

# see note on factor features at top of this file
getRow1 <- function(data,yName) 
{
   ycol <- which(names(data) == yName)
   data[1,-ycol]
}

# some predict.qe*() functions call this for cleanup at end; see
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

# common code for qeLogit(), qeLin() etc.; preprocesses the input,
# returning new data frame xy, same x but y changing to dummies if
# classif; if yName is null, check features only

getXY <- function(data,yName,xMustNumeric=FALSE,classif,
   factorsInfo=NULL) 
{
   if (is.vector(data) && is.null(yName)) data <- data.frame(data)
   if (!is.data.frame(data))
      stopBrowser('data must be a data frame')
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
         print('change to factors'); return(NA)
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

# standard split into training, test sets

# arguments:
#    holdout: holdout set size
#    data: XY data frame

# globals/vale:
#    tst: the generated holdout set
#    data: the correspondingly reduced training set
#    holdIdxs: indices of the holdout set in original ata

require(gtools)

splitData <- defmacro(holdout,data, 
   expr={
      nHold <- holdout
      idxs <- sample(1:nrow(data),nHold);
      tst <- data[idxs,];
      data <- data[-idxs,]
      holdIdxs <- idxs
   }
)

# deprecated, gradually moving to toAllNumeric()
# x: 
#    change character variables to factors, then all factors to dummies,
#    recording factorInfo for later use in prediction; put result in xm
# data: 
#    if character, change to factor
makeAllNumeric <- defmacro(x,data,
   expr={
      data <- charsToFactors(data)
      if (hasFactors(x)) {
         xm <- factorsToDummies(x,omitLast=TRUE)
         factorsInfo <- attr(xm,'factorsInfo')
      } else {
         xm <- as.matrix(x)
         factorsInfo <- NULL
      }
   }
) 

# do the predictions in the holdout set

# arguments:
#    res: ultimate output of qe*()

# global inputs (from the caller):

#    tst: the holdout set
#    data: arg in the qe*() function
#    yName: arg in the qe*() function

# global outputs (creating locals in the caller):

#     res$testAcc: MAPE or class. error in holdout set
#     res$baseAcc: base MAPE or class. error (no features) in holdout set
#     res$holdoutPreds: predicted values in the holdout set
#     preds: ditto 
#     ycol: index of yName in 'data'
#     tstx: X portion of holdout data

predictHoldout <- defmacro(res,
   expr={
      # ycol <- which(names(data) == yName);
      ycol <- which(names(tst) == yName);
      tstx <- tst[,-ycol,drop=FALSE];
      preds <- predict(res,tstx);
      res$holdoutPreds <- preds;
      if (res$classif) {
         res$testAcc <- mean(preds$predClasses != tst[,ycol])
         res$baseAcc <- 1 - max(table(data[,ycol])) / nrow(data)
      } else {
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycol])))
      }
   }
)

######################  misc.  ###############################0w

# lm() balks if a label begins with a digit; check to see if we have any
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

# plot code for most

genericPlot <- function(object) 
{
   obj <- object
   class(obj) <- class(obj)[-1]  # actually not needed in many cases
   plot(obj)
}
