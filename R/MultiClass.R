
# multiclass models

##################################################################
##################################################################

# the *Class() functions provide wrappers for a uniform interface, for
# quick and convenient fitting and prediction; for any given method, not
# all options are offered in the wrapper version; sophisticated use
# should use the original functions; they might be viewed as analogous
# to the qplot() function in ggplot2

# each has a predict() method, again with a fairly uniform interface

# the 2-class case is included, but with more appropriate form for the
# predictions

# *Class() arguments:

#    data:  dataframe, training set; class labels col is a factor 
#    yName:  name of the class labels column
#    possible options

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from logitClass()
#    possible options
#    newx:  data frame of points to be predicted
 
# value:  R list with components
 
#    ypreds:  R factor instance of predicted class labels, one element f
#       for each row of newx 
#    conditprobs:  vector/matrix of class probabilities; in the 2-class
#       case, a vector, the probabilities of Y = 1

# deprecated:

ovalogtrn <- function(...) 
{
   stop('deprecated; use logitClass()')
}

predict.ovalog <- function(...) 
{
   stop('')
}

##################################################################
##################################################################

##################################################################
# logitClass: generate estimated regression functions
##################################################################

# arguments:  see above

# value:

#    list of glm() output objects, one per class, and some misc.

logitClass <- function(data,yName) 
{
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
   class(outlist) <- c('logitClass')
   outlist
}

##################################################################
# predict.logitClass: predict Ys from new Xs
##################################################################

# arguments:  see above

# value:  object of class 'logitClass'; see above for components
 
predict.logitClass <- function(object,newx) 
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

#######################  linClass()  ################################

# uses multivariate (i.e. vector Y) lm() for classification; faster than
# glm(), and may be useful as a rough tool if the goal is prediction, 
# esp. if have some quadratic terms, which would make the linear
# approximation better

# arguments:  see above

# value:  object of class 'linClass'; lm() output object, plus misc.

linClass <- function(data,yName) 
{
   xyc <- xClassGetXY(data,yName)
   xy <- xyc$xy
   classNames <- xyc$classNames
   yNames <- paste0(classNames,collapse=',')
   cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .,data=xy)')
   eval(parse(text=cmd))
   class(lmout) <- c('linClass',class(lmout))
   lmout
}

# arguments:  see above

# value:  see above

predict.linClass <- function(linClassObj,newx) {
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

#########################  knnClass()  #################################

# arguments:  see above, plus

#     k: number of nearest neighbors
#     scaleX: if TRUE, features will be centered and scaled; note that
#        this means the features must be numeric

# value:  see above
 
knnClass <- function(data,yName,k,scaleX=TRUE) 
{
   
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

#########################  rfClass()  #################################

# random forests

# arguments:  see above, plus

#     ntree: number of treesprobsto generate
#     minNodeSize: minimum number of data points in a node

# value:  see above
 
rfClass <- function(data,yName,nTree=500,minNodeSize=10) 
{
   require(randomForest)
   xyc <- xClassGetXY(data,yName,xMustNumeric=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   rfout <- randomForest(frml,data=data,ntree=nTree,nodesize=minNodeSize)
   rfout$classNames <- xyc$classNames
   class(rfout) <- c('rfClass','randomForest')
   rfout
}

predict.rfClass <- function(object,newx)
{
   class(object) <- 'randomForest'
   probs <- predict(object,newx,type='prob')
   collectForReturn(object,probs)
}

#########################  svmClass()  #################################

# SVM

# arguments:  see above, plus

#     gamma: scale param, e.g. sd of radial basis ftn
#     cost: the SVM "C" parameter penalizing nonseparable data

# value:  see above
 
svmClass <- function(data,yName,gamma=1.0,cost=1.0) 
{
   require(e1071)
   xyc <- xClassGetXY(data,yName,xMustNumeric=TRUE)
   frml <- as.formula(paste(yName,'~ .'))
   svmout <- svm(frml,data=data,cost=cost,gamma=gamma)
   svmout$classNames <- xyc$classNames
   ycol <- which(names(data) == yName)
   svmout$x <- data[,-ycol,drop=FALSE]
   class(svmout) <- c('svmClass','svm')
   svmout
}

predict.svmClass <- function(object,newx,k=25)
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

#########################  boostClass()  #################################

# gradient boosting

# arguments:  see above, plus

#     nTrees: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
boostClass <- function(data,yName,nTrees=100,minNodeSize=10,learnRate=0.1)
{
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
   outlist <- 
      list(x=x,y=y,classNames=classNames,empirClassProbs=empirClassProbs)
   doGbm <- function(colI) 
   {
      tmpDF <- cbind(x,yDumms[,colI])
      names(tmpDF)[nx+1] <- 'yDumm'
      gbmout <- gbm(yDumm ~ .,data=tmpDF,
         n.trees=nTrees,n.minobsinnode=minNodeSize,shrinkage=learnRate)
   }
   outlist$gbmOuts <- lapply(1:nydumms,doGbm)
   class(outlist) <- c('boostClass')
   outlist
}

####################  predict.boostClass  ######################

# arguments:  see above

# value:  object of class 'boostClass'; see above for components
 
predict.boostClass <- function(object,newx) 
{
stop('under construction')
   # get probabilities for each class
   gbmOuts <- object$gbmOuts
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

##################################################################

#  older AVA, OVA code

##################################################################
# avalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    as in logitClass() above

# value:

#    as in logitClass() above

avalogtrn <- function(trnxy,yname) 
{
   if (is.null(colnames(trnxy))) 
      stop('trnxy must have column names')
   ycol <- which(names(trnxy) == yname)
   y <- trnxy[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   x <- trnxy[,-ycol,drop=FALSE]
   xd <- factorsToDummies(x,omitLast=TRUE)
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   m <- ncol(yd)
   n <- nrow(trnxy)
   classIDs <- apply(yd,1,which.max) - 1
   classcounts <- table(classIDs)
   ijs <- combn(m,2) 
   outmat <- matrix(nrow=ncol(xd)+1,ncol=ncol(ijs))
   colnames(outmat) <- rep(' ',ncol(ijs))
   attr(outmat,'Xcolnames') <- colnames(xd)
   doreg <- function(ij)  # does a regression for one pair of classes
   {
      i <- ij[1] - 1
      j <- ij[2] - 1
      tmp <- rep(-1,n)
      tmp[classIDs == i] <- 1
      tmp[classIDs == j] <- 0
      yij <- tmp[tmp != -1]
      xij <- xd[tmp != -1,]
      coef(glm(yij ~ xij,family=binomial))
   }
   for (k in 1:ncol(ijs)) {
      ij <- ijs[,k]
      outmat[,k] <- doreg(ij)
      colnames(outmat)[k] <- paste(ij,collapse=',')
   }
   if (any(is.na(outmat))) warning('some NA coefficients')
   empirClassProbs <- classcounts/sum(classcounts)
   attr(outmat,'empirClassProbs') <- empirClassProbs
   attr(outmat,'nclasses') <- m
   class(outmat) <- c('avalog','matrix')
   outmat
}

################################################################## 
# predict.avalog: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from avalogtrn()
#    predx:  as above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

avalogpred <- function() stop('user predict.avalog()')
predict.avalog <- function(object,...) 
{
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   n <- nrow(predpts)
   predpts <- factorsToDummies(predpts,omitLast=TRUE)
   if (!identical(colnames(predpts),attr(object,'Xcolnames')))
      stop('column name mismatch between original, new X variables')
   ypred <- vector(length = n)
   m <- attr(object,'nclasses')
   for (r in 1:n) {
      # predict the rth new observation
      xrow <- c(1,predpts[r,])
      # wins[i] tells how many times class i-1 has won
      wins <- rep(0,m)  
      ijs <- combn(m,2)  # as in avalogtrn()
      for (k in 1:ncol(ijs)) {
         # i,j play the role of Class 1,0
         i <- ijs[1,k]  # class i-1
         j <- ijs[2,k]  # class j-1
         bhat <- object[,k]
         mhat <- logitftn(bhat %*% xrow)  # prob of Class i
         if (mhat >= 0.5) wins[i] <- wins[i] + 1 else wins[j] <- wins[j] + 1
      }
      pred[r] <- which.max(wins) - 1
   }
   ypred
}

# deprecated
avalogloom <- function(m,trnxy) stop('deprecated')
## ##################################################################
## # avalogloom: LOOM predict Ys from Xs
## ##################################################################
## 
## # arguments: as with avalogtrn()
## 
## # value: LOOM-estimated probability of correct classification\
## 
## avalogloom <- function(m,trnxy) {
##    n <- nrow(trnxy)
##    p <- ncol(trnxy) 
##    i <- 0
##    correctprobs <- replicate(n,
##       {
##          i <- i + 1
##          avout <- avalogtrn(m,trnxy[-i,])
##          predy <- avalogpred(m,avout,trnxy[-i,-p])
##          mean(predy == trnxy[-i,p])
##       })
##    mean(correctprobs)
## }

logitftn <- function(t) 1 / (1+exp(-t))

matrixtolist <- function (rc,m) 
{
   if (rc == 1) {
      Map(function(rownum) m[rownum, ], 1:nrow(m))
   }
   else Map(function(colnum) m[, colnum], 1:ncol(m))
}

# kNN for classification, more than 2 classes

# uses One-vs.-All approach

# ovaknntrn: generate estimated regression function values

# arguments

#    trnxy:  matrix or dataframe, training set; Y col is a factor 
#    yname:  name of the Y column
#    k:  number of nearest neighbors
#    xval:  if true, "leave 1 out," ie don't count a data point as its
#        own neighbor

# value:

#    xdata from input, plus new list components: 
#
#       regest: the matrix of estimated regression function values; 
#               the element in row i, column j, is the probability 
#               that Y = j given that X = row i in the X data, 
#               estimated from the training set
#       k: number of nearest neighbors
#       empirClassProbs: proportions of cases in classes 0,1,...,m

knntrn <- function() stop('use ovaknntrn')
ovaknntrn <- function(trnxy,yname,k,xval=FALSE)
{
   if (is.null(colnames(trnxy))) 
      stop('trnxy must have column names')
   ycol <- which(names(trnxy) == yname)
   y <- trnxy[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   m <- ncol(yd)
   x <- trnxy[,-ycol,drop=FALSE]
   xd <- factorsToDummies(x,omitLast=TRUE)
   xdata <- preprocessx(xd,k,xval=xval)
   empirClassProbs <- table(y) / sum(table(y))
   knnout <- knnest(yd,xdata,k)
   xdata$regest <- knnout$regest
   xdata$k <- k
   xdata$empirClassProbs <- empirClassProbs
   class(xdata) <- c('ovaknn')
   xdata
}

# predict.ovaknn: predict multiclass Ys from new Xs

# arguments:  
# 
#    object:  output of knntrn()
#    predpts:  matrix of X values at which prediction is to be done
# 
# value:
# 
#    object of class 'ovaknn', with components:
#
#       regest: estimated class probabilities at predpts
#       predy: predicted class labels for predpts    

predict.ovaknn <- function(object,...) {
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   # could get k too, but not used; we simply take the 1-nearest, as the
   # prep data averaged k-nearest
   x <- object$x
   if (!is.data.frame(predpts))
      stop('prediction points must be a data frame')
   predpts <- factorsToDummies(predpts,omitLast=TRUE)
   # need to scale predpts with the same values that had been used in
   # the training set
   ctr <- object$scaling[,1]
   scl <- object$scaling[,2]
   predpts <- scale(predpts,center=ctr,scale=scl)
   tmp <- FNN::get.knnx(x,predpts,1)
   idx <- tmp$nn.index
   regest <- object$regest[idx,,drop=FALSE]
   predy <- apply(regest,1,which.max) - 1
   attr(predy,'probs') <- regest
   predy
}

# adjust a vector of estimated condit. class probabilities to reflect
# actual uncondit. class probabilities

# NOTE:  the below assumes just 2 classes, class 1 and class 0; however,
# it applies to the general m-class case, because P(Y = i | X = t) can
# be viewed as 1 - P(Y != i | X = t), i.e. class i vs. all others

# arguments:
 
#    econdprobs: estimated conditional probs. for class 1, for various t, 
#       reported by the ML alg.
#    wrongprob1: proportion of class 1 in the training data; returned as
#       attr() in, e.g. logitClass()
#    trueprob1: true proportion of class 1 
 
# value:
 
#     adjusted versions of econdprobs, estimated conditional class
#     probabilities for predicted cases

# why: say we wish to predict Y = 1,0 given X = t 
# P(Y=1 | X=t) = pf_1(t) / [pf_1(t) + (1-p)f_0(t)]
# where p = P(Y = 1); and f_i is the density of X within class i; so
# P(Y=1 | X=t) = 1 / [1 + {f_0(t)/f_(t)} (1-p)/p]
# and thus
# f_0(t)/f_1(t) = [1/P(Y=1 | X=t) - 1] p/(1-p)

# let q the actual sample proportion in class 1 (whether by sampling
# design or from a post-sampling artificial balancing of the classes);
# the ML algorith has, directly or indirectly, taken p to be q; so
# substitute and work back to the correct P(Y=1 | X=t)

# WARNING: adjusted probabilities may be larger than 1.0; they can be
# truncated, or in a multiclass setting compared (which class has the
# highest untruncated probability?)

classadjust <- function(econdprobs,wrongprob1,trueprob1) {
   wrongratio <- (1-wrongprob1) / wrongprob1
   fratios <- (1 / econdprobs - 1) * (1 / wrongratio)
   trueratios <- (1-trueprob1) / trueprob1
   1 / (1 + trueratios * fratios)
}

#######################  boundaryplot()  ################################

# for binary Y setting, drawing boundary between predicted Y = 1 and
# predicted Y = 0, as determined by the argument regests

# boundary is taken to be b(t) = P(Y = 1 | X = t) = 0.5

# purpose: visually assess goodness of fit, typically running this
# function twice, one for glm() then for say kNN() or e1071::svm(); if
# there is much discrepancy and the analyst wishes to still use glm(),
# he/she may wish to add polynomial terms or use the polyreg package

# arguments:

#   y01,x: Y vector (1s and 0s), X matrix or numerical data frame 
#   regests: estimated regression function values
#   pairs: matrix of predictor pairs to be plotted, one pair per column
#   cex: plotting symbol size
#   band: max distance from 0.5 for a point to be included in the contour 

boundaryplot <- function(y01,x,regests,pairs=combn(ncol(x),2),
   pchvals=2+y01,cex=0.5,band=0.10) 
{
   # e.g. fitted.values output of glm() may be shorter than an X column,
   # due to na.omit default
   if(length(regests) != length(y01))
      stop('y01 and regests of different lengths')

   # need X numeric for plotting
   if (is.data.frame(x)) {
      if (hasFactors(x)) stop('x must be numeric')
      x <- as.matrix(x)
      if (mode(x) != 'numeric') stop('x has character data')
   }

   # Y must be 0s,1s or equiv
   if (length(table(y01)) != 2) stop('y01 must have only 2 values')
   if (is.factor(y01)) y01 <- as.numeric(y01) - 1
   
   p <- ncol(x)
   for (m in 1:ncol(pairs)) {

      i <- pairs[1,m]
      j <- pairs[2,m]
      x2 <- x[,c(i,j)]

      # plot X points, symbols for Y
      plot(x2,pch=pchvals,cex=cex)  

      # add contour
      near05 <- which(abs(regests - 0.5) < band)
      points(x2[near05,],pch=21,cex=1.5*cex,bg='red')
      if (m < ncol(pairs)) readline("next plot")
   }
}

#########################  confusion matrix  ################################

# generates the confusion matrix

# for an m-class problem, 'pred' are the predicted class IDs,
# taking values in 1,2,...,m; if 'actual' is numeric, then the same
# condition holds, but 'actual' can be a factor, which when
# "numericized" uses the same ID scheme (must be consistent with
# 'actual')

confusion <- function(actual,pred) {
   if (is.factor(actual)) actual <- as.numeric(actual) 
   table(actual,pred)
}

#########################  labelsToProbs()  ################################

# generates estimated conditional class probabilities from predicted
# labels; useful for classification methodology that does not inherently
# generate those probabilities

# for a given new case, the k nearest neighbors in the training set are
# obtained; the predicted class labels for the neighbors are then
# obtained, and the resulting proportions for the different labels are
# then used as the estimated probabilities

# arguments:

#    x: the feature matrix of the training set; must be numeric
#    fittedY: R factor, predicted classes in the training set
#    newX: matrix of new cases to be predicted
#    classNames: levels(y) from the training set y
#    k: number of neighbors

labelsToProbs <- function(x,newX,fittedY,classNames,k) 
{
   if (!is.matrix(x)) {
      x <- as.matrix(x)
      if (!is.numeric(x)) stop('x must be numeric')
   }
   x <- scale(x)
   ctr <- attr(x,'scaled:center')
   scl <- attr(x,'scaled:scale')
   newX <- scale(newX,center=ctr,scale=scl)
   tmp <- FNN::get.knnx(x,newX,k)
   nClass <- length((classNames))
   doRowI <- function(i)  # do row i of newX
   {
      idxs <- tmp$nn.index[i,]
      nhbrFittedY <- fittedY[idxs]
      tblNhbrs <- table(nhbrFittedY)
      nhbrClasses <- names(tblNhbrs)
      probs <- rep(0,nClass)
      names(probs) <- classNames
      probs[nhbrClasses] = tblNhbrs / k
      probs
   }
   tmp <- t(sapply(1:nrow(newX),doRowI))
   tmp
}

logOddsToProbs <- function(x) 
{
   u <- exp(-x)
   1 / (1+u)
}

