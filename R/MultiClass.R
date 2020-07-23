
# One-vs.-All (OVA) and All-vs.All (AVA), parametric models

##################################################################
# logitClass: generate estimated regression functions
##################################################################

# arguments:

#    dta:  dataframe, training set; class labels col is a factor 
#    yName:  name of the class labels column

# value:

#    matrix of the betahat vectors, one class per column; via attr(),
#    also empirical class probabilities, column names

ovalogtrn <- function(...) 
{
   stop('deprecated; use logitClass()')
}

logitClass <- function(dta,yName) 
{
   if (!is.data.frame(dta)) 
      stop('dta must be a data frame')
   ycol <- which(names(dta) == yName)
   y <- dta[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   x <- dta[,-ycol,drop=FALSE]
   classNames <- levels(y)
   yDumms <- factorToDummies(y,'',omitLast=FALSE)
   colnames(yDumms) <- classNames
   yDumms <- as.data.frame(yDumms)
   xy <- cbind(x,yDumms)


   m <- length(levels(y))
   ncxy <- ncol(xy)
   nx <- ncxy - m 
   x <- xy[,1:nx]
   y <- xy[,-(1:nx)]
   xDumms <- factorsToDummies(x,omitLast=TRUE)
   xy <- cbind(xDumms,y)
   nx <- ncol(xy) - m
   outmat <- matrix(nrow=nx+1,ncol=m)
   attr(outmat,'Xcolnames') <- colnames(x)
   # outmat has 1 col for each of the m sets of coefficients
   for (i in 1:m) {
      ycol <- nx + i
      yCol <- xy[,ycol]
      tmpxy <- cbind(x,yCol)
      betahat <- coef(glm(yCol ~ .,family=binomial,data=tmpxy))
      outmat[,i] <- betahat
   }
   if (any(is.na(outmat))) warning('some NA coefficients')
   colnames(outmat) <- colnames(y)
   empirclassprobs <- colMeans(y)  # for classAdjust() if needed
   attr(outmat,'empirclassprobs') <- empirclassprobs
   class(outmat) <- c('logitClass','matrix')
   outmat
}

##################################################################
# predict.logitClass: predict Ys from new Xs
##################################################################

# arguments:  

#    object:  coefficient matrix, output from logitClass()
#    predpts:  points to be predicted; named argument
 
# value:
 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

predict.ovalog <- function(...) 
{
   stop('')
}

predict.logitClass <- function(object,...) 
{
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   predpts <- factorsToDummies(predpts,omitLast=TRUE)
   if (!identical(colnames(predpts),attr(object,'Xcolnames')))
      stop('column name mismatch between original, new X variables')
   # get est reg ftn values for each row of predpts and each col of
   # coefmat; vals from coefmat[,i] in tmp[,i]; 
   # say np rows in predpts, i.e. np new cases to predict, and let m be
   # the number of classes; then tmp is np x m
   tmp <- cbind(1,predpts) %*% object  # np x m 
   tmp <- logitftn(tmp)
   # separate logits for the m classes will not necessrily sum to 1, so
   # normalize
   sumtmp <- apply(tmp,1,sum)  # length np
   tmp <- (1/sumtmp) * tmp
   preds <- apply(tmp,1,which.max) - 1
   attr(preds,'probs') <- tmp
   preds
}

##################################################################
# logitClassloom: LOOM predict Ys from Xs
##################################################################


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
   empirclassprobs <- classcounts/sum(classcounts)
   attr(outmat,'empirclassprobs') <- empirclassprobs
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
      ypred[r] <- which.max(wins) - 1
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
#       empirclassprobs: proportions of cases in classes 0,1,...,m

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
   empirclassprobs <- table(y) / sum(table(y))
   knnout <- knnest(yd,xdata,k)
   xdata$regest <- knnout$regest
   xdata$k <- k
   xdata$empirclassprobs <- empirclassprobs
   class(xdata) <- c('ovaknn')
   xdata
}

xClassGetXY <- function(dta,yName) 
{

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

#######################  linClass()  ################################

# uses multivariate (i.e. vector R) lm() for classification; faster than
# glm(), and may be useful as a rough tool if the goal is prediction, 
# esp. if have some quadratic terms

# arguments:

#    dta: data frame, one column for class, rest for features; class
#       column must be an R factor
#    yName: name of the class column

# value:

#    object of class 'linClass'
linClass <- function(dta,yName) {
   if (!is.data.frame(dta)) 
      stop('dta must be a data frame')
   y <- dta[[yName]]
   if (!is.factor(y)) stop('class column must be an R factor')
   yIdx <- which(names(dta) == yName)
   x <- dta[,-yIdx]
   classNames <- levels(y)
   yDumms <- factorToDummies(y,'',omitLast=FALSE)
   colnames(yDumms) <- classNames
   yDumms <- as.data.frame(yDumms)
   xy <- cbind(x,yDumms)

   yNames <- paste0(classNames,collapse=',')
   cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .,data=xy)')
   eval(parse(text=cmd))
   class(lmout) <- c('linClass',class(lmout))
   lmout
}

# linClassObj is output of linClass(), newx is a data frame compatible with x
# in linClass(); output is the most likely class label

predict.linClass <- function(linClassObj,newx) {
   class(linClassObj) <- class(linClassObj)[-1]
   preds <- predict(linClassObj,newx)
   tmp <- apply(preds,1,which.max)
   colnames(preds)[tmp]
}

#########################  confusion matrix  #################################

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



