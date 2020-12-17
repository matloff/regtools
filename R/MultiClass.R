
# multiclass models; see also th q*() functions

# deprecated:

ovalogtrn <- function(...) 
{
   stop('deprecated; use qLogit()')
}

predict.ovalog <- function(...) 
{
   stop('')
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
      if (hasFactors(x)) stop('x must be nrongkuiumeric')
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
   # if (is.factor(actual)) actual <- as.numeric(actual) 
   table(actual,pred)
}

#######################################################################
#######################  calibration  #################################
#######################################################################

# the *Calib() functions generate estimated conditional class
# probabilities from scores, e.g.  SVM decision values

# useful for classification methodology that does not inherently
# generate those probabilities, or for which those probabilities are
# biased or otherwise inaccurate

#########################  knnCalib()  ################################

# for a given new case, the k nearest neighbors in the training set are
# obtained; the class labels for the neighbors are then obtained, and
# the resulting proportions for the different labels are then used as
# the estimated probabilities

# arguments:

#    y: R factor of labels in training set
#    trnScores: vector/matrix of scores in training set
#    newScores: scores of new case(s)
#    k: number of nearest neighbors

# value: vector of estimated probabilities for the new cases

knnCalib <- function(y,trnScores,newScores,k) 
{
   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   if (is.vector(newScores))
      newScores <- matrix(newScores,ncol=1)
   tmp <- FNN::get.knnx(trnScores,newScores,k)
   classNames <- levels(y)
   nClass <- length(classNames)
   doRowI <- function(i)  # do row i of newScores
   {
      idxs <- tmp$nn.index[i,]
      nhbrY <- y[idxs]
      nhbrY <- toSuperFactor(nhbrY,levels(y))
      tblNhbrs <- table(nhbrY)
      nhbrClasses <- names(tblNhbrs)
      probs <- rep(0,nClass)
      names(probs) <- classNames
      probs[nhbrClasses] = tblNhbrs / k
      probs
   }
   tmp <- t(sapply(1:nrow(newScores),doRowI))
   tmp
}

scoresToProbs <- knnCalib


#########################  plattCalib()  ################################

# run the logit once and save, rather doing running repeatedly, each
# time we have new predictions to make

# arguments:

#    y, trnScores, newScores, value as above

prePlattCalib <- function(y,trnScores) 
{

   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   tsDF <- as.data.frame(trnScores)
   dta <- cbind(y,tsDF)
   res <- qeLogit(dta,'y')
}

plattCalib <- function(prePlattCalibOut,newScores) 
{
   if (is.vector(newScores))
      newScores <- matrix(newScores,ncol=1)
   tsDF <- as.data.frame(newScores)
   predict(prePlattCalibOut,tsDF)$probs
}

# isotonic regression, AVA

# y, trnScores, newScores, value as above

ExperimentalisoCalib <- function(y,trnScores,newScores)
{
stop('under construction')
   require(Iso)
   # find estimated regression function of yy on xx, at the values
   # newxx
   predictISO <- function(xx,yy,newxx)  # xx, yy, newxx numeric vectors
   {
      xo <- order(xx)
      xs <- xx[xo]  # sorted xx
      ys <- yy[xo]  # yy sorted according to xx
      newxxs <- matrix(newxx[xo],ncol=1)  # same for newxx
      isoout <- pava(ys)
      # get est. reg. ftn. value for each newxx; could interpolate for
      # improved accuracy, but good enough here
      minspots <- apply(newxxs,1,
         function(newxxsi) which.min(abs(newxxsi - xs)))
      isoout[minspots]
   }
   yn <- as.numeric(y)
   do1Pair <- function(ij) 
   {
      # require Iso
      # get pair
      i <- ij[1]
      j <- ij[2]
      # which has y = i or j?
      idxs <- which(yn == i | yn == j)
      # form subsets
      ys <- yn[idxs] - 1  # yn is 1s and 2s
      trnscores <- trnScores[idxs]
      # return probs for this pair
      predictISO(trnscores,ys,newScores)
   }
   pairs <- combn(length(levels(y)),2)
   apply(pairs,2,do1Pair)
}

#########################  isoCalib()  ################################

# wrapper calibrate either training scores or probability by isotonic 
# regression

# arguments

#    y: R factor of labels in training set
#    trnScores: vector/matrix of scores in training set
#    newScores: scores of new case(s)

isoCalib <- function(y,trnScores,newScores)
{
   require(CORElearn)
   model <- calibrate(y, 
      trnScores, 
      class1=1,
      method = "isoReg",
      assumeProbabilities=F)
   applyCalibration(newScores, model)
}

#########################  hist_bbq_guess_Calib()  ################################

# wrapper calibrate either training scores or probability by 
# hist_scaled, 
# hist_transformed,
# BBQ_scaled, 
# BBQ_transformed, 
# GUESS

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    newScores: scores of new case(s)
#    model_idx : which calibration models should be implemented, 
#     1=hist_scaled, 2=hist_transformed,3=BBQ_scaled, 
#     4=BBQ_transformed, 5=GUESS, Default: c(1, 2, 3, 4, 5)

hist_bbq_guess_Calib <- function(y,trnScores, newScores, model_idx=c(1, 2, 3, 4, 5))
{
   require(CalibratR)
   model <- calibrate(y, trnScores, model_idx = model_idx)
   predict_calibratR(model, new = newScores)
}


#########################  JOUSBoostCalib()  ################################

# wrapper calibrate by JOUSBoost
# algorithm. The algorithm requires ksvm from kernlab
# the arguments in ksvm are set according to the default
# of e1017:::svm()

# arguments

#    y: R factor of labels in training set, 
#       y must take values in -1, 1
#    X: standardized train set
#    newx: standardized test set

JOUSBoostCalib <- function(y,X,newx)
{
   require(JOUSBoost)
   require(kernlab)

   # Use the kernlab svm function with radial kernel
   # the following follows the JOUSBoost manual 
   class_func <- function(X, y)
   {
      ksvm(X,
         as.factor(y), 
         kernel = 'rbfdot',
         scaled = FALSE, 
         kpar = list(sigma =  if (is.vector(X)) 1 else 1 / ncol(X)),
         nu=0.5,
         C=1)  
   } 
   pred_func <- function(obj, X)
   { 
      as.numeric(as.character(predict(obj, X))) 
   }

   jous_obj <- jous(X, y, class_func = class_func,
      pred_func = pred_func, keep_models = TRUE)

   predict(jous_obj, newx, type = 'prob')

}

#########################  eliteCalib()  ################################

# wrapper calibrate by ELiTe
# arguments

#    y: vector of corresponding true class. 
#        1 indicates positive class and 0 indicates negative class.
#    trnProb: vector of uncalibrated classification scores 
#        that are real numbers in the interval of [0,1]
#    newProb: vector of uncalibratd classification probability

eliteCalib <- function(y,trnProb,newProb)
{
   require(glmgen)
   require(ELiTE)

   model <- elite.build(trnProb, y)
   elite.predict(model, newProb)
}

#########################  calibWrap()  ################################

# wrapper; calibrate all variables in the training set, apply to new
# data

# arguments

#     qeout: output of a qe*-series function
#     trnScores: vector/matrix of scores output from running the
#        classification method on the training set; will have either c
#        or c(c-1)/2 columns, where c is the number of classes
#     newScores: scores for the data to be predicted
#     calibMethod: currently knnCalib or plattCalib
#     k: number of nearest neighbors (knnCalib case)
#     plotsPerRow: number of plots per row; 0 means no plotting

calibWrap <- function(trnY,tstY,trnScores,newScores,calibMethod,k=NULL,
   plotsPerRow=2,nBins=0,se=FALSE) 
{
stop('under construction')
   classNames <- levels(trnY)
   nClass <- length(classNames)
   if (calibMethod == 'knnCalib') {
      probs <- knnCalib(trnY,trnScores,newScores,k)
   } else if (calibMethod == 'plattCalib') {
      preout <- prePlattCalib(trnY,trnScores)
      ### plattOut <- plattCalib(preout,newScores,se=se)
      if (se) {
         probs <- plattOut$probs
         se <- plattOut$se
         res <- list(probs=probs,ym=ym, se = se)
      } else {
         probs = plattOut  
         res <- list(probs=probs,ym=ym)
      }
   } else stop('invalid calibration method')
   ym <- factorToDummies(tstY,fname='y')
   res <- list(probs=probs,ym=ym)
   if (plotsPerRow) {
      nRow <- ceiling(nClass/plotsPerRow)
      par(mfrow=c(nRow,plotsPerRow))
      for (rw in 1:nRow) {
         start <- (rw - 1) * plotsPerRow + 1
         end <- min(rw * plotsPerRow,nClass)
         for (cls in start:end) {
            tmp <- 
               reliabDiagram(ym[,cls],probs[,cls],nBins,TRUE)
         }
      }
      par(mfrow=c(1,1))
   }
   res
}

# calculcate decision values ("scores") for new cases on previously-fit
# e1071 SVM
getDValsE1071 <- function(object,newx) 
{
   require(e1071)
   # need to strip off non-SVM classes, if any
   toDelete <- NULL
   for (i in 1:length(class(object))) {
      if (!class(object)[i] %in% c('svm.formula','svm')) 
         toDelete <- c(toDelete,i)
   }
   if (length(toDelete) > 0) class(object) <- class(object)[-toDelete]
   tmp <- predict(object,newx,decision.values=TRUE)
   attr(tmp,'decision.values')
}

# reliability diagram and associated compilation of statistics

# arguments:

#    y: vector of Y values from training set, 0s and 1s
#    probs: vector of estimated cond. class probabilities
#    nBins: number of bins
#    plotGraph: TRUE means plotting is desired

reliabDiagram <- function(y,probs,nBins,plotGraph=TRUE) 
{
   breaks <- seq(0,1,1/nBins)
   probsBinNums <- findInterval(probs,breaks)
   fittedYCounts <- tapply(probs,probsBinNums,sum)
   actualYCounts <- tapply(y,probsBinNums,sum)
   if (plotGraph) {
      plot(fittedYCounts,actualYCounts)
      abline(0,1)
   }
   cbind(fittedYCounts,actualYCounts)
}

logOddsToProbs <- function(x) 
{
   u <- exp(-x)
   1 / (1+u)
}

# arguments:

#    y: labels in training set; a 0s and 1s vector 
#    scores: values that your ML algorithm predicts from

ROC <- function(y,scores) 
{
   n <- length(y)
   numPos <- sum(y)
   numNeg <- n - numPos
   scoreOrder <- order(scores,decreasing=T)
   tpr <- vector(length = n)
   fpr <- vector(length = n)
   for (i in 1:n) {
      # scoresSorted = sort(scores); h = scoresSorted[i]
      whichGteH <- scoreOrder[1:i]
      numTruePos <- sum(y[whichGteH] == 1)
      numFalsePos <- i - numTruePos
      tpr[i] <- numTruePos / numPos
      fpr[i] <- numFalsePos / numNeg
   }
   plot(fpr,tpr,type='l',pch=2,cex=0.5)
   abline(0,1)

}

#########################  multi_calibWrap()  ################################

# wrapper; it plots relibability diagram for each algorithm 
# 
# arguments:
# formula: a formula like: class ~ Lin_Platt+Quad_Platt+KNN+IsoReg+BBQ+JOUSBoost
# where class is the test labels and the right side is the probability output
# by each algorithm
# df (data.frame ): the dataframe that contains all probabilities output 
# by each calib method and the test labels
# num_algorithms (numeric): the number of calibration methods
# title (character): the title of the plot

multi_calibWrap <- function(formula, df, num_algorithms, title)
{
   require(caret)
   require(ggplot2)
   cal_obj <- calibration(formula,
                         data = df,
                         cuts = 10)
  p <- plot(cal_obj, type = "o", auto.key = list(columns = num_algorithms,
                                            lines = TRUE,
                                            points = TRUE),
            main=title)
  
  g <- ggplot(cal_obj)
}

#########################  crossEntropy()  ################################

# it calculates crossEntropy for probability calibrationn algorithms
# 
# arguments:
# calibWrapOut: output of function calibWrap() 

crossEntropy = function(calibWrapOut) {
   p = calibWrapOut$ym
   phat = calibWrapOut$probs
   x = 0
   for (i in 1:nrow(p)) {
      x = x - sum(p[i,]*log(phat[i,]))
   }  
   return(x)
}

#########################  KLDivergence()  ################################

# it calculates Kullback_Leibler divergence for probability calibrationn 
# algorithms
# 
# arguments:
# calibWrapOut: output of function calibWrap() 

KLDivergence = function(calibWrapOut) {
   require(philentropy)
   p = calibWrapOut$ym
   phat = calibWrapOut$probs
   x = 0
   for (i in 1:ncol(p)) {
      df = rbind(p[i,], phat[i,])
      x = x + philentropy::KL(df)
   }
   return(x)
}

#########################  sim4()  ################################

# simulated a dataset of 10 multi-variable normally distributed 
# ddependent variables and 1 dependent variable of four classes.
# The true probability of each class for each sample is also 
# given. 
#
# arguments: seed for random number generation (default 2); n for 
# number of samples (default 11000)

sim4 <- function(seed = 2, n = 11000)
{
   set.seed(seed)
   
   k <- 10
   A <- matrix(runif(k^2)*2-1, ncol=k) 
   Sigma <- t(A) %*% A
   
   x = mvrnorm(n = n, mu = rep(0, 10), 
               Sigma = Sigma)
   x1 = x[,1]
   x2 = x[,2]
   x3 = x[,3]
   x4 = x[,4]
   x5 = x[,5]
   x6 = x[,6]
   x7 = x[,7]
   x8 = x[,8]
   x9 = x[,9]
   x10 = x[,10]
   
   means = rnorm(20, mean = 0, sd = 10)
   sds = rexp(20, rate = 3)
   params = c()
   for (i in 1:20) {
      param = rnorm(1, mean = means[i], sd = sds[i])
      params = c(params, param)
   }
   p1 = inv.logit(params[1]*x1*x2 + params[2]*x3*x4 + params[3]*x5*x6 + 
                     params[4]*x7*x8 + params[5]*x9*x10)
   p2 = inv.logit(params[6]*x1*x3 + params[7]*x4*x5 + params[8]*x6*x7 + 
                     params[9]*x8*x9 + params[10]*x2*x10)
   p3 = inv.logit(params[11]*x1*x5*x10 + params[12]*x4*x8)
   p4 = inv.logit(params[13]*x1 + params[14]*x2 + params[15]*x3 + 
                     params[16]*x4 + params[17]*x5 + params[18]*x6 + params[19]*x7 +
                     params[20]*x8)
   psum = p1 + p2 + p3 + p4
   p1 = p1/psum
   p2 = p2/psum
   p3 = p3/psum
   p4 = p4/psum
   ps = data.frame(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
   
   y = c()
   for (i in 1:nrow(ps)) {
      item = sample(c("1","2","3","4"), 1, prob = ps[i,])
      y = c(y, item)
   }
   
   sim4 = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6,
                     x7 = x7, x8 = x8, x9 = x9, x10 = x10, y = as.factor(y), 
                     p1 = p1, p2 = p2, p3 = p3, p4 = p4)
   sim4.std.cols = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
   
   data = list(sim4 = sim4, sim4.std.cols = sim4.std.cols)
}


