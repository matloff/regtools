
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
#    tstScores: scores of new case(s)
#    k: number of nearest neighbors

# value: vector of estimated probabilities for the new cases

knnCalib <- function(y,trnScores,tstScores,k) 
{
   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   if (is.vector(tstScores))
      tstScores <- matrix(tstScores,ncol=1)
   tmp <- FNN::get.knnx(trnScores,tstScores,k)
   classNames <- levels(y)
   nClass <- length(classNames)
   doRowI <- function(i)  # do row i of tstScores
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
   tmp <- t(sapply(1:nrow(tstScores),doRowI))
   tmp
}

scoresToProbs <- knnCalib


#########################  plattCalib()  ################################

# run the logit once and save, rather doing running repeatedly, each
# time we have new predictions to make
# author: Norm, Kenneth

# arguments:

#    y, trnScores, tstScores, value as above
#    degree: the degree of polynomial for the logistic model

prePlattCalib <- function(y,trnScores,deg) 
{
   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   tsDF <- as.data.frame(trnScores)
   if (nrow(trnScores) != length(y)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }
   dta <- cbind(y,tsDF)
   res <- qePolyLog(dta,'y',deg=deg,maxInteractDeg=0,holdout=NULL)
}

ppc <- prePlattCalib

plattCalib <- function(prePlattCalibOut,tstScores,se=FALSE) 
{
   if (is.vector(tstScores)) {
      tstScores <- matrix(tstScores,ncol=1)
   }
   tsDF <- as.data.frame(tstScores)
   probs <- predict(prePlattCalibOut,tsDF)$probs
   if(se) {
      require("RcmdrMisc")
      SEs = list()
      for (i in 1:length(levels(as.factor(prePlattCalibOut$y))))
      {
         model = prePlattCalibOut$glmOuts[[i]]
         nscores = ncol(tstScores)
         
         alg = "1/(1+exp((-1)*(b0"
         for (j in 1:nscores) {
            alg = paste(alg,"+b",j,"*",colnames(tstScores)[j], sep = "")
         }
         alg = paste(alg,")))", sep = "")
         SE = DeltaMethod(model,alg)$test$SE
         SEs[[i]] = SE
      }
      df.SEs = do.call(rbind, SEs)
      return(list(probs = probs, se = df.SEs))
   } else {
      return(list(probs = probs))
   }
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

# author: kenneth

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
   CORElearn:::applyCalibration(newScores, model)
}

#########################  bbqCalib()  ################################

# wrapper calibrate either training scores or probability by 
# Bayesian Binning 
# reference: https://cran.r-project.org/web/packages/CalibratR/CalibratR.pdf

# author: kenneth

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    newScores: scores of new case(s)


bbqCalib <- function(y,trnScores, newScores)
{
   require(CalibratR)
   bbqmod <-  CalibratR:::build_BBQ(y, trnScores)
   # the paper suggests that However, model averaging 
   # is typically superior to model selection (Hoeting et al. 1999)
   # so we use option = 1 for predict_BBQ
   CalibratR:::predict_BBQ(bbqmod, test.scores, 1)
}

#########################  guessCalib()  ################################

# wrapper calibrate either training scores or probability by 
# a GUESS calibration model 
# reference: https://cran.r-project.org/web/packages/CalibratR/CalibratR.pdf

# author: kenneth

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    newScores: scores of new case(s)


guessCalib <- function(y,trnScores, newScores)
{
   require(CalibratR)
   bbqmod <-  CalibratR:::build_GUESS(y, trnScores)
   # the paper suggests that However, model averaging 
   # is typically superior to model selection (Hoeting et al. 1999)
   # so we use option = 1 for predict_BBQ
   CalibratR:::predict_GUESS(bbqmod, test.scores)
}




#########################  JOUSBoostCalib()  ################################

# wrapper calibrate by JOUSBoost
# algorithm. The algorithm requires ksvm from kernlab
# the arguments in ksvm are set according to the default
# of e1017:::svm()

# author: kenneth

# arguments

#    y: R factor of labels in training set, 
#       y must take values in -1, 1
#    X: standardized train set
#    newx: standardized test set
#    class_func: 
JOUSBoostCalib <- function(y,X,newx, class_func)
{
   require(JOUSBoost)

   pred_func <- function(obj, X)
   { 
      as.numeric(as.character(predict(obj, X))) 
   }

   jous_obj <- jous(X, y, class_func = class_func,
      pred_func = pred_func, keep_models = TRUE)

   predict(jous_obj, X = newx, type = 'prob')

}

#########################  eliteCalib()  ################################

# wrapper calibrate by ELiTe
# author: kenneth
# arguments

#    y: vector of corresponding true class. 
#        1 indicates positive class and 0 indicates negative class.
#    trnScores: vector of uncalibrated decisions scores for training
#    newScore: vector of uncalibratd decisions scores for testing

eliteCalib <- function(y,trnScores,newScores)
{
   #require(devtools)
   #install_github("statsmaths/glmgen", subdir="R_pkg/glmgen")
   require(glmgen)
   #follow instruction on 
   # https://github.com/pakdaman/calibration/tree/master/ELiTE/R
   # to install EliTE
   require(ELiTE) 
   
   eliteMod <- elite.build(trnScores, y)
   elite.predict(eliteMod, newScores)

}

#########################  getCalibMeasure()  ################################

# wrapper of EliTe error measure for calibration
# author: kenneth
# arguments
# y : vector of true class of instances {0,1} 
# scores : vector of predictions (classification scores) which is in the interval [0, 1]

getCalibMeasure <- function(y, scores){
   require(glmgen)
   require(ELiTE) 
   require(philentropy)

   df <- as.data.frame(elite.getMeasures(scores, y))

   # compute cross entropy
   df$crossEntropy <- 0 - sum(log(scores)*y)

   # computer KL divergence
   distribution <- data.frame(prob=scores, class=y)
   df$kl_divergence <- philentropy::KL(distribution)
   return(df)
}


####################  calibWrap() and preCalibWrap()  #############################

# calibWrap() is a wrapper; calibrate model in the training set, apply to test data

# preCalibWrap() can be used on the orginal dataset, to set the holdout
# set, run the model etc.

preCalibWrap <- function(dta,yName,qeFtn='qeSVM',qeArgs=NULL,holdout=500)
{
   qecall <- paste0('qeout <- ',qeFtn,'(dta,"',yName,'",',qeArgs,',
      holdout=',holdout,')') 
   eval(parse(text=qecall))

   tmp <- substitute({
   tstIdxs <- qeout$holdIdxs
   trnIdxs <- setdiff(1:nrow(dta),tstIdxs)
   ycol <- which(names(dta) == yName)
   trnX <- dta[trnIdxs,-ycol]
   trnY <- dta[trnIdxs,ycol]
   tstX <- dta[tstIdxs,-ycol]
   tstY <- dta[tstIdxs,ycol]

   if (qeFtn == 'qeSVM') {
      trnScores <- qeout$decision.values
      tstScores <- getDValsE1071(qeout,tstX)
      trnScores <- rmSlashesE1071(trnScores)
      tstScores <- rmSlashesE1071(tstScores)
      startsWithDigit <- function(s) {
         s <- substr(s,1,1)
         s >= '0' && s <= '9'
      }
      cols <- colnames(trnScores)
      if (any(sapply(cols,startsWithDigit))) {
         colnames(trnScores) <- paste0('a',cols)
         colnames(tstScores) <- colnames(trnScores)
      }
   }
   })

   eval(tmp, parent.frame())
}

# arguments

#     
#     trnScores: vector/matrix of scores output from running the
#        classification method on the training set; will have either c
#        or c(c-1)/2 columns, where c is the number of classes
#     tstScores: scores for the data to be predicted
#     calibMethod: currently knnCalib or plattCalib
#     opts: R list of classification-specific parameters, e.g.
#        list(k = 50) for knnCalib
#     plotsPerRow: number of plots per row; 0 means no trellis plotting
#     oneAtATime: if TRUE, show the plots one at a time, and give the
#        user the option to print and/or zoom in

calibWrap <- function(trnY,tstY,trnX,tstX,trnScores,tstScores,calibMethod,
   opts=NULL,nBins=25,se=FALSE,plotsPerRow=0,oneAtATime=TRUE) 
{
   require(kernlab)
   classNames <- levels(trnY)
   nClass <- length(classNames)
   ym <- factorToDummies(tstY,fname='y')
   
   if (calibMethod == 'knnCalib') {
      k <- opts$k
      probs <- knnCalib(trnY,trnScores,tstScores,k)
      res <- list(probs=probs,ym=ym)
   } else if (calibMethod == 'plattCalib') {
      deg <- opts$deg
      preout <- prePlattCalib(trnY,trnScores,deg)
      plattOut <- plattCalib(preout,tstScores,se=se)
      if (se) {
         probs <- plattOut$probs
         se <- plattOut$se
         res <- list(probs=probs,ym=ym,se=se)
      } else {
         probs = plattOut$probs
         res <- list(probs=probs,ym=ym)
      }
   } else if (calibMethod == 'isoCalib') {

      isoProbs = list()
      for (pair in colnames(trnScores)){
         pred <- isoreg(x = tstScores[,which(colnames(tstScores) == pair)])
         isoProbs[[pair]] = pred$yf
      }
      
      isoProb = do.call(cbind, isoProbs)
      
      pwc_isoProb = apply(isoProb, 1, PairwiseCoupling, 
                           K = length(levels(trnY)))
      pwc_isoProb = t(pwc_isoProb)
      res <- list(probs=pwc_isoProb,ym=ym)
      
   } else if (calibMethod == 'BBQCalib') {
      require(CalibratR)
      
      bbqProbs = list()
      for (pair in colnames(trnScores)) {
         g1 = strsplit(pair, "/")[[1]][1]
         g2 = strsplit(pair, "/")[[1]][2]
         colidx = which(colnames(trnScores) == pair)
         
         y_binary <- ifelse(trnY==g1, 1, 
                            ifelse(trnY==g2, 0, NA))
         
         y_select = y_binary[!is.na(y_binary)]
         trnScores_select = trnScores[!is.na(y_binary),colidx]
         tstScores_select = tstScores[,colidx]
         
         bbqmod <-  CalibratR:::build_BBQ(y_select, trnScores_select)
         # the paper suggests that However, model averaging 
         # is typically superior to model selection (Hoeting et al. 1999)
         # so we use option = 1 for predict_BBQ
         pred <-  CalibratR:::predict_BBQ(bbqmod, tstScores_select, 1)
         bbqProbs[[pair]] = pred$predictions
      }
      
      bbqProb = do.call(cbind, bbqProbs)
      
      pwc_bbqProb = apply(bbqProb, 1, PairwiseCoupling, 
                          K = length(levels(trnY)))
      pwc_bbqProb = t(pwc_bbqProb)
      res <- list(probs=pwc_bbqProb,ym=ym)
      
   } else if (calibMethod == 'ELiTECalib') {
      #stop("under construction")
      require(ELiTE) 
      #https://github.com/pakdaman/calibration/tree/master/ELiTE/R
      
      eliteProbs = list()
      for (pair in colnames(trnScores)) {
         g1 = strsplit(pair, "/")[[1]][1]
         g2 = strsplit(pair, "/")[[1]][2]
         colidx = which(colnames(trnScores) == pair)
         
         y_binary <- ifelse(trnY==g1, 1, 
                            ifelse(trnY==g2, 0, NA))
         
         y_select = y_binary[!is.na(y_binary)]
         trnScores_select = trnScores[!is.na(y_binary),colidx]
         tstScores_select = tstScores[,colidx]
         
         elitemod <-  elite.build(y_select, trnScores_select)
         pred <- elite.predict(elitemod, tstScores_select, 1)
         eliteProbs[[pair]] = pred
      }
      
      eliteProb = do.call(cbind, eliteProbs)
      
      pwc_eliteProb = apply(eliteProb, 1, PairwiseCoupling, 
                          K = length(levels(trnY)))
      pwc_eliteProb = t(pwc_eliteProb)
      res <- list(probs=pwc_eliteProb,ym=ym)
      
   } else if (calibMethod == 'JOUSCalib') {
      
      JOUSprobs = list()
      for (pair in colnames(trnScores)) {
         g1 = strsplit(pair, "/")[[1]][1]
         g2 = strsplit(pair, "/")[[1]][2]
         y_binary <- ifelse(trnY==g1, 1, 
                            ifelse(trnY==g2, -1, NA))
         y_select = y_binary[!is.na(y_binary)]
         trnX_select = trnX[!is.na(y_binary),]
         JOUSprob = JOUSBoostCalib(y_select,as.matrix(trnX_select),as.matrix(tstX))
         JOUSprobs[[pair]] = JOUSprob
      }
      JOUSprob = do.call(cbind, JOUSprobs)
      
      pwc_JOUsprob = apply(JOUSprob, 1, PairwiseCoupling, 
                           K = length(levels(trnY)))
      pwc_JOUsprob = t(pwc_JOUsprob)
      res <- list(probs=pwc_JOUsprob,ym=ym)
   } else stop('invalid calibration method')
   
   if (plotsPerRow) {
      nRow <- ceiling(nClass/plotsPerRow)
      par(mfrow=c(nRow,plotsPerRow))
      for (rw in 1:nRow) {
         start <- (rw - 1) * plotsPerRow + 1
         end <- min(rw * plotsPerRow,nClass)
         for (cls in start:end) {
            tmp <- 
               reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE)
         }
      }
      par(mfrow=c(1,1))
   } else if (oneAtATime) {
      for (cls in 1:nClass) {
         reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,classNum=cls)
         while (1) {
            print('you can go to the next plot, or zoom, or print to file')
            cmd <- 
               readline('hit Enter for next plot, or low hi or fname: ')
            if (cmd == '') break
            cmdParts <- pythonBlankSplit(cmd)
            if (length(cmdParts) == 1)
               prToFile(cmd)
            else {
               if (cmdParts[2] == 'ly') cmdParts[2] <- as.numeric(nrow(ym))
               zoom <- as.numeric(cmdParts)
               reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,zoom=zoom,
                  classNum=cls)
            }
         }
      }
   }
   res
}

##########################################################################
########################  e1017 routines  ################################
##########################################################################

# for those users of the probability calibration functions on output
# from the e1071 package, here are useful utilities

# replace '/' by '_' in the colnames of the decision values

rmSlashesE1071 <- function(dvals) 
{
   colnames(dvals) <- gsub('/','_',colnames(dvals)) 
   dvals
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

reliabDiagram <- function(y,probs,nBins,plotGraph=TRUE,zoom=NULL,classNum=NULL) 
{
   breaks <- seq(0,1,1/nBins)
   probsBinNums <- findInterval(probs,breaks)
   fittedYCounts <- tapply(probs,probsBinNums,sum)
   actualYCounts <- tapply(y,probsBinNums,sum)
   axisLimit <- max(max(fittedYCounts),max(actualYCounts))
   if (plotGraph) {
      if (is.null(zoom)) {
         zoomTo <- 1:nBins
         lims <- c(0,axisLimit) 
      } else {
         ftdy <- fittedYCounts
         zoomTo <- which(ftdy >= zoom[1] & ftdy <= zoom[2])
         lims <- zoom
      }
      plot(fittedYCounts[zoomTo],actualYCounts[zoomTo], type="b"
         xlim=lims,ylim=lims,xlab='fittedYCounts',ylab='actualYCounts')
      abline(0,1,col='red')
      if (!is.null(classNum)) {
         topLabel <- paste('Class',classNum)
         title(main=topLabel,col='blue')
      }
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
# author: kenneth

# arguments:
# formula: a formula like: class ~ Lin_Platt+Quad_Platt+KNN+IsoReg+BBQ+JOUSBoost
# where class is the test labels and the right side is the probability output
# by each algorithm
# df (data.frame ): the dataframe that contains all probabilities output 
# by each calib method and the test labels
# classvalue: specify the class index (e.g. 0, 1, -1) for calibration
# num_algorithms (numeric): the number of calibration methods
# title (character): the title of the plot

multi_calibWrap <- function(formula, 
   df, 
   classvalue, 
   num_algorithms, 
   title="Calibration plot"){

  
  cal_obj <- calibration(formula,
                         data = df,
                         class=classvalue,
                         cuts=10)

  p <- plot(cal_obj, type = "o", auto.key = list(rows= num_algorithms,
                                            lines = TRUE,
                                            points = TRUE),
            main=title,
            xlab='Predicted Percentage',
            ylab='Actual Percentage',
            ylim = extendrange(c(0, 100)),
            xlim = extendrange(c(0, 100)))
  
  g <- ggplot(cal_obj)+
    ggtitle(title)
  return(list(pplot = p, gplot = g))
  
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

# it calculates Kullback_Leibler divergence for probability calibration 
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

#########################  PairwiseCoupling()  ################################

# it uses pairwise coupling first described in Hastie & Tibshirani 1998 
# and the algorithm proposed in Wu et al., 2003 to calculate class 
# probability from pairwise probabilities generated in AVA classifiation.
# 
# arguments:
# K: number of classess
# probs: pairwise probabilities of one sample

# code credit:
# Claas Heuer, September 2015
#
# Reference: Probability Estimates for Multi-class Classification by Pairwise Coupling, Wu et al. (2003)

PairwiseCoupling = function(K, probs)
{
   Q <- matrix(0,K,K)
   Q[lower.tri(Q)] <- 1 - probs
   Qt <- t(Q)
   Q[upper.tri(Q)] <- 1 - Qt[upper.tri(Qt)]
   diag(Q) <- rowSums(Q)
   Q <- Q / (K-1)
   
   p <- rbeta(K,1,1)
   p <- p/sum(p)
   
   # updating the prob vector until equilibrium is reached
   for(i in 1:1000) p <- Q%*%p
   
   out = round(t(p), digits=4)
   
   return(out)
}
