
# One-vs.-All (OVA) and All-vs.All (AVA), parametric models

##################################################################
# ovalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    m:  number of classes
#    trnxy:  matrix, X, Y training set; Y in last m columns 

# value:

#    matrix of the betahat vectors, one class per column

ovalogtrn <- function(m,trnxy) {
   if (!is.matrix(trnxy))
      stop('trnxy must be a matrix, with dummy Y cols')
   p <- ncol(trnxy) 
   x <- trnxy[,1:(p-m)]
   y <- trnxy[,(p-m+1):p]
   # 1 col for each of the m sets of coefficients
   outmat <- matrix(nrow=ncol(x)+1,ncol=m)
   for (i in 1:m) {
      betahat <- coef(glm(y[,i] ~ x,family=binomial))
      outmat[,i] <- betahat
   }
   colnames(outmat) <- as.character(0:(m-1))
   empirclassprobs <- colMeans(y)
   attr(outmat,'empirclassprobs') <- empirclassprobs
   class(outmat) <- c('ovalog','matrix')
   outmat
}

##################################################################
# ovalogpred: predict Ys from new Xs
##################################################################

# arguments:  

#    coefmat:  coefficient matrix, output from ovalogtrn()
#    predx:  as above
#    probs:  in addition to predicted classes, return the condtional
#            class probabilities as an attribute, 'probs'
#    trueclassprobs:  known correct (or estimated correct) unconditional
#                     class probabilities
 
# value:
 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

ovalogpred <- function() stop('user predict.ovalog()')
predict.ovalog <- function(object,...) 
{
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   trueclassprobs <- dts$trueclassprobs
   # get est reg ftn values for each row of predpts and each col of
   # coefmat; vals from coefmat[,i] in tmp[,i]; 
   # say np rows in predpts, i.e. np new cases to predict, and let m be
   # the number of classes; then tmp is np x m
   tmp <- cbind(1,predpts) %*% object  # np x m 
   tmp <- logitftn(tmp)
   if (!is.null(trueclassprobs)) {
      empirprobs <- attr(coefmat,'empirclassprobs')
      trueprobs <- trueclassprobs
      for (i in 1:ncol(tmp)) {
         tmp[,i] <- classadjust(tmp[,i],empirprobs[i],trueprobs[i])
      }
   }
   # separate logits for the m classes will not necessrily sum to 1, so
   # normalize
   sumtmp <- apply(tmp,1,sum)  # length np
   tmp <- (1/sumtmp) * tmp
   preds <- apply(tmp,1,which.max) - 1
   attr(preds,'probs') <- tmp
   preds
}

##################################################################
# ovalogloom: LOOM predict Ys from Xs
##################################################################

# arguments: as with ovalogtrn()

# value: LOOM-estimated probability of correct classification\

ovalogloom <- function(m,trnxy) {
   n <- nrow(trnxy)
   p <- ncol(trnxy) 
   i <- 0
   correctprobs <- replicate(n,
      {
         i <- i + 1
         ovout <- ovalogtrn(m,trnxy[-i,])
         predy <- ovalogpred(ovout,trnxy[-i,-p])
         mean(predy == trnxy[-i,p])
      })
   mean(correctprobs)
}


##################################################################
# avalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    m:  as above in ovalogtrn()
#    trnxy:  as above in ovalogtrn()

# value:

#    matrix of the betahat vectors, one per column, in the order of
#    combin(); also the proportions of each class, as attr()

avalogtrn <- function(m,trnxy) 
{
   p <- ncol(trnxy) 
   n <- nrow(trnxy)
   x <- trnxy[,1:(p-1)]
   if (hasFactors(x)) 
      stop('predictors must be numeric; convert to dummies')
   x <- as.matrix(trnxy[,1:(p-1)])
   y <- trnxy[,p]
   classcounts <- table(y)
   if (is.factor(y)) y <- as.numeric(y) - 1
   outmat <- NULL
   ijs <- combn(m,2) 
   doreg <- function(ij)  # does a regression for one pair of classes
   {
      i <- ij[1] - 1
      j <- ij[2] - 1
      tmp <- rep(-1,n)
      tmp[y == i] <- 1
      tmp[y == j] <- 0
      yij <- tmp[tmp != -1]
      xij <- x[tmp != -1,]
      coef(glm(yij ~ xij,family=binomial))
   }
   coefmat <- NULL
   for (k in 1:ncol(ijs)) {
      ij <- ijs[,k]
      coefmat <- cbind(coefmat,doreg(ij))
      if (any(is.na(coefmat))) 
         stop('glm() failed for ij = ',ij)
      colnames(coefmat)[k] <- paste(ij,collapse=',')
   }
   empirclassprobs <- classcounts/sum(classcounts)
   attr(coefmat,'empirclassprobs') <- empirclassprobs
   coefmat
}
################################################################## 
# avalogpred: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    m: as above
#    coefmat:  coefficient matrix, output from avalogtrn()
#    predx:  as above
#    trueclassprobs: as in ovalogtrn() above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

avalogpred <- function(m,coefmat,predx,trueclassprobs=NULL) {
   n <- nrow(predx)
   ypred <- vector(length = n)
   for (r in 1:n) {
      # predict the rth new observation
      xrow <- c(1,unlist(predx[r,]))
      # wins[i] tells how many times class i-1 has won
      wins <- rep(0,m)  
      ijs <- combn(m,2)  # as in avalogtrn()
      for (k in 1:ncol(ijs)) {
         # i,j play the role of Class 1,0
         i <- ijs[1,k]  # class i
         j <- ijs[2,k]  # class j
         bhat <- coefmat[,k]
         mhat <- logitftn(bhat %*% xrow)  # prob of Class i
         if (!is.null(trueclassprobs)) {
            empirclassprobs <- attr(coefmat,'empirclassprobs')
            if (m > 2) {
               normal2 <- function(probs) {
                  z <- probs[c(i,j)]; z/sum(z)
               }
               # need to normalize the m-class probs to 2-class
               ecp <- normal2(empirclassprobs)
               tcp <- normal2(trueclassprobs)
            }
            mhat <- classadjust(mhat,ecp[1],tcp[1])
         }
         if (i == 1) print(mhat)
         if (mhat >= 0.5) wins[i] <- wins[i] + 1 else wins[j] <- wins[j] + 1
      }
      ypred[r] <- which.max(wins) - 1
   }
   ypred
}

##################################################################
# avalogloom: LOOM predict Ys from Xs
##################################################################

# arguments: as with avalogtrn()

# value: LOOM-estimated probability of correct classification\

avalogloom <- function(m,trnxy) {
   n <- nrow(trnxy)
   p <- ncol(trnxy) 
   i <- 0
   correctprobs <- replicate(n,
      {
         i <- i + 1
         avout <- avalogtrn(m,trnxy[-i,])
         predy <- avalogpred(m,avout,trnxy[-i,-p])
         mean(predy == trnxy[-i,p])
      })
   mean(correctprobs)
}

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

#    y:  training set class data; either an R factor, or numeric
#        coded 0,1,...,m-1
#    x:  either numeric matrix of predictor/feature data, or  
#        output of preprocessx() applied to the training set
#        predictor data
#    m:  number of classes
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
#
#       k: number of nearest neighbors
#
#       empirclassprobs: proportions of cases in classes 0,1,...,m

knntrn <- function() stop('use ovaknntrn')

ovaknntrn <- function(y,x,m,k,xval=FALSE,trueclassprobs=NULL)
{
   if (m < 3) stop('m must be at least 3; use knnest() or knn() instead')  
   if (class(y) != 'factor') {
      uy <- unique(y)
      if (length(uy) != m || min(uy) != 0) 
         stop('y must either be a factor or have values 0,1,2,...,m-1')
      y <- as.factor(y)
   }
   if (class(x) == 'preknn') {
      xdata <- x
   } else {
      xdata <- preprocessx(x,k,xval=xval)
   }
   empirclassprobs <- table(y) / sum(table(y))
   # replace y with m dummies
   y <- factorToDummies(y,'y',FALSE)
   knnout <- knnest(y,xdata,k)
   xdata$regest <- knnout$regest
   xdata$k <- k
   xdata$empirclassprobs <- empirclassprobs
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
   trueclassprobs <- dts$trueclassprobs
   # could get k too, but not used; we simply take the 1-nearest, as the
   # prep data averaged k-nearest
   if (!is.matrix(predpts))
      stop('prediction points must be a matrix')
   x <- object$x
   # need to scale predpts with the same values that had been used in
   # the training set
   ctr <- object$scaling[,1]
   scl <- object$scaling[,2]
   predpts <- scale(predpts,center=ctr,scale=scl)
   tmp <- FNN::get.knnx(x,predpts,1)
   idx <- tmp$nn.index
   regest <- object$regest[idx,,drop=FALSE]
   if (!is.null(trueclassprobs)) {
      for (i in 1:ncol(regest)) {
         regest[,i] <- classadjust(regest[,i],
            empirclassprobs[i],trueclassprobs[i])
      }
   }
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
 
#    econdprobs: probs. for class 1, for various t, reported by the ML alg.
#    wrongprob1: proportion of class 1 in the training data
#    trueprob1: true proportion of class 1 
 
# value:
 
#     adjusted version of econdprobs

# why: say we wish to predict Y = 1,0 given X = t 
# P(Y=1 | X=t) = pf_1(t) / [pf_1(t) + (1-p)f_0(t)]
# where p = P(Y = 1); and f_i is the density of X within class i; so
# P(Y=1 | X=t) = 1 / [1 + {f_0(t)/f_(t)} (1-p)/p]
# and thus
# f_0(t)/f_1(t) = [1/P(Y=1 | X=t) - 1] p/(1-p)

# let q the actual proportion in class 1 (whether by sampling design or
# from a post-sampling artificial balancing of the classes); the ML
# algorith has, directly or indirectly, taken p to be q; so substitute
# and work back to the correct P(Y=1 | X=t)

classadjust <- function(econdprobs,wrongprob1,trueprob1) {
   wrongratio <- (1-wrongprob1) / wrongprob1
   fratios <- (1 / econdprobs - 1) * (1 / wrongratio)
   trueratio <- (1-trueprob1) / trueprob1
   1 / (1 + trueratio * fratios)
}

# plot estimated regression/probability function of a univariate, 
# binary y against each specified pair of predictors in x 

# for each point t, we ask whether est. P(Y = 1 | X = t) > P(Y = 1); if
# yes, plot '1', else '0'
 
# cexval is the value of cex in 'plot' 

# if user specifies 'pairs', the format is one pair per column in the
# provided matrix

# if band is non-NULL, only points within band, say 0.1, of est. P(Y =
# 1) are displayed, for a contour-like effect

pwplot <- function(y,x,k,pairs=combn(ncol(x),2),cexval=0.5,band=NULL) {
   p <- ncol(x)
   meanyval <- mean(y)
   ny <- length(y)
   for (m in 1:ncol(pairs)) {
      i <- pairs[1,m]
      j <- pairs[2,m]
      x2 <- x[,c(i,j)]
      xd <- preprocessx(x2,k)
      kout <- knnest(y,xd,k)
      regest <- kout$regest
      pred1 <- which(regest >= meanyval)
      if (!is.null(band))  {
         contourpts <- which(abs(regest - meanyval) < band)
         x2 <- x2[contourpts,]
      }
      xnames <- names(x2)
      plot(x2[pred1,1],x2[pred1,2],pch='1',cex=cexval,
         xlab=xnames[1],ylab=xnames[2])
      graphics::points(x2[-pred1,1],x2[-pred1,2],pch='0',cex=cexval)
      readline("next plot")
   }
}

#######################  mvrlm()  ################################

# uses multivariate (i.e. vector R) lm() for classification; faster than
# glm(), and may be useful as a rough tool if the goal is prediction, 
# esp. if have some quadratic terms

# arguments:

#    x: the usual matrix/df of predictor values
#    y: an R factor, vector or matrix/df; if vector, assumed to contain
#       class ID codes, and converted to a factor, which is then
#       converted to dummies; if matrix/df, assumed to already consist
#       of dummies
#    yname: name to be used as a base in dummies created from y

# value:

#    object of class 'mvrlm'
mvrlm <- function(x,y,yname=NULL) {
   if (!is.matrix(y) && !is.data.frame(y)) {
      if (is.vector(y)) y <- as.factor(y)
      if (is.null(yname)) stop('need non-null yname')
      ydumms <- factorToDummies(y,yname,FALSE)
   } else ydumms <- y 
   ydumms <- as.data.frame(ydumms)
   xy <- cbind(x,ydumms)
   xnames <- names(x)
   ynames <- names(ydumms)
   ynames <- paste0(ynames,collapse=',')
   cmd <- paste0('lmout <- lm(cbind(',ynames,') ~ .,data=xy)')
   eval(parse(text=cmd))
   class(lmout) <- c('mvrlm',class(lmout))
   lmout
}

# mvrlmObj is output of mvrlm(), newx is a data frame compatible with x
# in mvrlm()

predict.mvrlm <- function(mvrlmObj,newx) {
   class(mvrlmObj) <- class(mvrlmObj)[-1]
   preds <- predict(mvrlmObj,newx)
   tmp <- apply(preds,1,which.max)
   colnames(preds)[tmp]
}



