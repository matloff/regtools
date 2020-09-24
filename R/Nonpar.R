
######################  k-NN routines  #############################

# kNN() is now the main k-NN routine in the package; knnest() is
# deprecated; see also knnClass()

# in its basic form, kNN() does both fitting and predicting; if the
# latter will be done repeatedly over time, call kNN() with newx = NULL,
# and use predict.kNN() for prediction

######################  kNN()  ###############################

# arguments:

#   x: matrix (df or vec will be converted), "X" of training set
#   y: vector or matrix, "Y" of training set; in multiclass (> 2) case,
#      y can be specified either as a matrix of dummies or a vector of
#      numeric class labels; in the latter case, if classif is TRUE,
#      dummies will automatically be generated
#   newx: vector or matrix; "X" values to be predicted, if any; if NULL,
#      compute regests values at each "X", saving for later
#      prediction using predict.kNN()
#   kmax: maximum value of k requested; see 'allK' below
#   scaleX: x and newx will be scaled
#   PCAcomps: apply PCA (after scaling, if any) to x, newx, using this
#      many components; 0 means no PCA
#   smoothingFtn: op applied to the "Y"s of nearest neighbors; could be,
#      say, median instead of mean, even variance
#   allK: report kNN estimates for all k = 1,...,kmax; otherwise just k = kmax
#   leave1out: delete the 1-nearest neighbor (n-fold cross-validation)
#   classif: if TRUE, consider this a classification problem. 
#      Then 'ypreds' will be included in the return value.  See also the
#      entry for 'y' above.
#   startAt1: if classification case, labels 1,2,...; else 0,1,2,...

# value:

#    R object of class 'kNN', containing vector of nearest-neighbor
#    indices and a vector/matrix of estimated regression function values
#    at the rows of newx; these are conditional means, used directly as
#    predicted Y values in the continuous "Y" case; see also comments
#    about 'y' and 'ypreds' above

#    if newx is NULL, then return closest "X" to each "X" (accounting
#    for leave1out), regests, scaleX, x, leave1out

kNN <- function(x,y,newx=x,kmax,scaleX=TRUE,PCAcomps=0,
          expandVars=NULL,expandVals=NULL,
          smoothingFtn=mean,allK=FALSE,leave1out=FALSE,
          classif=FALSE,startAt1=TRUE)
{  
   noPreds <- is.null(newx)  # don't predict, just save for future predict
   startA1adjust <- if (startAt1) 0 else 1
   # general checks 
   if (identical(smoothingFtn,loclin)) {
      if (allK) stop('cannot use loclin() yet with allK = TRUE')
   }
   # checks on x
   if (is.vector(x)) x <- matrix(x,ncol=1)
   if (hasFactors(x)) stop('use factorsToDummies() to create dummies')
   if (is.data.frame(x)) 
      x <- as.matrix(x)
   ccout <- constCols(x) 
   if (length(ccout) > 0) {
      warning('X data has constant columns:')
      print(ccout)
      if (scaleX) stop('constant columns cannot work with scaling')
   }
   # checks on y
   nYvals <- length(unique(y))
   if (is.vector(y)) {
      if (classif && nYvals > 2) 
         y <- factorsToDummies(as.factor(y),omitLast=FALSE)
      else y <- matrix(y,ncol=1)
   }
   if (!is.vector(y) && !is.matrix(y)) stop('y must be vector or matrix')
   if (is.matrix(y) && identical(smoothingFtn,mean)) 
      smoothingFtn <- colMeans
   if (is.matrix(y) && allK)  
      stop('for now, in multiclass case, allK must be FALSE')
   # if (classif && allK) print('stub')
   #    stop('classif=TRUE can be set only if allK is FALSE')
   if (ncol(y) > 1 && !allK) classif <- TRUE
   # checks on newx
   if (is.factor(newx) || is.data.frame(newx) && hasFactors(newx))
      stop('change to dummies, factorsToDummies()')
   if (is.vector(newx)) {
      nms <- names(newx)
      newx <- matrix(newx,nrow=1)
      colnames(newx) <- nms
   }
   
   if (is.data.frame(newx)) {
      newx <- as.matrix(newx)
   }
   # at this point, x, y and newx will all be matrices

   if (nrow(y) != nrow(x)) 
      stop('number of X data points not equal to that of Y')

   if (noPreds) newx <- x

   kmax1 <- kmax + leave1out

   if (scaleX) {
      x <- scale(x)
      xcntr <- attr(x,'scaled:center')
      xscl <- attr(x,'scaled:scale')
      newx <- scale(newx,center=xcntr,scale=xscl)
   }

  # expand any specified variables
   eVars <- !is.null(expandVars)
   eVals <- !is.null(expandVals)
   if (eVars || eVals) {
      if(xor(eVars,eVals)) {
        stop('expandVars and expandVals must be used together')
      }
      if (length(expandVars) != length(expandVals)) {
          stop('expandVars and expandVals should have the same length')
      }
      x <- multCols(x,expandVars,expandVals)
      newx <- multCols(newx,expandVars,expandVals)  
   }

   if (PCAcomps > 0) 
      stop('PCA now must be done separately')

   # find NNs
   tmp <- FNN::get.knnx(data=x, query=newx, k=kmax1)
   closestIdxs <- tmp$nn.index
   if (leave1out) closestIdxs <- closestIdxs[,-1,drop=FALSE]

   # closestIdxs is a matrix; row i gives the indices of the kmax 
   # closest rows in x to newx[i,]

   # we might want to try various values of k (allK = T), up through
   # kmax; e.g.  for k = 2 would just use the first 2 columns

   # now, the predictions

   # treat kmax1 = 1 specially, as otherwise get 1x1 matrix issues
   if (kmax1 == 1) {
      regests <- y[closestIdxs,]
   } else {
      # in fyh(), closestIdxs is a row in closestIdxs, with the first k columns
      fyh <- function(closestIdxsRow) smoothingFtn(y[closestIdxsRow,,drop=FALSE])
      if (!allK) {
         if (identical(smoothingFtn,loclin)) {
            regests <- loclin(newx,cbind(x,y)[closestIdxs,])
         } else {
            regests <- apply(closestIdxs,1,fyh)
            if (ncol(y) > 1) regests <- t(regests)
         }
      } else {
         regests <- NULL
         for (k in 1:kmax) 
            regests <- 
               if (ncol(y) == 1)
                 rbind(regests,apply(closestIdxs[,1:k,drop=FALSE],1,fyh))
               else 
                 rbind(regests,t(apply(closestIdxs[,1:k,drop=FALSE],1,fyh)))
      }
   }

   # start building return value

   tmplist <- list(whichClosest=closestIdxs,regests=regests)

   # MH dists for possible re-run using loclin()
   if (length(ccout) == 0) {
      meanx <- rep(0,ncol(x))
      covx <- cov(x)
      tried <- try(
         tmplist$mhdists <- mahalanobis(newx,meanx,covx)
      )
      if (is.null(tried) || inherits(tried,'try-error')) {
         warning('Mahalanobis distances not calculated')
         tmplist$mhdists <- NULL
      } 
   }

   if (classif && !noPreds) {
      if (ncol(y) > 1) {  # multiclass (> 2) case
         yp <- apply(regests,1,which.max) - startA1adjust
         if (!allK) {
           ypreds <- yp
         } else ypreds <- matrix(yp,nrow=kmax,byrow=TRUE)
      } else ypreds <- round(regests)  # 2-class case
      tmplist$ypreds <- ypreds
   }

   tmplist$scaleX <- scaleX

   if (scaleX) {
      tmplist$xcntr <- xcntr
      tmplist$xscl <- xscl
   }
   if (noPreds) {
      tmplist$x <- x
   } else {
      tmplist$x <- NULL
   }
   tmplist$leave1out <- leave1out
   tmplist$startAt1adjust <- startA1adjust
   tmplist$expandVars <- expandVars
   class(tmplist) <- 'kNN'
   tmplist
}

# actual call is predict(kNNoutput,newx,add1); for each row in newx, the
# 1-nearest row in kNNoutput$x is found, and the corresponding
# kNNoutput$regests value returned; should change this to k >= 1

predict.kNN <- function(object,...)
{
   x <- object$x
   regests <- object$regests
   arglist <- list(...)
   newx <- arglist[[1]]
   expandVars <- object$expandVars
   if (!is.null(expandVars)) 
      stop('separate prediction with expandVars is not yet implemented')
   if (is.vector(newx)) newx <- matrix(newx,nrow=1)
   if (is.data.frame(newx)) {
      newx <- as.matrix(newx)
   }
   if (object$scaleX)  newx <- scale(newx,center=object$xcntr,scale=object$xscl)
   # k <- 1 + object$leave1out
   k <- 1
   tmp <- FNN::get.knnx(data=x, query=newx, k=k)
   if (k == 1) {
      # note: if k = 1, closestIdxs will be a 1-column matrix
      closestIdxs <- tmp$nn.index
   } else closestIdxs <- tmp$nn.index[,-1]
   if (is.vector(regests)) return(regests[closestIdxs])
   return(regests[closestIdxs,])
}

# n-fold cross validation for kNN(); instead of applying "leave 1 out"
# to all possible singletons, we do so for a random nSubSam of them;
# return matrix of estimated regression ftn values, one row for each
# leave-1-out op; number of columns will be 1 in the regression case,
# and number of classes in the classification case; other than nSubSam,
# args are as in kNN()
kNNxv <- function(x,y,k,scaleX=TRUE,PCAcomps=0,
          smoothingFtn=mean,nSubSam=500)
{
   if (!is.matrix(x) && !is.vector(x)) stop('x must be a matrix or vector')
   if (is.vector(x)) x <- matrix(x,ncol=1)
   if (is.factor(y)) stop('y must not be a factor')
   if (is.vector(y)) y <- matrix(y,ncol=1)
   n <- nrow(x)
   regests <- matrix(nrow=nSubSam,ncol=ncol(y))
   for (i in 1:nSubSam) {
      leftOutIdx <- sample(1:n,1)
      tmp <- kNN(x[-leftOutIdx,],y[-leftOutIdx,],x[leftOutIdx,],k,
         scaleX,PCAcomps,smoothingFtn)
      regests[i,] <- tmp$regests
   }
   regests
}
######################  expandVars  ###############################

# exploration of the expandVars and expandVals arguments in kNN()

# arguments:
 
#    xtrn: vector or matrix for "X" portion of training data
#    ytrn: vector or matrix for "Y" portion of training data; matrix
#       case is for vector "Y", i.e. multiclass 
#    xtst,ytst: test data analogs of xtrn, ytrn
#    k: number of nearest neighbors
#    eVar: column number of the predictor to be expanded
#    maxEVal: maximum expansion 
#    lossFtn: loss function; internal offerings are 'MAPE' and 'propMisclass'
#    eValIncr: expansion value increment 

# value:

#    mean loss, evaluated from 0 to maxEVal, increments of eValIncr

exploreExpVars <- 
   function(xtrn,ytrn,xtst,ytst,k,eVar,maxEVal,lossFtn,eValIncr=0.05,
      classif=FALSE,leave1out=FALSE) 
{
   lossFunction <- get(lossFtn)
   dfr <- data.frame(NULL,NULL)
   if (classif) ytst <- apply(ytst,1,which.max)
   for (w in seq(0.05,1.5,eValIncr)) {
      preds <- kNN(xtrn,ytrn,xtst,k,expandVars=eVar,expandVals=w,
         classif=classif,leave1out)
      if (!classif) {
         prds <- preds$regests
      } else {
         prds <- preds$ypreds
      }
      dfr <- rbind(dfr,c(w,mean(lossFunction(prds,ytst))))
   } 
   names(dfr) <- c('w',lossFtn)
   frmla <- as.formula(paste0(lossFtn, ' ~ w'))
   lwout <- loess(frmla,data=dfr) 
   lwout
}

######################  plotExpVars()  ###############################

# plot output of exploreExpVars(), one or more runs on the same plot

# arguments:

#    as for exploreExpVars() above, but with

#    eVars: indices of the predictors to be expanded (1 at a time)
#    ylim: as in R plot(), lower and upper limits for Y axis

plotExpVars <- function(xtrn,ytrn,xtst,ytst,k,eVars,maxEVal,lossFtn,
   ylim,eValIncr=0.05,classif=FALSE,leave1out=FALSE) 
{
   for (i in 1:length(eVars)) {
      lwout <- exploreExpVars(xtrn,ytrn,xtst,ytst,k,eVars[i],maxEVal,lossFtn,
         eValIncr=eValIncr,classif=classif,leave1out=leave1out)
      if (i == 1) {
         plot(lwout$x,lwout$fitted,type='l',ylim=ylim,
            xlab='w',ylab='accuracy',lwd=2)
      } else {
         lines(lwout$x,lwout$fitted,col=i)
      }
   }
   varNames <- colnames(xtrn)[eVars]
   legend('topright',legend=varNames,col=1:length(eVars),lwd=1.5)

}


######################  knnest()  ###############################

# use kNN to estimate the regression function at each data point in the
# training set

# will refer here to predictor and response variable data as matrix X
# and vector/matrix Y (see below); together they form the training set

# to accommodate vector-valued Y, including multiclass classification
# problems, Y is allowed to be a matrix; it is a vector in the
# "ordinary" case

# X must undergo preprocessing -- centering/scaling, and determination
# of nearest neighbors -- which is done by calling preprocessx() before
# calling knnest() (even if some components of X are indicator
# variables)

# the Fast Nearest Neighbor (FNN) package is used

# arguments:
#
#   y: Y values in training set, vector or matrix (the latter case is
#      for multivariate Y, e.g. in a classification problem with more
#      than 2 classes)
#   xdata: X and associated neighbor indices; output of preprocessx()
#   k:  number of nearest neighbors
#   nearf: function to apply to the nearest neighbors 
#          of a point; default is mean(), as in standard kNN
#
# value: object of class 'knn':
#        x,scaling,idxs: from input xdata
#        regest: estimated reg. ftn. at the X values 
#        y: the Y values at those X values
#        nycol: dimensionality of Y, normally 1
#        k: input k value

# NOTE: knnest() does NOT have an argument corresponding to xval in
# preprocessx(); if it is desired that xval = TRUE, the user must call
# preprocessx() with that value before calling knnest()

knnest <- function(y,xdata,k,nearf=meany)
{
   if (class(xdata) != 'preknn' && class(xdata) != 'knn') 
      stop('must call preprocessx() first')
   # take only the idxs for our value of k
   idxs <- xdata$idxs 
   if (ncol(idxs) < k) stop('k must be <= kmax')
   if (is.vector(y)) y <- as.matrix(y)
   if (ncol(y) == 2) stop('for 2-class case, use Y = 0,1 vector')
   idx <- idxs [,1:k]
   # set idxrows[[i]] to row i of idx, the indices of
   # the neighbors of the i-th observation
   idxrows <- matrixtolist(1,idx)
   # now do the kNN smoothing
   # first, form the neighborhoods
   x <- xdata$x
   xy <- cbind(x,y)
   nycol <- ncol(y)  # how many cols in xy are y?
   # ftn to form one neighborhood (x and y vals)
   form1nbhd <-  function(idxrow) xy[idxrow,]
   # now form all the neighborhoods
   nearxy <- lapply(idxrows,function(idxrow) xy[idxrow,])
   # now nearxy[[i]] is the rows of x corresponding to 
   # neighbors of x[i,], together with the associated Y values

   # now find the estimated regression function values at each point in
   # the training set
   regest <- sapply(1:nrow(x),
      function(i) nearf(x[i,],nearxy[[i]]))
   regest <- if (nycol > 1) t(regest) else as.matrix(regest)
   xdata$regest <- regest
   xdata$nycol <- nycol
   xdata$y <- y
   xdata$k <- k
   class(xdata) <- 'knn'
   xdata
}

######################  preprocessx()  ###############################

# form indices of neighbors and scale the X matrix 

# arguments:

#    x: "X variables" matrix or data frame, cases in rows, predictors 
#        in columns
#    kmax: maximal number of nearest neighbors sought
#    xval: cross-validation; if TRUE, the neighbors of a point 
#          will not include the point itself

# value: object of class 'preknn', with components: 

#        x: result of scale(x); 
#        scaling: 2-column matrix consisting of the attributes 
#                 scaled:center and scaled:scale from scale(x)
#        idxs: matrix; row i, column j shows the index of jth-closest 
#              data point to data point i, j = 1+xval,...,kmax 

preprocessx <- function(x,kmax,xval=FALSE) {
   xval <- as.numeric(xval)
   if (is.data.frame(x)) {
      if (hasFactors(x)) stop('features must be numeric')
      if (ncol(x) == 1) x <- matrix(x,ncol=1)
   } 
   if (is.vector(x)) x <- matrix(x,ncol=1)
   x <- scale(x)
   tmp <- cbind(attr(x,'scaled:center'),attr(x,'scaled:scale'))
   result <- list(scaling = tmp)
   attr(x,'scaled:center') <- NULL
   attr(x,'scaled:scale') <- NULL
   result$x <- x
   tmp <- FNN::get.knnx(data=x, query=x, k=kmax+xval)
   nni <- tmp$nn.index
   result$idxs <- nni[,(1+xval):ncol(nni)]
   result$xval <- xval
   result$kmax <- kmax
   class(result) <- 'preknn'
   result
}

######################  predict.knn()  ###############################

# do prediction on new data (or on the training set, if predpts is set
# that way); call via the predict() generic function

# arguments:

#    object:  output from knnest(), object of class 'knn'
#    predpts:  matrix/data frame of X values at which to predict Y
#    needtoscale:  if TRUE, scale predpts according to xdata

# value:

#    the predicted Y values for predpts

# note:  "1-nearest neighbor" is used here; for each row of predpts, the
# estimated regression function value for the closest point in the
# training data is used as our est. reg. ftn. value at that predpts row

predict.knn <- function(object,...) 
{
   if (class(object) != 'knn')
      stop('must be called on object of class "knn"')
   x <- object$x
   ### predpts <- unlist(...)
   arglist <- list(...)
   predpts <- arglist[[1]]
   if (is.vector(predpts)) {
      if (ncol(x) == 1) {
          predpts <- matrix(predpts,ncol=1)
      } else
          predpts <- matrix(predpts,nrow=1)
   }
   if (!is.matrix(predpts)) 
      stop('prediction points must be in a matrix')
   # needtoscale <- arglist[[2]]
   if (!is.null(object$scaling)) {
      # scale predpts with the same values that had been used in
      # the training set
      ctr <- object$scaling[,1]
      scl <- object$scaling[,2]
      predpts <- scale(predpts,center=ctr,scale=scl)
   }
   k <- 1
   # if (length(arglist) > 1) k <- arglist[[2]]
   if (k == 1) {
      tmp <- FNN::get.knnx(x,predpts,1)
      idxs <- tmp$nn.index
      # in regest[,], keep in mind that Y may be multivariate, 
      # thus regest's matrix form, rather than a vector
      return(object$regest[idxs,])
   }
   # start loc linear regression code
   if (object$nycol > 1)
      stop('not capable of multiclass Y yet')
   if (k <= 1 + ncol(x))
      stop('need more neighbors than 1 + number of predictors')
   # for each predpt fit lin reg in neighborhood of that point, and use
   # it to predict Y for predpt
   tmp <- FNN::get.knnx(x,predpts,k)
   idxs <- tmp$nn.index
   npred <- nrow(predpts)
   result <- vector(length = npred)
   for (i in 1:npred) {
      nbhdyvals <- object$y[idxs[i,],]
      nbhdxvals <- object$x[idxs[i,],]
      tmp <- lm(nbhdyvals ~ nbhdxvals)
      result[i] <- coef(tmp) %*% c(1,predpts[i,])
   }
   result
}

######################  kmin()  ###############################

# finds "best" value of k by cross-validation, over a set of
# evenly-spaced values of k; "best" means minimum cross-validated loss

# arguments:

#   y: Y values in the data set
#   xdata: result of calling preprocessx() on the X portion of the data;
#          xval=True is suggested for that call
#   lossftn(y,muhat): measure of loss if muhat used to predict y
#   nk: number of values to try for k, evenly spaced; or, if specified
#       as a vector, the actual values to try
#   nearf: see knnest()

#   value: the value of k found to be "best"

kmin <- function(y,xdata,lossftn=l2,nk=5,nearf=meany) {
   if (is.matrix(y) && ncol(y) > 1)
      stop('not capable of multiclass Y yet')
   n <- nrow(xdata$x)
   x <- xdata$x
   xval <- xdata$xval
   kmax <- xdata$kmax
   meanerr <- function(k) {
      kout <- knnest(y,xdata,k,nearf)
      kpred <- predict(kout,x,needtoscale=FALSE)
      mean(lossftn(y,kpred))
   }
   # evaluate at these values of k
   if (length(nk) == 1) {
      ks <- floor(kmax/nk) * (1:nk)
   } else ks <- nk
   if (min(ks) <= 1)
      stop('need k at least 2')
   merrs <- ks
   for (i in 1:length((ks))) merrs[i] <- meanerr(ks[i])
   names(merrs) <- ks
   result <- list(meanerrs = merrs)
   result$kmin <- ks[which.min(merrs)]
   class(result) <- 'kmin'
   result
}

#### removed, Aug. 8, 2019

## # incorrect prediction in 2-class classification problem
## predwrong <- function(muhat,y)
##    as.integer(y == round(muhat))

### ######################  plot.kmin()  ###############################

# x is output from kmin()

plot.kmin <- function(x,y,...) {
   plot(names(x$meanerrs),x$meanerrs,
      xlab='k',ylab='mean error',pch=20)
}

######################  meany(), etc. ###############################

# these are the functions specifying the operation to be applied to the
# nearest-neighbor Y values; standard kNN uses the mean, implemented
# here as mean()

# find mean of Y on the data z, Y in last column, and predict at xnew
meany <- function(predpt,nearxy) 
{
   # predpt not directly used (but see loclin() below)
   nxcol <- 
      if(is.vector(predpt)) length(predpt) else ncol(predpt)
   nxycol <- ncol(nearxy)
   ycols <- (nxcol+1):nxycol
   colMeans(nearxy[,ycols,drop=FALSE])
}

# find variance of Y in the neighborhood of predpt

vary <- function(predpt,nearxy) {
   nycol <- ncol(nearxy) - length(predpt)
   if (nycol > 1) stop('not capable of vector y with vary()')
   # predpt not used (but see loclin() below)
   ycol <- ncol(nearxy)
   var(nearxy[,ycol])
}

# fit linear model to the data z, Y in last column, and predict at xnew
loclin <- function(predpts,nearxy) {
   if (is.vector(predpts)) predpts <- matrix(predpts,nrow=1)
   # single- and multicolumn Y work on mostly the same code
   xy <- nearxy
   nycol <- ncol(nearxy) - ncol(predpts)
   ystartcol <- ncol(predpts) + 1
   colnames(xy) <- paste0('z',1:ncol(xy))
   yNames <- paste0('y',1:nycol)
   colnames(xy)[ystartcol:ncol(xy)] <- yNames
   xy <- as.data.frame(xy)
   colnames(predpts) <- paste0('z',1:ncol(predpts))
   predpts <- as.data.frame(predpts)
   if (nycol > 1) {
      cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .,data=xy)')
   } else {
      cmd <- paste0('lmout <- lm(',yNames[1],' ~ .,data=xy)')
   }
   eval(parse(text=cmd))
   predict(lmout,predpts)
}

######################  parvsnonparplot(), etc. ###############################

# plot fitted values of parameteric model vs. kNN fitted
#
# arguments:
#    lmout: object of class 'lm' or 'glm' 
#    knnout: knnest()
parvsnonparplot <- function(lmout,knnout,cex=1.0) {
   parfitted <- lmout$fitted.values
   nonparfitted <- knnout$regest
   plot(nonparfitted,parfitted,cex=cex)
   abline(0,1,col='red')
}

######################  nonparvsxplot(), etc. ###############################

# plot, against each predictor, either nonpar or par - nonpar
#
# arguments:
#    lmout: object of class 'lm' or 'glm' 
#    knnout: return value of knnest()

nonparvsxplot <- function(knnout,lmout=NULL) {
   nonparfitted <- knnout$regest
   havelmout <- !is.null(lmout) 
   if (havelmout) {
      parfitted <- lmout$fitted.values
      vertval <- parfitted - nonparfitted
   } else vertval <- nonparfitted
   x <- knnout$x
   for (i in 1:ncol(x)) {
      xlab <- colnames(x)[i]
      plot(x[,i],vertval,xlab=xlab,pch=20)
      if (havelmout) abline(0,0)
      readline('next plot')
   }
}

######################  nonparvarplot()  ###############################

# plots nonpar estimated conditional variance against nonpar estimated
# conditional mean 
#
# arguments:
#    knnout: return value of knnest()

nonparvarplot <- function(knnout,returnPts=FALSE) {
   nonparcondmean <- knnout$regest
   y <- knnout$y
   k <- knnout$k
   tmp <- knnest(y,knnout,k,nearf=vary)
   plot(knnout$regest,tmp$regest,xlab='mean',ylab='var')
   abline(0,1)
   if (returnPts) return(cbind(knnout$regest,tmp$regest))
}

######################  l2, etc.  ###############################

l2 <- function(y,muhat) (y - muhat)^2
l1 <- function(y,muhat) abs(y - muhat)

######################  matrixtolist()  ###############################

matrixtolist <- function (rc, m) 
{
    if (rc == 1) {
        Map(function(rownum) m[rownum, ], 1:nrow(m))
    }
    else Map(function(colnum) m[, colnum], 1:ncol(m))
}

######################  bestKperPoint()  ###############################

# for each point in the training set, find which k would have produced
# the best (MAPE) prediction

# a single set is used for training and test, but with leave1out TRUE

bestKperPoint <- function(x,y,maxK,lossFtn='MAPE',classif=FALSE) 
{

   if (lossFtn != 'MAPE' && lossFtn != 'propMisclass') 
      stop('only MAPE or propMisclass loss allowed')
   if (lossFtn == 'propMisclass') {
      if (!classif) stop('classif must be TRUE here')
      yvals <- unique(y)
      if (yvals != 0:1 && yvals != 1:0)
         stop('Y must be coded 0,1 for classif=TRUE')
   } else {
      if (classif) {
         stop('did you want propMisclass?')
      }
   }
   knnout <- kNN(x,y,x,maxK,leave1out=TRUE,classif=classif)
   whichClosest <- knnout$whichClosest
   whichClosest <- whichClosest[,-1]
   n <- nrow(whichClosest)
   nc <- ncol(whichClosest)
   bestK <- function(i) {
      nearYs <- y[whichClosest[i,]]
      nearYbars <- cumsum(nearYs) / 1:nc
      if (lossFtn == 'MAPE') {
         which.min(abs(nearYbars -y[i]))
      } else {
         which.min((round(nearYbars) == y[i]))
      }
   }
   sapply(1:n,bestK)
}

