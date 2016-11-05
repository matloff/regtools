
######################  k-NN routines  #############################

# use knnest() to estimate the regression function values; send output
# to predict(), i.e. predict.knn() to predict; before calling knnest(),
# run preprocessx(), to determine nearest neighbors, thus allowing for
# trying several different values of k without recomputing nearest
# neighbors

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
#   y: Y values in training set, vector or matrix
#   xdata: X and associated neighbor indices; output of preprocessx()
#   k:  number of nearest neighbors
#   nearf: function to apply to the nearest neighbors 
#          of a point; default is mean(), as in standard kNN
#
# value: class of type 'knn', consisting of the components of the
#        the input xdata and a component regest, consisting of
#        the estimated reg. ftn. at the X values in xdata; regest

# NOTE: knnest() does NOT have an argument corresponding to xval in
# preprocessx(); if it is desired that xval = TRUE, the user must call
# preprocessx() with that value before calling knnest()

knnest <- function(y,xdata,k,nearf=meany)
{
   # take only the idxs for our value of k
   idxs <- xdata$idxs 
   if (ncol(idxs) < k) stop('k must be <= kmax')
   if (is.vector(y)) y <- as.matrix(y)
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

# scale the X matrix, and form indices of neighbors

# arguments:

#    x: "X variables" matrix, cases in rows, predictors in columns; will
#       be scaled by this function and returned in scaled form
#    kmax: maximal number of nearest neighbors sought
#    xval: cross-validation; if TRUE, the neighbors of a point 
#          will not include the point itself

# value: R list; component 'x' is the result of scale(x); 'idxs' is a
#        matrix -- row i, column j shows the index of the jth-closest 
#        data point to data point i, j = 1,...,kmax (actually j =
#        2,...,kmax if xval component is TRUE; 'scaling' is a
#        2-column matrix consisting of the attributes scaled:center and
#        scaled:scale from scale(x)

preprocessx <- function(x,kmax,xval=FALSE) {
   xval <- as.numeric(xval)
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
   result
}

######################  predict.knn()  ###############################

# do prediction on new data (or on the training set, if predpts is set
# that way); call via the predict() generic function

# arguments:

#    object:  output from knnest(), object of class 'knn'
#    predpts:  matrix/data frame of X values at which to predict Y

# value:

#    the predicted Y values for predpts

# note:  "1-nearest neighbor" is used here; for each row of predpts, the
# estimated regression function value for the closest point in the
# training data is used as our est. reg. ftn. value at that predpts row

predict.knn <- function(object,...) {
   predpts <- unlist(...)
   x <- object$x
   if (!is.matrix(predpts)) 
      stop('prediction points must be a matrix')
   # need to scale predpts with the same values that had been used in
   # the training set
   ctr <- object$scaling[,1]
   scl <- object$scaling[,2]
   predpts <- scale(predpts,center=ctr,scale=scl)
   tmp <- FNN::get.knnx(x,predpts,1)
   idx <- tmp$nn.index
   object$regest[idx,]
}

######################  kmin()  ###############################

# finds "best" value of k by cross-validation, over a set of
# evenly-spaced values of k; "best" means minimum cross-validated loss

# arguments:

#   y: Y values in the data set
#   xdata: result of calling preprocessx() on the X portion of the data;
#          xval=True is suggested for that call
#   lossftn(y,muhat): measure of loss if muhat used to predict y
#   nk: number of values to try for k, evenly spaced
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
      kpred <- predict(kout,x)
      mean(lossftn(y,kpred))
   }
   # evaluate at these values of k
   ks <- floor(kmax/nk) * (1:nk)
   merrs <- ks
   for (i in 1:nk) merrs[i] <- meanerr(ks[i])
   names(merrs) <- ks
   result <- list(meanerrs = merrs)
   result$kmin <- ks[which.min(merrs)]
   class(result) <- 'kmin'
   result
}

### ######################  plot.kmin()  ###############################
### 
### # x is output from kmin()
### 
### plot.kmin <- function(x,y,...) {
###    plot(names(x$meanerrs),x$meanerrs,
###       xlab='k',ylab='mean error',pch=20)
### }

######################  meany(), etc. ###############################

# these are the functions specifying the operation to be applied to the
# nearest-neighbor Y values; standard kNN uses the mean, implemented
# here as mean()

# find mean of Y on the data z, Y in last column, and predict at xnew
meany <- function(predpt,nearxy) {
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
   if (nycol > 1) stop('not capable of vector y yet')
   # predpt not used (but see loclin() below)
   ycol <- ncol(nearxy)
   var(nearxy[,ycol])
}

# fit linear model to the data z, Y in last column, and predict at xnew
loclin <- function(predpt,nearxy) {
   nycol <- ncol(nearxy) - length(predpt)
   if (nycol > 1) stop('not capable of vector y yet')
   ycol <- ncol(nearxy)
   bhat <- coef(lm(nearxy[,ycol] ~ nearxy[,-ycol]))
   c(1,predpt) %*% bhat
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
   abline(0,1)
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

nonparvarplot <- function(knnout) {
   nonparcondmean <- knnout$regest
   y <- knnout$y
   k <- knnout$k
   tmp <- knnest(y,knnout,k,nearf=vary)
   plot(knnout$regest,tmp$regest,xlab='mean',ylab='var')
   abline(0,1)
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

######################  knnstep() ###############################

# stepwise variable selection, for classification problems; add/delete
# predictor based on change in overall prediction error

######### experimental ###########

# parget.knnx():

# wrapper for use of 'parallel' package with get.knnx() of FNN package

# arguments are same is for get.knnx(), except that 'algorithm' is set
# and except for cls, a 'parallel' cluster; 'query' is distributed to
# chunks at the cluster nodes, and 'data' is copied to all cluster
# nodes; gen.knnx() is called at each node, then the results are
# combined

# value is the nn.index component of the list returned by get.knnx()

