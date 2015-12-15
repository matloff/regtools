
######################  k-NN  ###############################

# use knnest() to estimate the regression function values; send output
# to predict(), i.e. predict.knn() to predict; before calling knnest(),
# run preprocessx(), to determine nearest neighbors, thus allowing for
# trying several different values of k without recomputing nearest
# neighbors

# knnest(): 

# use kNN to estimate the regression function at each data point in the
# training set

# will refer here to predictor and response variable data as X and Y;
# together they form the training set

# X must undergo preprocessing -- centering/scaling, and determination
# of nearest neighbors -- which is done by calling preprocessx() before
# calling knnest() (even if some components of X are indicator
# variables)

# the Fast Nearest Neighbor (FNN) package is used

# arguments:
#
#   y: Y values in training set
#   xdata: X and associated neighbor indices; output of preprocessx()
#   k:  number of nearest neighbors
#   nearf: function to apply to the nearest neighbors 
#          of a point; default is mean(), as in standard kNN
#
# value: class of type 'knn', consisting of input xdata and 
#        the estimated reg. ftn. at those values

knnest <- function(y,xdata,k,nearf=meany)
{  require(FNN)
   # take only the idxs for our value of k
   idxs <- xdata$idxs 
   if (ncol(idxs) < k) stop('k must be <= kmax')
   idx <- idxs [,1:k]
   # set idxrows[[i]] to row i of idx
   idxrows <- matrixtolist(1,idx)
   # now do the kNN smoothing
   x <- xdata$x
   xy <- cbind(x,y)
   nearxy <- lapply(idxrows,function(idxrow) xy[idxrow,])
   # now nearxy[[i]] is the portion of x corresponding to 
   # neighbors of x[i,], together with the associated Y values

   # now find the estimated regression function values at each point in
   # the training set
   regest <- sapply(1:nrow(x),
      function(i) nearf(x[i,],nearxy[[i]]))
   xdata$regest <- regest
   class(xdata) <- 'knn'
   xdata
}

# preprocessx():

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
   tmp <- get.knnx(data=x, query=x, k=kmax+xval)
   nni <- tmp$nn.index
   result$idxs <- nni[,(1+xval):ncol(nni)]
   result$xval <- xval
   result
}

# predict.knn():

# do prediction on new data (or on the training set, if predpts is set
# that way); call via the predict() generic function

# arguments:

#    xdata:  output from knnest(), object of class 'knn'
#    predpts:  matrix/data frame of X values at which to predict Y

# value:

#    the predicted Y values for predpts

# note:  "1-nearest neighbor" is used here; for each row of predpts, the
# estimated regression function value for the closest point in the
# training data is used as our est. reg. ftn. value at tht predpts row

# knnpred <- function(xdata,predpts) {
predict.knn <- function(xdata,predpts) {
   x <- xdata$x
   if (is.vector(predpts)) 
      predpts <- matrix(predpts,nrow=1)
   # need to scale predpts with the same values that had been used in
   # the training set
   ctr <- xdata$scaling[,1]
   scl <- xdata$scaling[,2]
   predpts <- scale(predpts,center=ctr,scale=scl)
   tmp <- get.knnx(x,predpts,1)
   idx <- tmp$nn.index
   xdata$regest[idx]
}

# goodk():

# finds a "good" value of k by cross-validation and bisection; not
# guaranteed to be the "best" k for this data, let alone prediction
# future data

# range of values considered for k are 1,2,...,xdata$kmax

# arguments:

#   y: Y values in the data set
#   xdata: result of calling preprocessx() on the X portion of the data;
#          best to set xval = TRUE in that call
#   lossftn(y,munat): loss function; for classification case, predwrong() 
#                     is suggested

#   value: the value of k found to be "good"

goodk <- function(y,xdat,lossftn=l2) {
   n <- nrow(xdata$x)
   x <- xval$x
   xval <- xdata$xval
   lok <- 1 + xval
   hik <- xdata$kmax
   errrate <- function(k) {
      kout <- knnest(y,xdata,k+xval)
      kpred <- predict(xdata,x)
      if (lossftn == predwrong) 
         kpred <- round(kpred)
      mean(lossftn(y,kout$regest))
   }
   minerr <- 1.09e+38
   repeat {
      mid <- round((lok+hik)/2)
      loerr <- errrate(lok)
      hierr <- errrate(hik)
      if (loerr >= minerr && hierr >= minerr) return(mid)
      if (loerr <- hierr) hik <- mid else
         lok <- mid
   }
}

# find mean of Y on the data z, Y in last column, and predict at xnew
meany <- function(predpt,nearxy) {
   # predpt not used (but see loclin() below)
   ycol <- ncol(nearxy)
   mean(nearxy[,ycol])
}

# find variance of Y in the neighborhood of predpt
vary <- function(predpt,nearxy) {
   # predpt not used (but see loclin() below)
   ycol <- ncol(nearxy)
   var(nearxy[,ycol])
}

# fit linear model to the data z, Y in last column, and predict at xnew
loclin <- function(predpt,nearxy) {
   ycol <- ncol(nearxy)
   bhat <- coef(lm(nearxy[,ycol] ~ nearxy[,-ycol]))
   c(1,predpt) %*% bhat
}

# plot fitted values of parameteric model vs. kNN fitted
#
# arguments:
#    lmout: object of class 'lm' or 'glm' 
#    knnout: knnest()
parvsnonparplot <- function(lmout,knnout) {
   parfitted <- lmout$fitted.values
   nonparfitted <- knnout$regest
   plot(nonparfitted,parfitted,pch=20)
   abline(0,1)
}

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

matrixtolist <- function (rc, m) 
{
    if (rc == 1) {
        Map(function(rownum) m[rownum, ], 1:nrow(m))
    }
    else Map(function(colnum) m[, colnum], 1:ncol(m))
}

# parget.knnx():

# wrapper for use of 'parallel' package with get.knnx() of FNN package

# arguments are same is for get.knnx(), except that 'algorithm' is set
# and except for cls, a 'parallel' cluster; 'query' is distributed to
# chunks at the cluster nodes, and 'data' is copied to all cluster
# nodes; gen.knnx() is called at each node, then the results are
# combined

# value is the nn.index component of the list returned by get.knnx()

parget.knnx <- function(data, query, k=10, 
      algorithm="kd_tree",cls=NULL) {
   if (is.null(cls))  {
      tmp <- get.knnx(data,query,k,algorithm)
      return(tmp$nn.index)
   }
   require(partools)
   setclsinfo(cls)
   clusterExport(cls,c('data','k','algorithm'),envir=environment())
   distribsplit(cls,'query')
   clusterEvalQ(cls,library(FNN))
   tmp <- clusterEvalQ(cls,get.knnx(data,query,k,algorithm))
   tmp <- lapply(tmp,function(tmpelt) tmpelt$nn.index)
   Reduce(rbind,tmp)
}

