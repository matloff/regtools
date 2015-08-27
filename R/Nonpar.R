
# use knnest() to estimate the regression function values; send output
# to knnpred() to predict

###########################  k-NN  ###############################

# k-NN estimation and prediction

# estimation:`

# arguments:
#
#   xydata: matrix or data frame of (X,Y) data, 
#           Y in last column
#   k:  number of nearest neighbors
#   scalefirst: call scale() on the data first; NULL means no scaling,
#               'default' means call scale() with defaults, and
#               a length-2 numeric vector means call scale() with these
#               center and scale values
#   nearf: function to apply to the nearest neighbors 
#          of a point
#   outfile: if non-NULL, save knnout, including attributes, to this file
#
# value: cbind() of X values in xydata and estimated reg. ftn. at those
#        values, contains 'center' and 'scale' attributes if scalefirst
#        is non-NULL

knnest <- function(xydata,k,scalefirst=NULL,nearf=meany,outfile=NULL)
{  require(FNN)
   ycol <- ncol(xydata)  # where is Y?
   # extract the X and Y data
   x <- xydata[,-ycol,drop = F]
   y <- xydata[,ycol]
   sf <- scalefirst
   if (!is.null(sf)) {
      if (is.character(sf)) {
         x <- scale(x) 
      } else
         x <- scale(x,center=sf[[1]],scale=sf[[2]])
      xydata <- cbind(x,y)
   }
   tmp <- get.knnx(data=x, query=x, k=k)
   idx <- tmp$nn.index
   # set idxrows[[i]] to row i of idx
   idxrows <- matrixtolist(1,idx)
   nearxy <- lapply(idxrows,function(idxrow) xydata[idxrow,])
   regest <- sapply(1:nrow(x),
      function(i) nearf(x[i,],nearxy[[i]]))
   knnout <- cbind(x,regest)
   attr(knnout,'center') <- attr(x,'scaled:center')
   attr(knnout,'scale') <- attr(x,'scaled:scale')
   if (!is.null(outfile)) save(knnout,file=outfile)
   knnout
}

# prediction:

# arguments:

# knnout:  output from knnest(); if string, a file name, else matrix
# predpts:  matrix/data frame of X values at which to predict Y

knnpred <- function(knnout,predpts) {
   if (is.character(knnout)) load(knnout)
   p1 <- ncol(knnout)
   p <- p1 - 1
   knnoutx <- knnout[,-p1]
   knnouty <- knnout[,p1]
   if (is.vector(predpts)) 
      predpts <- matrix(predpts,nrow=1)
   if (!is.null(attr(knnout,'center'))) {
      ctr <- attr(knnout,'center')
      scl <- attr(knnout,'scale')
      predpts <- scale(predpts,center=ctr,scale=scl)
   }
   tmp <- get.knnx(knnoutx,predpts,1)
   idx <- tmp$nn.index
   knnouty[idx]
}

# find mean of Y on the data z, Y in last column, and predict at xnew
meany <- function(predpt,nearxy) {
   ycol <- ncol(nearxy)
   mean(nearxy[,ycol])
}

# find variance of Y in the neighborhood of predpt
vary <- function(predpt,nearxy) {
   ycol <- ncol(nearxy)
   var(nearxy[,ycol])
}

# fit linear model to the data z, Y in last column, and predict at xnew
loclin <- function(predpt,nearxy) {
   ycol <- ncol(nearxy)
   bhat <- coef(lm(nearxy[,ycol] ~ nearxy[,-ycol]))
   c(1,predpt) %*% bhat
}

matrixtolist <- function (rc, m) 
{
    if (rc == 1) {
        Map(function(rownum) m[rownum, ], 1:nrow(m))
    }
    else Map(function(colnum) m[, colnum], 1:ncol(m))
}

