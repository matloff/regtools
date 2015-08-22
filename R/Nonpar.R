
###########################  k-NN  ###############################

# arguments:
#
#  xydata: matrix or data frame of (X,Y) data, 
#          Y in last column
#   k:  number of nearest neighbors
#  predpts:  matrix or data frame of X values at
#            which to estimate the regression ftn
#   scalefirst: call scale() on the data first
#   nearf: function to apply to the nearest neighbors 
#          of a point
#
# value: estimated reg. ftn. at the given X values predpts

knnest <- function(xydata,k,predpts=xydata[,-ncol(xydata)],
   scalefirst=FALSE,nearf=meany)
{  require(FNN)
   ycol <- ncol(xydata)  # where is Y?
   if (is.null(predpts))
      predpts <- xydata[,-ycol,drop=FALSE]
   if (is.vector(predpts))
      predpts <- matrix(predpts,ncol=1)
   # extract the X and Y data
   x <- xydata[,-ycol,drop = F]
   y <- xydata[,ycol]
   colnames(predpts) <- colnames(x)
   if (scalefirst) {
      tmp <- rbind(x,predpts)
      tmp <- scale(tmp)
      x <- tmp[1:nrow(x),,drop=FALSE]
      predpts <- tmp[(nrow(x)+1):nrow(tmp),,drop=FALSE]
   }
   tmp <- get.knnx(data=x, query=predpts, k=k)
   idx <- tmp$nn.index
   # set idxrows[[i]] to row i of idx
   idxrows <- matrixtolist(1,idx)
   nearxy <- lapply(idxrows,function(idxrow) xydata[idxrow,])
   sapply(1:nrow(predpts),
      function(i) nearf(predpts[i,],nearxy[[i]]))
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

