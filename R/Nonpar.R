
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
#
# value: cbind() of X values in xydata and estimated reg. ftn. at those
#        values, contains 'center' and 'scale' attributes if scalefirst
#        is non-NULL

knnest <- function(xydata,k,predpts=xydata[,-ncol(xydata),drop=FALSE],
   scalefirst=NULL,nearf=meany,outfile=NULL)
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
         x <- scale(x,center=sf[1],scale=sf[2])
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
   knnout
}

# prediction

# knnpred

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

# sgm <- 1
# a <- 5
# b <- 8
# ones <- matrix(rep(1,p),ncol=1)
# z <- matrix(nrow = n, ncol = p1)
# z[,1:p] <- runif(n*p,min=a,max=b)
# z[,p1] <- z[,1:p] %*% ones + sgm * runif(n,min = -0.5,max = 0.5)
# zout <- knnest(z,50,scalefirst='default')
# plot(z[,3],zout[,3]) # incorrect flattening at the 2 ends
# zout <- knnest(z,50,scalefirst='default',nearf=loclin)
# plot(z[,3],zout[,3]) # 

