
######################  k-NN  ###############################

# use knnest() to estimate the regression function values; send output
# to knnpred() to predict

# knnest(): 

# use kNN to estimate the regression function at each data point in the
# training set

# will refer to predictor and response variable data as X and Y;
# together they form the training set

# X must undergo preprocessing -- centering/scaling, and determination
# of nearest neighbors -- which is done by calling preprocessx() before
# calling knnest()

# arguments:
#
#   y: Y values in training set
#   xdata: X and associated neighbor indices; output of preprocessx()
#   k:  number of nearest neighbors
#   nearf: function to apply to the nearest neighbors 
#          of a point; default is mean(), as in standard kNN
#
# value: R list, consisting of xdata and the estimated 
#        reg. ftn. at those values

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
   xdata
}

# preprocessx():

# scale the X matrix, and form indices of neighbors

# arguments:

#    x: "X variables" matrix, cases in rows, predictors in columns; will
#       be scaled by this function and returned in scaled form
#    kmax: maximal number of nearest neighbors sought
#    xval: if TRUE, the neighbors of a point will not include 
#          the point itself

# value: R list; component 'x' is the result of scale(x); 'idxs' is a
#        matrix -- row i, column j shows the index of the jth-closest 
#        data point to data point i, j = 1,...,kmax; 'scaling' is a
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
   result
}

# knnpred():

# arguments:

#    xdata:  output from knnest()
#    predpts:  matrix/data frame of X values at which to predict Y

# value:

#    the predicted Y values for predpts

# note:  "1-nearest neighbor" is used here; for each row of predpts, the
# estimated regression function value for the closest point in the
# training data is used as our est. reg. ftn. value at tht predpts row

knnpred <- function(xdata,predpts) {
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

###########################  misc.  ###############################


# ucbdf <- tbltofakedf(UCBAdmissions)
# newucb <- matrix(nrow=nrow(ucbdf),ncol=ncol(ucbdf))
# for (i in 1:3) {
#    z <- ucbdf[,i] 
#    z <- as.numeric(as.factor(z))
#    newucb[,i] <- z
# }
# newucb[,3] <- newucb[,3] - 1
# ovout <- ovalogtrn(6,newucb)
# predy <- ovalogpred(ovout,newucb[,1:2])
# mean(predy == newucb[,3])
# avout <- avalogtrn(6,newucb)
# predy <- avalogpred(6,avout,newucb[,1:2])
# mean(predy == newucb[,3])

# forest <- read.csv("~/Research/Data/ForestTypes/training.csv")
# z <- forest[,1]
# z <- as.numeric(z)
# z <- z - 1
# forest[,1] <- z
# f1 <- cbind(forest[,-1],forest[,1]) 
# f2 <- f1[,-(1:20)]
# ovout <- ovalogtrn(4,f2)
# predy <- ovalogpred(ovout,f2[,-8])
# mean(predy == f2[,8])
# avout <- avalogtrn(4,f2)
# predy <- avalogpred(4,avout,f2[,-8])
# mean(predy == f2[,8])
# f3 <- f1[,c(1:9,28)]


# vert <- read.table('~/Research/Data/Vertebrae/vertebral_column_data/column_3C.dat',header=F)
# vert$V7 <- as.numeric(vert$V7) - 1
# ovout <- ovalogtrn(3,vert)
# predy <- ovalogpred(ovout,vert[,-7])
# mean(predy == vert$V7)
# trnidxs <- sample(1:310,155)
# predidxs <- setdiff(1:310,trnidxs)
# trnidxs <- sample(1:310,225)
# predidxs <- setdiff(1:310,trnidxs)
# ovout <- ovalogtrn(3,vert[trnidxs,])
# predy <- ovalogpred(ovout,vert[predidxs,1:6])
# mean(predy == vert[predidxs,7])
# avout <- avalogtrn(3,vert[trnidxs,])
# predy <- avalogpred(3,avout,vert[predidxs,1:6])
# mean(predy == vert[predidxs,7])
# vert <- read.table('~/Research/Data/Vertebrae/column_3C.dat',header=F)
# vert$V7 <- as.numeric(vert$V7) - 1
# xdata <- preprocessx(vert[,-7],25)
# zout <- ovaknntrn(vert$V7,xdata,3,25)
# predout <- ovaknnpred(zout,vert[,-7])
# mean(predout == vert[,7])


# trnidxs <- sample(1:4526,2263)
# predidxs <- setdiff(1:4526,trnidxs)
# ovout <- ovalogtrn(6,newucb[trnidxs,])
# predy <- ovalogpred(ovout,newucb[predidxs,1:2])
# mean(predy == newucb[predidxs,3])
# avout <- avalogtrn(6,newucb[trnidxs,])
# predy <- avalogpred(6,avout,newucb[predidxs,1:2])
# mean(predy == newucb[predidxs,3])



# glass <- read.csv('~/Research/Data/Glass/glass.data.txt',header=F)
# glass[,11] <- glass[,11] - 1


# trnidxs <- sample(1:4526,2263)
# predidxs <- setdiff(1:4526,trnidxs)
# ovout <- ovalogtrn(6,newucb[trnidxs,])
# predy <- ovalogpred(ovout,newucb[predidxs,1:2])
# mean(predy == newucb[predidxs,3])
# avout <- avalogtrn(6,newucb[trnidxs,])
# predy <- avalogpred(6,avout,newucb[predidxs,1:2])
# mean(predy == newucb[predidxs,3])

# yeast <- read.table('~/Research/Data/Yeast/yeast.data.txt',header=F)
# y1 <- yeast[,-1]  # delete name
# y1[,9] <- as.numeric(y1[,9]) - 1
# trnidxs <- sample(1:1484,742)
# predidxs <- setdiff(1:1484,trnidxs)
# ovout <- ovalogtrn(10,y1[trnidxs,])
# predy <- ovalogpred(ovout,y1[predidxs,1:8])
# mean(predy == y1[predidxs,9])
# avout <- avalogtrn(10,y1[trnidxs,])
# predy <- avalogpred(10,avout,y1[predidxs,1:8])
# mean(predy == y1[predidxs,9])

# pecs1$edulvl <- pecs1$ms + 2 * pecs1$phd
# xdata <- preprocessx(pecs1[train,c(1:3,6)],50)
# zout <- ovaknntrn(pecs1$edulvl[train],xdata,3,50)
# predout <- ovaknnpred(zout,pecs1[test,c(1:3,6)])
# always predicts 0!

