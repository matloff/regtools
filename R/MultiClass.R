
# One-vs.-All (OVA) and All-vs.All (AVA), parametric models

##################################################################
# ovalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    m:  number of classes
#    trnxy:  X, Y training set; Y in last column; Y coded 0,1,...,m-1
#            for the m classes
#    predx:  X values from which to predict Y values
#    truepriors:  true class probabilities, typically from external source

# arguments:

#    m:  as above
#    trnxy:  as above
#    truepriors:  as above

# value:

#    matrix of the betahat vectors, one per column

ovalogtrn <- function(m,trnxy,truepriors=NULL) {
   p <- ncol(trnxy) 
   x <- as.matrix(trnxy[,1:(p-1)])
   y <- trnxy[,p]
   outmat <- NULL
   for (i in 0:(m-1)) {
      ym <- as.integer(y == i)
      betahat <- coef(glm(ym ~ x,family=binomial))
      outmat <- cbind(outmat,betahat)
   }
   if (!is.null(truepriors)) {
      tmp <- table(y)
      wrongpriors <- tmp / sum(tmp)
      outmat[1,] <- outmat[1,] 
         - log((1-truepriors)/truepriors) + log((1-wrongpriors)/wrongpriors)
   }
   colnames(outmat) <- as.character(0:(m-1))
   outmat
}

##################################################################
# ovalogpred: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from ovalogtrn()
#    predx:  as above
#    probs:  in addition to predicted classes, return the condtional
#            class probabilities as an attribute, 'probs'
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

ovalogpred <- function(coefmat,predx,probs=FALSE) {
   # get est reg ftn values for each row of predx and each col of
   # coefmat; vals from coefmat[,] in tmp[,i]
   #
   # let m = number of classes, np = number of obs to be predicted
   tmp <- as.matrix(cbind(1,predx)) %*% coefmat  # np x m 
   tmp <- logit(tmp)
   preds <- apply(tmp,1,which.max) - 1
   if (probs) {
      sumtmp <- apply(tmp,1,sum)  # length np
      normalized <- diag(1/sumtmp) %*% tmp
      attr(preds,'probs') <- normalized
   }
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
#    combin()

avalogtrn <- function(m,trnxy) {
   p <- ncol(trnxy) 
   n <- nrow(trnxy)
   x <- as.matrix(trnxy[,1:(p-1)])
   y <- trnxy[,p]
   outmat <- NULL
   ijs <- combn(m,2) 
   doreg <- function(ij) {
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
      colnames(coefmat)[k] <- paste(ij,collapse=',')
   }
   coefmat
}
################################################################## # avalogpred: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    m: as above
#    coefmat:  coefficient matrix, output from avalogtrn()
#    predx:  as above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

avalogpred <- function(m,coefmat,predx) {
   ijs <- combn(m,2)  # as in avalogtrn()
   n <- nrow(predx)
   ypred <- vector(length = n)
   for (r in 1:n) {
      # predict the rth new observation
      xrow <- c(1,unlist(predx[r,]))
      # wins[i] tells how many times class i-1 has won
      wins <- rep(0,m)  
      for (k in 1:ncol(ijs)) {
         i <- ijs[1,k]  # class i-1
         j <- ijs[2,k]  # class j-1
         bhat <- coefmat[,k]
         mhat <- logit(bhat %*% xrow)
         if (mhat >= 0.5) wins[i] <- wins[i] + 1 else
         wins[j] <- wins[j] + 1
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

logit <- function(t) 1 / (1+exp(-t))

matrixtolist <- function (rc,m) 
{
   if (rc == 1) {
      Map(function(rownum) m[rownum, ], 1:nrow(m))
   }
   else Map(function(colnum) m[, colnum], 1:ncol(m))
}

# kNN for classification, more than 2 classes

# uses One-vs.-All approach

# knntrn: generate estimated regression function values

# arguments

#    y:  training set class data; either an R factor, or numeric
#        coded 0,1,...,m-1
#    xdata:  output of preprocessx() applied to the training set
#            predictor data
#    m:  number of classes
#    k:  number of nearest neighbors
#    truepriors:  true class probabilities, typically from external source

# value:

#    xdata from input, plus a new list componens regest: 
#
#       regest: the matrix of estimated regression function values; 
#               the element in row i, column j, is the probability 
#               that Y = j given that X = row i in the X data, 
#               estimated from the training set

knntrn <- function(y,xdata,m=length(levels(y)),k,truepriors=NULL)
{
   if (m < 3) stop('m must be at least 3; use knnest() instead')  
   if (class(y) == 'factor') {
      y <- as.numeric(y) - 1
   }
   x <- xdata$x
   # replace y with m-1 dummies
   ds <- dummies::dummy(y)[,-m]
   for (i in 1:(m-1)) colnames(ds)[i] <- sprintf('y%d',i-1)
   knnout <- knnest(ds,xdata,k)
   outmat <- knnout$regest
   outmat <- cbind(outmat,1-apply(outmat,1,sum))
   colnames(outmat)[m] <- sprintf('y%d',m-1)
   if (!is.null(truepriors)) {
      tmp <- table(y)
      if (length(tmp) != m) 
         stop('some classes missing in Y data')
      tmp <- tmp / sum(tmp)
      # tmp now contains the estimated unconditional 
      # class probabilities
      for (i in 0:(m-1)) {
         wrongp <- tmp[i]
         wrongratio <- (1-wrongp) / wrongp
         truep <- truepriors[i]
         trueratio <- (1-truep) / truep
         outmat[,i] <- classadjust(outmat[,i],wrongratio,trueratio)
      }
   }
   xdata$regest <- outmat
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
   predpts <- unlist(...)
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
   predy <- apply(regest,1,which.max) - 1
   list(regest=regest,predy=predy)
}

classadjust <- function(econdprobs,wrongratio,trueratio) {
   fratios <- (1 / econdprobs - 1) * (1 / wrongratio)
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


# parget.knnx():

# wrapper for use of 'parallel' package with get.knnx() of FNN package

# arguments are same is for get.knnx(), except that 'algorithm' is set
# and except for cls, a 'parallel' cluster; 'query' is distributed to
# chunks at the cluster nodes, and 'data' is copied to all cluster
# nodes; gen.knnx() is called at each node, then the results are
# combined; PARALLEL OP NOT NOW IMPLEMENTED

# value is the nn.index component of the list returned by get.knnx()

## parget.knnx <- function(data, query, k=10, 
##       algorithm="kd_tree",cls=NULL) {
##    if (is.null(cls))  {
##       tmp <- get.knnx(data,query,k,algorithm)
##       return(tmp$nn.index)
##    }
##    partools::setclsinfo(cls)
##    partools::clusterExport(cls,c('data','k','algorithm'),envir=environment())
##    partools::distribsplit(cls,'query')
##    partools::clusterEvalQ(cls,liibrary(FNN))
##    tmp <- partools::clusterEvalQ(cls,get.knnx(data,query,k,algorithm))
##    tmp <- lapply(tmp,function(tmpelt) tmpelt$nn.index)
##    Reduce(rbind,tmp)
## }



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


