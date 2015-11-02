
#################### work in progress #############################


# One-vs.-All (OVA) and All-vs.All (AVA), logit models

# arguments:

#    m:  number of classes
#    trnxy:  X, Y training set; Y in last column; Y coded 0,1,...,m-1
#            for the m classes
#    predx:  X values from which to predict Y values
#    tstxy:  X, Y test set, same format

##################################################################
# ovalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    m:  as above
#    trnxy:  as above

# value:

#    matrix of the betahat vectors, one per column

ovalogtrn <- function(m,trnxy) {
   p <- ncol(trnxy) 
   x <- as.matrix(trnxy[,1:(p-1)])
   y <- trnxy[,p]
   outmat <- NULL
   for (i in 0:(m-1)) {
      ym <- as.integer(y == i)
      betahat <- coef(glm(ym ~ x,family=binomial))
      outmat <- cbind(outmat,betahat)
   }
   outmat
}

##################################################################
# ovalogpred: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from ovalogtrn()
#    predx:  as above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

ovalogpred <- function(coefmat,predx) {
   # get est reg ftn values for each row of predx and each col of
   # coefmat; vals from coefmat[,] in tmp[,i]
   tmp <- as.matrix(cbind(1,predx)) %*% coefmat
   tmp <- logit(tmp)
   apply(tmp,1,which.max) - 1
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

#    m:  as above
#    trnxy:  as above

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
      coefmat <- cbind(coefmat,doreg(ijs[,k]))
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
f3 <- f1[,c(1:9,28)]


# vert <- read.table('~/Research/Data/Vertebrae/vertebral_column_data/column_3C.dat',header=F)
# vert$V7 <- as.numeric(vert$V7) - 1
# ovout <- ovalogtrn(3,vert)
# predy <- ovalogpred(ovout,vert[,-7])
# mean(predy == vert$V7)
trnidxs <- sample(1:310,155)
predidxs <- setdiff(1:310,trnidxs)
trnidxs <- sample(1:310,225)
predidxs <- setdiff(1:310,trnidxs)
ovout <- ovalogtrn(3,vert[trnidxs,])
predy <- ovalogpred(ovout,vert[predidxs,1:6])
mean(predy == vert[predidxs,7])
avout <- avalogtrn(3,vert[trnidxs,])
predy <- avalogpred(3,avout,vert[predidxs,1:6])
mean(predy == vert[predidxs,7])

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

yeast <- read.table('~/Research/Data/Yeast/yeast.data.txt',header=F)
y1 <- yeast[,-1]  # delete name
y1[,9] <- as.numeric(y1[,9]) - 1
trnidxs <- sample(1:1484,742)
predidxs <- setdiff(1:1484,trnidxs)
ovout <- ovalogtrn(10,y1[trnidxs,])
predy <- ovalogpred(ovout,y1[predidxs,1:8])
mean(predy == y1[predidxs,9])
avout <- avalogtrn(10,y1[trnidxs,])
predy <- avalogpred(10,avout,y1[predidxs,1:8])
mean(predy == y1[predidxs,9])


