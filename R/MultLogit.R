
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
   ijs <- combn(p-1,2)
   doreg <- function(ij) {
      i <- ij[1]
      j <- ij[2]
      tmp <- rep(-1,n)
      tmp[y == i] <- 1
      tmp[y == j] <- 0
      yij <- tmp[tmp != -1]
      xij <- x[tmp != -1,c(i,j)]
      coef(glm(yij ~ xij,family=binomial))
   }
   # sapply(matrixtolist(2,ijs),doreg)
   coefmat <- NULL
   for (k in 1:ncol(ijs)) 
      coefmat <- rbind(coefmat,doreg(ijs[,k]))
   coefmat
}

##################################################################
# avalogpred: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from avalogtrn()
#    predx:  as above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

avalogpred <- function(coefmat,predx) {
   ijs <- combn(p-1,2)
   for (r in 1:nrow(predx)) {
      # predict the rth new observation
      xrow <- c(1,predx[r,])
      # wins[i] tells how many times class i-1 has won
      wins <- rep(0,p-1)  
      for (k in 1:ncol(ijs)) {
         i <- ijs[1,k]
         j <- ijs[2,k]
         bhat <- coefmat[k,]
         mhat <- logit(bhat %*% xrow)
         if (mhat >= 0.5) wins[i] <- wins[i] + 1 else
         wins[j] <- wins[j] + 1
      }
      ypred <- which.max(wins) - 1
   }
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
# ypred <- ovalogpred(ovout,newucb[,1:2])
# sum(ypred == newucb[,3])

forest <- read.csv("~/Research/Data/ForestTypes/training.csv")
z <- forest[,1]
z <- as.numeric(z)
z <- z - 1
forest[,1] <- z
f1 <- cbind(forest[,-1],forest[,1]) 
f2 <- f1[,-(1:20)]
ovout <- ovalogtrn(4,f2)
ypred <- ovalogpred(ovout,f2[,-8])
sum(ypred == f2[,8])
avout <- avalogtrn(6,newucb)
