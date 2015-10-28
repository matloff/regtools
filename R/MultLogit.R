
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
#    vector of predicted Y values, in {0,1,...,m-1}

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
   x <- as.matrix(trnxy[,1:(p-1)])
   y <- trnxy[,p]
   outmat <- NULL
   ijs <- combn(p-1,2)
   doreg <- function(ij) {
      i <- ij[1]
      ym <- as.integer(y == i)
      coef(glm(ym ~ x[,ij],family=binomial))
   }
   sapply(ijs,doreg)
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
#    vector of predicted Y values, in {0,1,...,m-1}

avalogpred <- function(coefmat,predx) {
   ijs <- combn(p-1,2)
   # get est reg ftn values for each row of predx and each col of
   # coefmat; vals from coefmat[,] in tmp[,i]


}

logit <- function(t) 1 / (1+exp(-t))

# ucbdf <- tbltofakedf(UCBAdmissions)
# newucb <- matrix(nrow=nrow(ucbdf),ncol=ncol(ucbdf))
# for (i in 1:3) {
#    z <- ucbdf[,i] 
#    z <- as.numeric(as.factor(z))
#    newucb[,i] <- z
# }
# newucb[,3] <- newucb[,3] - 1
# ovalogtrn(6,newucb)

# forest <- read.csv("~/Research/Data/ForestTypes/training.csv")
# z <- forest[,1]
# z <- as.numeric(z)
# z <- z - 1
# forest[,1] <- z
# f1 <- cbind(forest[,-1],forest[,1]) 
# f2 <- f1[,-(1:20)]
# ovalogtrn(4,f2)
ypred <- ovalogpred(ovout,newucb[,1:2])
sum(ypred == newucb[,3])

