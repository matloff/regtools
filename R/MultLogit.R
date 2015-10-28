
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
   x <- trnxy[1:(p-1)]
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
# ovalogpred: predict sY from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from ovalogtrn()
#    predxy:  X,Y prediction set, Y in last column
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}

ovalogpred <- function(coefmat,predxy) {
   predx <- predxy[,-1]
   # get est reg ftn values for each row of predx and each col of
   # coefmat; vals for coefmat[,] in col i of tmp
   tmp <- cbind(1,predx) %*% coefmat
   tmp <- logit(tmp)
   predy <- apply(estlogodds,1,which.max)
}

