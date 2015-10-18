
#################### work in progress #############################


# OVA and AVA, logit models

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
   y <- 
   for (i in 0:(m-1)) {
      ym <- as.integer(y == i)
      betahat <- coef(glm(ym ~ x,family=binomial))
      outmat <- rbind(outmat,betahat)
   }
   outmat
}

##################################################################
# ovalogpred: predict sY from new Xs
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
   tmp <- cbind(1,predx) %*% coefmat
   # since the logit function is monotone increeasing and we are just
   # taking argmax, we need not plug into logit
   predy <- apply(tmp,1,which.max)
}

