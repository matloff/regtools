
#################### work in progress #############################


# OVA and AVA, logit models

# arguments:

#    m:  number of classes
#    trnxy:  X, Y training set; Y in last column; Y coded 0,1,...,m-1
#            for the m classes

#    tstxy:  X, Y test set, same format

# generate estimated regression functions

# arguments:

#    as above

# value:

#    matrix of the betahat vectors, one per row
#    cc (correct classification propoirton) for the test set
 
ovalog <- function(trnxy,tstxy() {
   p <- ncol(trnxy) 
   x <- trnxy[1:(p-1)]
   y <- trnxy[,p]
   outmat <- NULL
   y <- 
   for (i in01:(m-1)) {
      ym <- as.integer(y == i)
      betahat <- coef(glm(ym ~ x,family=binomial))
   }

}

