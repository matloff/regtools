
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
   x <- as.matrix(trnxy[1:(p-1)])
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

ovalogpred <- function(coefmat,predxy) {
   # get est reg ftn values for each row of predx and each col of
   # coefmat; vals from coefmat[,] in tmp[,i]
   tmp <- cbind(1,predx) %*% coefmat
   tmp <- logit(tmp)
   apply(tmp,1,which.max) - 1
}

logit <- function(t) 1 / (1+exp(-t))


ucbdf <- tbltofakedf(UCBAdmissions)
z <- ucbdf[,1] 
ucbdf[,1] <- ifelse(z=="Admitted",1,0)
z <- ucbdf[,2] 
ucbdf[,2] <- ifelse(z=="Male",1,0)
z <- ucbdf[,3] 
f <- function(chr) charToRaw(chr)
ucbdf[,3] <- asc(z) - 65
ucbdf <- as.data.frame(ucbdf)
for (i in 1:3) ucbdf[,i] <- as.numeric(ucbdf[,i])
ovalogtrn(6,ucbdf)

forest <- read.csv("~/Research/Data/ForestTypes/training.csv")
z <- forest[,1]
z <- as.numeric(z)
z <- z - 1
forest[,1] <- z
f1 <- cbind(forest[,-1],forest[,1]) 
f2 <- f1[,-(1:20)]
ovalogtrn(4,f2)


