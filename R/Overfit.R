
# routines to explore effect of deliberate overfitting beyond
# "interpolation," i.e. beyond "perfect fit"

###################  penroseLM()  #########################

# Penrose inverse version of lm(); a 1s col is added as in lm()

# arguments:

#    d:data frame; must be numeric
#    yName: name of "Y" column

# value:

#     object of class 'penroseLM',with beta-hat and colnames(x)

penroseLM <- function(d,yName) 
{
   require(MASS)
   ycol <- which(names(d) == yName)
   xnms <- colnames(x)
   x <- cbind(1,as.matrix(d[,-ycol]))
   y <- d[,ycol]
   res <- list(bh=ginv(x) %*% y, xnms=xnms)
   class(res) <- 'penroseLM'
   res
}

plm <- penroseLM

# arguments:

#    object: return value of penroseLM()
#    newx: data frame in the same format as x in penroseLM(); numeric

predict.penroseLM <- function(object,newx) 
{
   if(names(newx) != object$xnms) stop('name mismatch')
   newx <- cbind(1,as.matrix(newx))
   bh <- object$bh
   newx %*% bh
}

###################  penrosePoly()  #########################

penrosePoly <- function(d,yName,deg,maxInteractionDeg=deg) 
{
   require(polyreg)
   ycol <- which(names(d) == yName)
   x <- as.matrix(d[,-ycol])
   polyout <- getPoly(x,deg=deg,maxInteractionDeg=maxInteractionDeg)
   xPoly <- polyout$xdata
   y <- d[,ycol]
   xy <- cbind(xPoly,y)
   browser()
   penroseLM(xy,'y')

}
