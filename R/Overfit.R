
# routines to explore effect of deliberate overfitting beyond
# "interpolation," i.e. beyond "perfect fit"

###################  penroseLM()  #########################

# Penrose inverse version of lm(); a 1s col is added as in lm()

# arguments:

#    d:data frame; must be numeric
#    yName: name of "Y" column

# value:

#     object of class 'penroseLM',with beta-hat as 'bh' and colnames(x)

penroseLM <- function(d,yName) 
{
   require(MASS)
   ycol <- which(names(d) == yName)
   x <- cbind(1,as.matrix(d[,-ycol]))
   xnms <- colnames(x)
   y <- d[,ycol]
   # MASS::ginv() does Penrose inverse
   res <- list(bh=ginv(x) %*% y, xnms=xnms)
   class(res) <- 'penroseLM'
   res
}

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

# polynomial regression with Penrose inverse; uses polyreg

penrosePoly <- function(d,yName,deg,maxInteractionDeg=deg) 
{
   require(polyreg)
   ycol <- which(names(d) == yName)
   x <- as.matrix(d[,-ycol])
   polyout <- getPoly(x,deg=deg,maxInteractionDeg=maxInteractionDeg)
   xPoly <- polyout$xdata  # polynomial version of x
   y <- d[,ycol]
   xy <- cbind(xPoly,y)
   res <- list(bh=penroseLM(xy,'y')$bh,
      deg=deg,
      maxInteractionDeg=maxInteractionDeg,
      modelFormula=polyout$modelFormula,
      XtestFormula=polyout$XtestFormula,
      retainedNames=polyout$retainedNames,
      standardize=FALSE
      )
   class(res) <- 'penrosePoly'
   res
}

predict.penrosePoly <- function(object,newx) 
{
   polyout <- getPoly(newx,
      deg=object$deg,
      maxInteractDeg = object$maxInteractDeg,
      modelFormula = object$modelFormula,
      retainedNames = object$retainedNames)
   xPoly <- polyout$xdata  # polynomial version of newx
   xPoly <- as.matrix(xPoly)
   xPoly <- cbind(1,xPoly)
   bh <- object$bh
   xPoly %*% bh
}

predpnr <- predict.penrosePoly

