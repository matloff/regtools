
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
   ycol <- which(names(d) == yName)
   x <- cbind(1,as.matrix(d[,-ycol]))
   xnms <- colnames(x)
   y <- d[,ycol]
   # MASS::ginv() does Penrose inverse
   res <- list(bh=MASS::ginv(x) %*% y, xnms=xnms)
   class(res) <- 'penroseLM'
   res
}

# arguments:

#    object: return value of penroseLM()
#    newx: data frame in the same format as x in penroseLM(); numeric

predict.penroseLM <- function(object,...) 
{
   arglist <- list(...)
   newx <- arglist[[1]]

   if(names(newx) != object$xnms) stop('name mismatch')
   newx <- cbind(1,as.matrix(newx))
   bh <- object$bh
   newx %*% bh
}

###################  penrosePoly()  #########################

# polynomial regression with Penrose inverse; uses polyreg

penrosePoly <- function(d,yName,deg,maxInteractDeg=deg) 
{
   requireNamespace('polyreg')
   ycol <- which(names(d) == yName)
   x <- as.matrix(d[,-ycol,drop=FALSE])
   polyout <- polyreg::getPoly(x,deg=deg,maxInteractDeg=maxInteractDeg)
   xPoly <- polyout$xdata  # polynomial version of x
   y <- d[,ycol]
   xy <- cbind(xPoly,y)
   res <- list(bh=penroseLM(xy,'y')$bh,
      deg=deg,
      maxInteractDeg=maxInteractDeg,
      modelFormula=polyout$modelFormula,
      XtestFormula=polyout$XtestFormula,
      retainedNames=polyout$retainedNames,
      standardize=FALSE
      )
   class(res) <- 'penrosePoly'
   res
}

predict.penrosePoly <- function(object,...) 
{
   requireNamespace('polyreg')
   arglist <- list(...)
   newx <- arglist[[1]]

   if (nrow(newx) == 1) {
      # problem in getPoly() for case of a 1-row newx, reported to PM;
      # have this workaround for now
      oneRow <- TRUE
      newx <- rbind(newx,newx)
   } else oneRow <- FALSE
   polyout <- polyreg::getPoly(newx,
      deg=object$deg,
      maxInteractDeg = object$maxInteractDeg,
      modelFormula = object$modelFormula,
      retainedNames = object$retainedNames)
   xPoly <- polyout$xdata  # polynomial version of newx
   xPoly <- as.matrix(xPoly)
   xPoly <- cbind(1,xPoly)
   bh <- object$bh
   res <- xPoly %*% bh
   if (oneRow) res <- res[1]
   res
}

predpnr <- predict.penrosePoly

###################  ridgePoly()  #########################

# according to Hastie et al, "properly tuned" ridge regression beats
# mininum norm

# polynomial regression with Penrose inverse; uses polyreg

ridgePoly <- function(d,yName,deg,maxInteractDeg=deg) 
{
   requireNamespace('polyreg')
   if (!allNumeric(d)) stop('for now, X,Y must be numeric')
   ycol <- which(names(d) == yName)
   x <- as.matrix(d[,-ycol])
   polyout <- polyreg::getPoly(x,deg=deg,maxInteractDeg=maxInteractDeg)
   xPoly <- polyout$xdata  # polynomial version of x
   xPoly <- as.matrix(xPoly)
   y <- d[,ycol]
   cvgout <- glmnet::cv.glmnet(x=xPoly,y=y,alpha=0,family='gaussian')
   res <- list(cvgout=cvgout,
      deg=deg,
      maxInteractDeg=maxInteractDeg,
      modelFormula=polyout$modelFormula,
      XtestFormula=polyout$XtestFormula,
      retainedNames=polyout$retainedNames,
      standardize=FALSE
      )
   class(res) <- 'ridgePoly'
   res
}

predict.ridgePoly <- function(object,...) 
{
   requireNamespace('polyreg')
   arglist <- list(...)
   newx <- arglist[[1]]

   # newx must be a matrix, with the original column names
   if (nrow(newx) == 1) {
      # problem in getPoly() for case of a 1-row newx, reported to PM;
      # have this workaround for now
      oneRow <- TRUE
      newx <- rbind(newx,newx)
   } else oneRow <- FALSE
   polyout <- polyreg::getPoly(newx,
      deg=object$deg,
      maxInteractDeg = object$maxInteractDeg,
      modelFormula = object$modelFormula,
      retainedNames = object$retainedNames)
   xPoly <- polyout$xdata  # polynomial version of newx
   xPoly <- as.matrix(xPoly)
   glmObject <- object$cvgout
   res <- predict(glmObject,s=glmObject$lambda.min,newx=xPoly)
#    bh <- object$bh
#    res <- xPoly %*% bh
   if (oneRow) res <- res[1]
   res
}

