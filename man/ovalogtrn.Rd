\name{multiclass routines}
\alias{boundaryplot}
\alias{ovalogtrn}
\alias{ovaknntrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{predict.ovaknn}
\alias{classadjust}
\alias{confusion}
\alias{factorTo012ec}
\alias{classadjust}

\title{Classification with More Than 2 Classes}

\description{
Tools for multiclass classification, parametric and nonparametric.
}

\usage{
avalogtrn(trnxy,yname)
ovaknntrn(trnxy,yname,k,xval=FALSE)
avalogpred()
classadjust(econdprobs,wrongprob1,trueprob1) 
boundaryplot(y01,x,regests,pairs=combn(ncol(x),2),pchvals=2+y01,cex=0.5,band=0.10)
}

\arguments{
\item{pchvals}{Point size in base-R graphics.}
\item{trnxy}{Data matrix, Y last.}
\item{xval}{If TRUE, use leaving-one-out method.}
\item{y01}{Y vector (1s and 0s).}
\item{regests}{Estimated regression function values.}
\item{x}{X data frame or matrix.}
\item{pairs}{Two-row matrix, column i of which is a pair of predictor
   variables to graph.}
\item{cex}{Symbol size for plotting.}
\item{band}{If \code{band} is non-NULL, only points within \code{band}, 
   say 0.1, of est. P(Y = 1) are displayed, for a contour-like effect.}
\item{yname}{Name of the Y column.}
\item{k}{Number of nearest neighbors.} 
\item{econdprobs}{Estimated conditional class probabilities, given the
   predictors.}
\item{wrongprob1}{Incorrect, data-provenanced, unconditional P(Y = 1).}
\item{trueprob1}{Correct unconditional P(Y = 1).}
}

\details{

These functions aid classification in the multiclass setting.  

The function \code{boundaryplot} serves as a visualization technique,
for the two-class setting.  It draws the boundary between predicted Y =
1 and predicted Y = 0 data points in 2-dimensional feature space, as
determined by the argument \code{regests}.  Used to visually assess
goodness of fit, typically running this function twice, say one for
\code{glm} then for \code{kNN}.  If there is much discrepancy and the
analyst wishes to still use glm(), he/she may wish to add polynomial
terms.

The functions not listed above are largely deprecated, e.g. in favor of
\code{qeLogit} and the other \code{qe}-series functions.

}

\examples{

\dontrun{


data(oliveoils) 
oo <- oliveoils[,-1] 

# toy example
set.seed(9999)
x <- runif(25)
y <- sample(0:2,25,replace=TRUE)
xd <- preprocessx(x,2,xval=FALSE)
kout <- ovaknntrn(y,xd,m=3,k=2)
kout$regest  # row 2:  0.0,0.5,0.5
predict(kout,predpts=matrix(c(0.81,0.55,0.15),ncol=1))  # 0,2,0or2
yd <- factorToDummies(as.factor(y),'y',FALSE)
kNN(x,yd,c(0.81,0.55,0.15),2)  # predicts 0, 1or2, 2

data(peDumms)  # prog/engr data 
ped <- peDumms[,-33] 
ped <- as.matrix(ped)
x <- ped[,-(23:28)]
y <- ped[,23:28]
knnout <- kNN(x,y,x,25,leave1out=TRUE) 
truey <- apply(y,1,which.max) - 1
mean(knnout$ypreds == truey)  # about 0.37
xd <- preprocessx(x,25,xval=TRUE)
kout <- knnest(y,xd,25)
preds <- predict(kout,predpts=x)
hats <- apply(preds,1,which.max) - 1
mean(yhats == truey)  # about 0.37

data(peFactors)
# discard the lower educ-level cases, which are rare
edu <- peFactors$educ 
numedu <- as.numeric(edu) 
idxs <- numedu >= 12 
pef <- peFactors[idxs,]
numedu <- numedu[idxs]
pef$educ <- as.factor(numedu)
pef1 <- pef[,c(1,3,5,7:9)]

# ovalog
ovaout <- ovalogtrn(pef1,"occ")
preds <- predict(ovaout,predpts=pef1[,-3])
mean(preds == factorTo012etc(pef1$occ))  # about 0.39

# avalog

avaout <- avalogtrn(pef1,"occ")  
preds <- predict(avaout,predpts=pef1[,-3]) 
mean(preds == factorTo012etc(pef1$occ))  # about 0.39 

# knn

knnout <- ovalogtrn(pef1,"occ",25)
preds <- predict(knnout,predpts=pef1[,-3])
mean(preds == factorTo012etc(pef1$occ))  # about 0.43

data(oliveoils)
oo <- oliveoils
oo <- oo[,-1]
knnout <- ovaknntrn(oo,'Region',10)
# predict a new case that is like oo1[1,] but with palmitic = 950
newx <- oo[1,2:9,drop=FALSE]
newx[,1] <- 950
predict(knnout,predpts=newx)  # predicts class 2, South

}

}

\author{
Norm Matloff
}

