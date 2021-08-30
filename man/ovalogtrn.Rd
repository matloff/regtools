\name{multiclass routines}
\alias{boundaryplot}
\alias{knnCalib}
\alias{scoresToProbs}
\alias{getDValsE1071}
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
knnCalib(y,trnScores,newScores,k) 
scoresToProbs(y,trnScores,newScores,k) 
getDValsE1071(object,newx)
logitClass(data,yName)
\method{predict}{logitClass}(object,newx)
linClass(data,yName)
\method{predict}{linClass}(object,newx)
knnClass(data,yName,k)
\method{predict}{knnClass}(object,newx)
ovalogtrn(...)
\method{predict}{ovalog}(object,...) 
avalogtrn(m,trnxy)
ovaknntrn(trnxy,yname,k,xval=FALSE)
avalogpred(m,coefmat,predx,trueclassprobs=NULL)
classadjust(econdprobs,wrongprob1,trueprob1) 
boundaryplot(y,x,regests,pairs=combn(ncol(x),2),pchvals=2+y,cex=0.5,band=0.10)
}

\arguments{
\item{y}{Vector or factor of response variable data in the training set, with
   codes 0,1,...\code{m}-1, except with \code{knnCalib}, where it
   must be an R factor.}
\item{trnScores}{Vector/matrix of scores in training set.  In the
   multiclass case, there will be as many columns as classes.}
\item{newScores}{Vector/matrix of scores for the cases to be predicted. 
   In the multiclass case, there will be as many columns as classes.}
\item{data}{A data frame, containing "X" and "Y" data.}
\item{yName}{Name of the column in \code{data} corresponding to "Y", the
   class labels.  Must be an R factor.}
\item{logitClass}{Object returned by \code{logitClass}.}
\item{linClass}{Object returned by \code{linClass}.}
\item{newx}{Data frame of one or more new cases to predict.}
\item{classNames}{Names of the classes, in "levels" order.}
\item{fittedY}{Predicted classes in the trained model.}
\item{newx}{Data frame of one or more new cases to predict.}
\item{newX}{Data frame of one or more new cases to predict.}
\item{k}{Number of nearest neighbors.}
\item{scaleX}{Scale the features.}
\item{x}{X data frame or matrix.}
\item{pairs}{Two-row matrix, column i of which is a pair of predictor
   variables to graph.}
\item{cex}{Symbol size for plotting.}
\item{band}{If \code{band} is non-NULL, only points within \code{band}, 
   say 0.1, of est. P(Y = 1) are displayed, for a contour-like effect.}
\item{otrnxy}{Data frame, one data point 
   per row, Y in some column as a factor. Must have column
   names.}
\item{yname}{Name of the Y column.}
\item{object}{Needed for consistency with generic.}
\item{...}{Needed for consistency with generic.}
\item{xdata}{X and associated neighbor indices. Output of
  \code{preprocessx}.} 
\item{coefmat}{Output from \code{ovalogtrn} or \code{avalogtrn}.}
\item{k}{Number of nearest neighbors.} 
\item{predx}{Data points to be predicted.} 
\item{predpts}{Data points to be predicted. Must be specified by
   argument name, i.e. 'predpts = '.} 
\item{m}{Number of classes in multiclass setting.}
\item{econdprobs}{Estimated conditional class probabilities, given the
   predictors.}
\item{wrongprob1}{Incorrect, data-provenanced, unconditional P(Y = 1).}
\item{trueprob1}{Correct unconditional P(Y = 1).}
\item{trueclassprobs}{True unconditional class probabilities, typically
   obtained externally.}
\item{}{}
}

\details{

These functions aid classification in the multiclass setting.  

The \code{knnCalib} function computes conditional probabilities for
new cases, using  the training scores, e.g. \code{decision.values} for
SVM in the \pkg{e1071} package or the outputted conditional
probabilities from \bold{glm} in a logistic model.  This is useful for
classification algorithms that lack inherent estimation of these
probabilities, or for which the inherent probability estimates are not
very accurate, say on the edges of the feature space.  This is a new
approach.  (The function \code{scoresToProbs} is identical, and is
deprecated.)

One needs to have to scores for the new cases to be predicted.  This is
application/package-specific.  The function \code{getDValsE1071} does
this in the case of SVM in the \pkg{e1071} package. 

How it works, say for one new case: The score(s) (more than 1 if
multiclass case) are compared to the training set scores, taking the
\code{k} nearest neighbors. Among the corresponding Y values, we find the
proportion for each class.

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

\value{

The function \code{knnCalib} returns the matrix of conditional
probabilities for the new cases to be predicted, one column per class.

}

\examples{

\dontrun{


data(oliveoils) 
oo <- oliveoils[,-1] 
lncout <- linClass(oo,'Region') 
predict(lncout,oo[222,-1]) 
# predicts class South

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

