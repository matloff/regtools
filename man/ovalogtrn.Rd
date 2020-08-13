\name{avalogtrn,avalogpred,ovalogtrn,ovalogpred,ovaknntrn,predict.ovaknn, boundaryplot,logitClass,linClass,knnClass}
\alias{ovalogtrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{predict.ovaknn}
\alias{classadjust}
\alias{confusion}
\alias{factorTo012ec}
\alias{classadjust}
\alias{boundaryplot}
\alias{knnClass}
\alias{linClass}
\alias{logitClass}

\title{Classification with More Than 2 Classes}

\description{
Tools for multiclass classification, parametric and nonparametric.
}

\usage{
logitClass(data,yName)
\method{predict}{logitClass}(object,newx)
linClass(data,yName)
\method{predict}{linClass}(object,newx)
knnClass(data,yName,k)
\method{predict}{knnClass}(object,newx)
ovalogtrn(...)
\method{predict}{ovalog}(object,...) 
avalogtrn(m,trnxy)
avalogpred(m,coefmat,predx,trueclassprobs=NULL)
classadjust(econdprobs,wrongprob1,trueprob1) 
boundaryplot(y,x,regests,pairs=combn(ncol(x),2),pchvals=2+y,cex=0.5,band=0.10)
}

\arguments{
\item{data}{A data frame, containing "X" and "Y" data.}
\item{yName}{Name of the column in \code{data} corresponding to "Y", the
   class labels.  Must be an R factor.}
\item{logitClass}{Object returned by \code{logitClass}.}
\item{linClass}{Object returned by \code{linClass}.}
\item{newx}{Data frame of one or more new cases to predict.}
\item{x}{X data matrix.}
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
\item{y}{Vector or factor of response variable data in the training set, with
   codes 0,1,...\code{m}-1.}
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
}

\details{

These functions do classification in the multiclass setting.  

For most uses, the convenience "xClass" functions, e.g.
\code{logitClass} should be used, with their simple, uniform interface.

Currently, the package includes the following "xClass" functions:

\code{knnClass}, \code{linClass}, \code{logitClass}

If you have matrix data, convert using \code{as.data.frame}.  Similarly,
if your entity of class labels is an R vector, convert using
\code{as.factor}.

The function \code{linClass} is a computationally-fast alternative to
\code{logitClass} in prediction contexts (as opposed to settings in
which the goal is description, e.g. inference on coefficients). 
For prediction purposes, the two will typically give very similar
results.  This is because (a) the logistic function is close to linear
in a broad range and (b) for prediction, all that matters is whether the
esimated regression value is above or below a given threshold, e.g. 0.5,
or in the multiclass, whether a given class has the highest estimated
value.  Note:  For this function, class labels must start with a letter.

OVA vs. AVA:  The functions \code{logitClass} and \code{linClass} use
the "One vs. All" approach (OVA) while \code{avatrn} uses "All vs. All"
(AVA).

}

\value{

The "xClass" functions return an object to be used in predicting future
cases, using a generical \code{predict} call.  The corresponding generic
\code{predict} functions return a vector of predicted class labels.
Except for \code{linClass}, the return value also has an R attribute
\code{probs}, the matrix of estimated class probabilities

The functions \code{ovalogtrn} and \code{avalogtrn} return the
estimated logit coefficent vectors, one per column. There are
\code{m} of them in the former case, \code{m}\code{m-1}/2 in the
latter, in which case the order of the R function \code{combin} is
used.

The prediction functions, \code{predict.ovalog}, \code{predict.avalog} and
\code{predict.ovaknn}, return the predicted class codes (integers
0,1,...) for the points in \code{predx} or \code{predpts}.  The 
corresponding conditional class probabilities are included as an attribute, 
accessible via \code{probs}.

}

\examples{

\dontrun{

data(peFactors)
pef <- peFactors[,c(1,3,5,7:9)]
lgcout <- logitClass(pef,'occ')
predict(lgcout,pef[8000,-3])
# predicts class 102, prob. 0.42

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

