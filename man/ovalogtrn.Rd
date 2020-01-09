\name{avalogtrn,avalogpred,ovalogtrn,ovalogpred,ovaknntrn,predict.ovaknn,pwplot}
\alias{ovalogtrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{knntrn}
\alias{predict.ovaknn}
\alias{classadjust}
\alias{confusion}
\alias{factorTo012ec}
\alias{classadjust}
\alias{pwplot}

\title{Classification with More Than 2 Classes}

\description{
One vs. All, All vs. All tools for multiclass classification, parametric
and nonparametric.
}

\usage{
ovalogtrn(,trnxy,yname)
ovalogpred()
\method{predict}{ovalog}(object,...) 
avalogtrn(m,trnxy)
avalogpred(m,coefmat,predx,trueclassprobs=NULL)
knntrn() 
\method{predict}{ovaknn}(object,...) 
classadjust(econdprobs,wrongprob1,trueprob1) 
pwplot(y,x,k,pairs=combn(ncol(x),2),cexval=0.5,band=NULL) 
}

\arguments{
\item{x}{X data matrix or data frame.}
\item{pairs}{Two-row matrix, column i of which is a pair of predictor
   variables to graph.}
\item{cexval}{Symbol size for plotting.}
\item{band}{If \code{band} is non-NULL, only points within \code{band}, 
   say 0.1, of est. P(Y = 1) are displayed, for a contour-like effect.}
\item{trnxy}{Data matrix (not a data frame), one data point 
   per row, Y in the last \code{m} column as dummiess. Must have column
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
   argument name.} 
\item{m}{Number of classes in multiclass setting.}
\item{econdprobs}{Estimated conditional class probabilities, given the
predictors.}
\item{wrongprob1}{Incorrect, data-provenanced, unconditional P(Y = 1).}
\item{trueprob1}{Correct unconditional P(Y = 1).}
\item{trueclassprobs}{True unconditional class probabilities, typically
obtained externally.}
}

\details{

These functions do classification in the multiclass setting.  During
training, the \code{ova*} functions use the One vs. All method:  For
each class i, Y=i is regressed against the predictors.  Subsequent
prediction is done by arg max of the resulting conditional
probabilities.

The \code{avalog*} functions use the All vs. All method:  For
each pair of classes i and j, data is restricted to those classes, and a
1-class regression is done with "Y" being Y=i.  Subsequent prediction is
done for a given new case by calculating the value of i that yields the
most "wins" for i.

In addition to logit, the k-Nearest Neighbor method is available,
via \code{ovaknntrn}.  Note too \code{kNN}.  

In \code{pwplot}, \code{y} must be a column of 0s and 1s.  For each pair
of predictor columns X12 in \code{x}, we compute estimated P(Y = 1) and P(Y
= 1 | X12).  If the latter is larger, plot a '1', else a '0'. The plot
is intended to be helpful in exploring a relation between Y and X.  For
non-NULL \code{band}, a contour-like plot is made, showing where X12
changes from making Y = 1 less likely to more likely.

The function \code{knntrn} is deprecated; use \code{ovaknn}.

}

\value{

The prediction functions, \code{predict.ovalog}, \code{predict.avalog} and
\code{predict.ovaknn}, return the predicted classes for the points in
\code{predx} or \code{predpts}.  The corresponding conditional
class probabilities are included as an attribute, accessible via \code{probs}

The functions \code{ovalogtrn} and \code{avalogtrn} return the
estimated logit coefficent vectors, one per column. There are
\code{m} of them in the former case, \code{m}\code{m-1}/2 in the
latter, in which case the order of the R function \code{combin} is
used.

The function \code{knntrn} returns a copy of the \code{xdata} input,
but with an extra component added.  The latter is the matrix of
estimated regression function values; the element in row i, column j, is
the probability that Y = j given that X = row i in the X data. 

}

\examples{

\dontrun{
# toy example
set.seed(9999)
x <- runif(25)
y <- sample(0:2,25,replace=TRUE)
xd <- preprocessx(x,2,xval=FALSE)
# kout <- knntrn(y,xd,m=3,k=2)
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
# discard the lower educ-level casess, which are rare
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

