\name{avalogtrn,avalogpred,ovalogtrn,ovalogpred,knntrn,predict.ovaknn,pwplot}
\alias{ovalogtrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{knntrn}
\alias{predict.ovaknn}
\alias{classadjust}
\alias{pwplot}

\title{Classification with More Than 2 Classes}

\description{
One vs. All, All vs. All tools for multiclass classification, parametric
and nonparametric.
}

\usage{
ovalogtrn(m,trnxy,truepriors=NULL)
ovalogpred(coefmat,predx,probs=FALSE)
avalogtrn(m,trnxy)
avalogpred(m,coefmat,predx)
knntrn(y,xdata,m,k,truepriors=NULL) 
\method{predict}{ovaknn}(object,...) 
classadjust(econdprobs,wrongratio,trueratio) 
pwplot(y,x,k,pairs=combn(ncol(x),2),cexval=0.5,band=NULL) 
}

\arguments{
\item{x}{X data matrix or data frame.}
\item{pairs}{Two-row matrix, column i of which is pair i of predictor
   variables to graph.}
\item{cexval}{Symbol size for plotting.}
\item{band}{If \code{band} is non-NULL, only points within \code{band}, 
   say 0.1, of est. P(Y = 1) are displayed, for a contour-like effect.}
\item{trnxy}{Data matrix (not a data frame), one data point 
   per row, Y in the last column.  Y must be numeric, 0,1,2,... \code{m-1}.}
\item{object}{Needed for consistency with generic.}
\item{...}{Needed for consistency with generic.}
\item{y}{Vector of response variable data in the training set, with
   codes 0,1,...\code{m}-1.}
\item{xdata}{X and associated neighbor indices. Output of
  \code{preprocessx}.} 
\item{coefmat}{Output from \code{ovalogtrn}.}
\item{k}{Number of nearest neighbors.} 
\item{predx}{One data point to be predicted.} 
\item{m}{Number of classes in multiclass setting.}
\item{econdprobs}{Estimated conditional class probabilities, given the
predictors.}
\item{wrongratio}{Incorrect, data-provenanced, p/(1-p), with p being
the unconditional probability of a certain class.}
\item{trueratio}{Same as \code{wrongratio}, but with the correct
value.}
\item{truepriors}{True unconditional class probabilities, typically
obtained externally.}
\item{probs}{If TRUE, return the estimated conditional probabilities of class
membership.}
}

\details{

These functions do classification in the multiclass setting.  During
training, the \code{ova*} functions use the One vs. All method:  For
each class i, \code{as.integer(Y == i)} is regressed against the
predictors, yielding a vector of beta coefficients for each class.  For
prediction, the new X is used with each such vector in the logit
function, producing a conditional probability for Y == i over all
classes i.  The predicted value is then the i with maximal probability.

The \code{ava*} functions use the All vs. All method:  During training,
for each pair of classes i and j, i less than j, the data are restricted
to cases having those classes.  \code{as.integer(Y == i)} is regressed
against the predictors, yielding a vector of beta coefficients for each
pair.  In prediction, the new X is used with each such vector in the
logit function, producing a conditional probability for Y == i over all
classes i and j, resulting in a "winner" between i and j.  The
predicted value is then the class with the most "wins."

In addition to logit, the k-Nearest Neighbor method is available.  For
this, \code{preprocessx} must first be called.  In the logit case, All
vs. All is also offered.  

The functions \code{ovalogtrn}, \code{avalogtrn} and \code{knntrn} are
used on the training set, and then fed into the prediction functions,
\code{ovalogpred}, \code{avalogpred} and \code{predict.ovaknn}.  The
arguments for the latter are the output of \code{knntrn} and a matrix of
prediction points (internally referred to as \code{predpts} in the
code), one per row.

In \code{pwplot}, \code{y} must be a column of 0s and 1s.  For each pair
of predictor columns X12 in \code{x}, we compute estimated P(Y = 1) and P(Y
= 1 | X12).  If the latter is larger, plot a '1', else a '0'. The plot
is intended to be helpful in exploring a relation between Y and X.  For
non-NULL \code{band}, a contour-like plot is made, showing where X12
changes from making Y = 1 less likely to more likely.

}

\value{

The prediction functions, \code{ovalogpred}, \code{avalogpred} and
\code{predict.ovaknn}, return the predicted classes for the points in
\code{predx} or \code{predpts}.

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
# toy example, kNN
set.seed(9999)
x <- runif(25)
y <- sample(0:2,25,replace=TRUE)
xd <- preprocessx(x,2,xval=TRUE)
kout <- knntrn(y,xd,m=3,k=2)
kout$regest  # row 2:  0.0,0.5,0.5
predict(kout,matrix(c(0.81,0.55,0.15),ncol=1))  # 0,1,2

# sim data, kNN
set.seed(9999)
n <- 1500  
# within-grp cov matrix 
cv <- rbind(c(1,0.2),c(0.2,1))  
xy <- NULL  
for (i in 1:3)  
   xy <- rbind(xy,rmvnorm(n,mean=rep(i*2.0,2),sigma=cv))  
y <- rep(0:2,each=n) 
xy <- cbind(xy,y) 
xdata <- preprocessx(xy[,-3],20) 
oo <- knntrn(y,xdata,m=3,k=20) 
predout <- predict(oo,xy[,-3])
mean(predout$predy == y)  # about 0.87

library(mlbench)
data(Vehicle)
xdata <- preprocessx(Vehicle[,-19],25)
kout <- knntrn(Vehicle$Class,xdata,k=25)
predict(kout,as.matrix(Vehicle[1,-19])) # predicted Y is 3

# UCI Letter Recognition data
data(LetterRecognition)
# prep data
lr <- LetterRecognition
# code Y values
lr[,1] <- as.numeric(lr[,1]) - 1
# training and test sets
lrtrn <- lr[1:14000,]
lrtest <- lr[14001:20000,]
# kNN
xdata <- preprocessx(lrtrn[,-1],50)
# without setting priors
trnout <- knntrn(lrtrn[,1],xdata,26,50)
ypred <- predict(trnout,as.matrix(lrtest[,-1]))
# how well did it work?
mean(ypred$predy == lrtest[,1])  # 0.86
# logit
ologout <- ovalogtrn(26,lr[,c(2:17,1)])
ypred <- ovalogpred(ologout,lr[,-1])
mean(ypred == lr[,1])  # only 0.73
# try quadratic terms
for (i in 2:17)
   lr <- cbind(lr,lr[,i]^2)
ologout1 <- ovalogtrn(26,lr[,c(2:33,1)])
ypred <- ovalogpred(ologout1,lr[,-1])
mean(ypred == lr[,1])  # increased to 0.81

library(mlbench)
data(PimaIndiansDiabetes) 
pima <- PimaIndiansDiabetes 
pima$diabetes <- as.integer(pima$diabetes == 'pos')
pwplot(pima$diabetes,pima[,1:8],25,pairs=cbind(c(2,3),c(2,6),c(6,8)),cex=0.8)
pwplot(pima$diabetes,pima[,1:8],25,pairs=cbind(c(2,3),c(2,6),c(6,8)),cex=0.8,band=0.05)

}

}

\author{
Norm Matloff
}

