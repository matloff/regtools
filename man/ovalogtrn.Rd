\name{avalogtrn,avalogpred,ovalogtrn,ovalogpred,ovaknntrn,ovaknnpred,classadjust}
\alias{ovalogtrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{ovaknntrn}
\alias{ovaknnpred}
\alias{classadjust}

\title{Classification with More Than 2 Classes}

\description{
OVA, AVA tools for multiclass classification.
}

\usage{
ovalogtrn(m,trnxy,truepriors=NULL)
ovalogpred(coefmat,predx)
avalogtrn(m,trnxy)
avalogpred(m,coefmat,predx)
preprocessx(x, kmax) 
ovaknntrn(y,xdata,m,k,truepriors=NULL) 
ovaknnpred(xdatarf,predpts) 
classadjust(econdprobs,wrongratio,trueratio) 
}

\arguments{
\item{trnxy}{Data matrix, one data point per row, Y in the last
  column.}
\item{y}{Vector of response variable data in the training set.}
\item{xdata}{X and associated neighbor indices. Output of
  \code{preprocessx}.} 
\item{xdatarf}{See \code{xdata}.}
\item{coefmat}{Output from \code{ovalogtrn}.}
\item{k}{Number of nearest neighbors.} 
\item{kmx}{Maximal number of nearest neighbors.} 
\item{predpts}{A matrix or data frame of X values, at which Y is to be
  predicted.}
\item{predx}{As with \code{predpts}} 
\item{x}{X data, i.e. predictors, one row per data point, in the training
  set.}
\item{m}{Number of classes in multiclass setting.}
\item{econdprobs}{Estimated conditional class probabilities, given the
predictors.}
\item{wrongratio}{Incorrect, data-provenanced, p/(1-p), with p being
the unconditional probability of a certain class.}
\item{trueratio}{Same as \code{wrongratio}, but with the correct
value.}
\item{truepriors}{True unconditional class probabilities, typically
obtained externally.}
}

\details{

These functions do classification in the multiclass setting, using
the One vs.\ All method.  In the logit case, All vs.\ All is also
offered.  In addition to logit, the k-Nearest Neighbor method is
available.  For this, \code{preprocessx} must first be called.

The functions \code{ovalogtrn}, \code{avalogtrn} and \code{ovaknntrn}
are used on the training set, and then feed into the prediction
functions, \code{ovalogpred}, \code{avalogpred} and \code{ovaknnpred}. 

}

\value{

The prediction functions, \code{ovalogpred}, \code{avalogpred} and
\code{ovaknnpred}, return the predicted classes for the points in
\code{predx} or \code{predpts}.

The functions \code{ovalogtrn} and \code{avalogtrn} return the
estimated logit coefficent vectors, one per column. There are
\code{m} of them in the former case, \code{m}\code{m-1}/2 in the
latter, in which case the order of the R function \code{combin} is
used.

The function \code{ovaknntrn} returns a copy of the \code{xdata} input,
but with an extra component added.  The latter is the matrix of
estimated regression function values; the element in row i, column j, is
the probability that Y = j given that X = row i in the X data. 

}

\examples{

\dontrun{
# UCI Vertebral Column data
# sample read:
vert <- read.table('~/Research/Data/Vertebrae/column_3C.dat',header=F)
# change to 0,1,2 class codes
vert$V7 <- as.numeric(vert$V7) - 1

# logit
ovout <- ovalogtrn(3,vert)
predy <- ovalogpred(ovout,vert[,-7])
# proportion correctly classified
mean(predy == vert$V7)

# kNN
xdata <- preprocessx(vert[,-7],50)
ovout <- ovaknntrn(vert[,7],xdata,3,50)
predy <- ovaknnpred(ovout,vert[,-7])
# proportion correctly classified
mean(predy == vert$V7) 

# sim data
n <- 1500  
# within-grp cov matrix 
cv <- rbind(c(1,0.2),c(0.2,1))  
xy <- NULL  
library(mvtnorm)  
for (i in 1:3)  
   xy <- rbind(xy,rmvnorm(n,mean=rep(i*2.0,2),sigma=cv))  
library(dummies) 
y <- rep(0:2,each=n) 
xy <- cbind(xy,y) 
xdata <- preprocessx(xy[,-3],20) 
oo <- ovaknntrn(y,xdata,m=3,k=20) 
predout <- predict(oo,xy[,-3])
mean(preds == y)

# UCI Letter Recognition data
library(mlbench)
data(LetterRecognition)#
# prep data
lr <- LetterRecognition
# code Y values
lr[,1] <- as.numeric(lr[,1]) - 1
# training and test sets
lrtrn <- lr[1:14000,]
lrtest <- lr[14001:20000,]
# priors
tmp <- table(lrtrn[,1])
wrongpriors <- tmp / sum(tmp)
data(ltrfreqs)
ltrfreqs <- ltrfreqs[order(ltrfreqs[,1]),]
truepriors <- ltrfreqs[,2] / 100
# wtd test data
newidxs <- sample(0:25,6000,replace=T,prob=truepriors)
lrtest1 <- lrtest[newidxs,]
# kNN
xdata <- preprocessx(lrtrn[,-1],50)
# without setting priors
trnout <- ovaknntrn(lrtrn[,1],xdata,26,50)
ypred <- ovaknnpred(trnout,lrtest[,-1])
# how well did it work?
mean(ypred == lrtest[,1])  # 0.86
# wtd test set, but still not setting priors
ypred <- ovaknnpred(trnout,lrtest1[,-1])
mean(ypred == lrtest1[,1])  # only about 0.75
# refit with true priors
trnout1 <- ovaknntrn(lrtrn[,1],xdata,26,50,truepriors)
ypred <- ovaknnpred(trnout1,lrtest1[,-1])
mean(ypred == lrtest1[,1])  # about 0.89
# logit
ologout <- ovalogtrn(26,lr[,c(2:17,1)])
ypred <- ovalogpred(ologout,lr[,-1])
mean(ypred == lr[,1])  # only 0.73
# try quadratic terms
for (i in 2:17)
   lr <- cbind(lr,lr[,i]^2)
ologout1 <- ovalogtrn(26,lr[,c(2:33,1)])
ypred <- ovalogpred(ologout1,lr[,-1])
mean(ypred == lr[,1])  # up to 0.82

}

}

\author{
Norm Matloff
}

