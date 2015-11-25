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
ovalogtrn(m,trnxy)
ovalogpred(coefmat,predx)
avalogtrn(m,trnxy)
avalogpred(m,coefmat,predx)
ovaknntrn(y,xdata,m,k) 
ovaknnpred(xdatarf,predpts) 
classadjust(econdprobs,wrongratio,trueratio) 

}

\arguments{
  \item{trnxy}{Data matrix, one data point per row, Y in the last
     column.}
  \item{y}{Vector of response variable data in the training set.}
  \item{xdata}{X and associated neighbor indices. Output of
     \code{preprocessx}.} 
  \item{xdatarf}{The augmented \code{xdata}, output of \code{knnest} or 
     \code{ovaknntrn}.} 
  \item{k}{Number of nearest neighbors} 
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
  values.}
}

\details{

   These functions do classification in the multiclass setting, using
   the One vs.\ All method.  In the logit case, All vs.\ All is also
   offered.  In addition to logit, the k-Nearest Neighbor method is
   available.  For this, \code{preprocessx} must first be called.
   
   The functions \code{ovalogtrn} and \code{avalogtrn} are used on the
   training set, and then feed into the prediction functions,
   \code{ovalogpred}, \code{avalogpred} and \code{ovaknnpred}. 
   
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

}

\examples{

\dontrun{
# assumes UCI Vertebral Column data already downloaded from
# https://archive.ics.uci.edu/ml/machine-learning-databases/00212/vertebral_column_data.zip
# and unzipped; example read:
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
}

library(mlbench)
data(LetterRecognition)
lr <- LetterRecognition
# code Y values
lr[,1] <- as.numeric(lr[,1]) - 1
# training and test sets
lrtrn <- lr[1:14000,]
lrtest <- lr[14001:20000,]
# apply kNN to training set
xdata <- preprocessx(lrtrn[,-1],50)
trnout <- ovaknntrn(lrtrn[,1],xdata,26,50)
# then to test set
ypred <- ovaknnpred(trnout,lrtest[,-1])
# how well did we do?
mean(ypred == lrtest[,1])

}

\author{
Norm Matloff
}

