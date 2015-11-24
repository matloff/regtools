\name{avalogtrn,avalogpred,ovalogtrn,ovalogpred,ovaknntrn,ovaknnpred}
\alias{ovalogtrn}
\alias{ovalogpred}
\alias{avalogtrn}
\alias{avalogpred}
\alias{ovaknntrn}
\alias{ovaknnpred}

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
# and unzipped
vert <- read.table('column_3C.dat',header=F)
# change to 0,1,2 class codes
vert$V7 <- as.numeric(vert$V7) - 1

# logit
ovout <- ovalogtrn(3,vert)
predy <- ovalogpred(ovout,vert[,-7])
# proportion correctly classified
mean(predy == vert$V7)
avout <- avalogtrn(3,vert[trnidxs,])
predy <- avalogpred(3,avout,vert[predidxs,1:6])
mean(predy == vert[predidxs,7])

# kNN
ovout <- ovalogtrn(3,vert)
predy <- ovalogpred(ovout,vert[,-7])
# proportion correctly classified
mean(predy == vert$V7)

}

}

\author{
Norm Matloff
}

