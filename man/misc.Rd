\name{misc}
\alias{replicMeans}
\alias{stdErrPred}
\alias{pythonBlankSplit}
\alias{stopBrowser}
\alias{doPCA}
\alias{PCAwithFactors}
\alias{ulist}
\alias{prToFile}
\alias{partTrnTst}
\alias{findOverallLoss}
\alias{getNamedArgs}
\alias{multCols}
\alias{probIncorrectClass}
\alias{propMisclass}

\title{Utilities}

\description{
Various helper functions.
}

\usage{
replicMeans(nrep,toReplic,timing=FALSE)
stdErrPred(regObj,xnew)
pythonBlankSplit(s)
stopBrowser(msg = stop("msg not supplied"))
doPCA(x,pcaProp)
PCAwithFactors(x, nComps = ncol(x))
ulist(lst)
prToFile(filename)
partTrnTst(fullData,nTest=min(1000,round(0.2*nrow(fullData))))
findOverallLoss(regests,y,lossFtn = MAPE) 
getNamedArgs(argVec)
multCols(x,cols,vals)
probIncorrectClass(yhat, y, startAt1 = TRUE)
propMisclass(y,yhat)
}

\arguments{
  \item{regests}{Fitted regression estimates, training set.}
  \item{y}{Y values, training set.}
  \item{yhat}{Predicted Y values}
  \item{startAt1}{TRUE if indexing starts at 1, FALSE if starting at 0.}
  \item{lossFtn}{Loss functin.}
  \item{fullData}{A data frame or matrix.}
  \item{nTest}{Number of rows for the test set.}
  \item{filename}{Name of output file.}
  \item{lst}{An R list.}
  \item{x}{Matrix or data frame.}
  \item{pcaProp}{Fraction in [0,1], specifying number of PCA components
     to compute, in terms of fraction of total variance.}
  \item{nComps}{Number of PCA components.}
  \item{regObj}{An object of class \code{'lm'} or similar, for which 
     there is a \code{vcov} generic function.}
  \item{xnew}{New X value to be predicted.}
  \item{nrep}{Number of replications.}
  \item{s}{A character string.}
  \item{toReplic}{Function call(s), as a quoted string, separated by
     semicolons if more than one call.}
  \item{timing}{If TRUE, find average elapsed time over the replicates.}
  \item{msg}{Character string, error message for existing debug browser.}
  \item{argVec}{R list or vector with named elements.}
  \item{cols}{A set of column numbers.}
  \item{vals}{A set of positive expansion numbers.}
}

\details{

The function \code{PCAwithFactors} is a wrapper for
\code{stats::prcomp}, to be used on data frames that contain at least on
R factor.

}

\value{

The function \code{PCAwithFactors} returns an object of class
'PCAwithFactors'. with components \code{pcout}, the object returned by
the wrapped call to \code{prcomp}; \code{factorsInfo}, factor conversion
information to be used with \code{predict}; and \code{preds}, the PCA
version of \code{x}.

The function \code{getNamedArgs} will assign in the caller's space
variables with the names and values in \code{argVec}.

}

\examples{

w <- list(a=3,b=8)
getNamedArgs(w)  
a
b
u <- c(5,12,13)
names(u) <- c('x','y','z')
getNamedArgs(u)
x
y
z

}

\author{
Norm Matloff
}

