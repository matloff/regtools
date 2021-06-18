\name{misc}
\alias{replicMeans}
\alias{stdErrPred}
\alias{pythonBlankSplit}
\alias{stopBrowser}
\alias{doPCA}

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
}

\arguments{
  \item{x}{Matrix or data frame.}
  \item{pcaProp}{Fraction in [0,1], specifying number of PCA components
     to compute, in terms of fraction of total variance.}
  \item{regObj}{An object of class \code{'lm'} or similar, for which 
     there is a \code{vcov} generic function.
  \item{xnew}{New X value to be predicted.}
  \item{nrep}{Number of replications.}
  \item{s}{A character string.}
  \item{toReplic}{Function call(s), as a quoted string, separated by
     semicolons if more than one call.}
  \item{timing}{If TRUE, find average elapsed time over the replicates.}
  \item{msg}{Character string, error message for existing debug browser.}
}

\details{
}

\examples{

}

\author{
Norm Matloff
}

