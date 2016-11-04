\name{ridgelm,plot.rlm}
\alias{ridgelm}
\alias{plot.rlm}

\title{Ridge Regression}

\description{Similar to \code{lm.ridge} in \code{MASS} packaged included
with R, but with a different kind of scaling and a little nicer
plotting.
}

\usage{
ridgelm(xy,lambda = seq(0.01,1,0.01),mapback=TRUE) 
\method{plot}{rlm}(x,y,...)
}

\arguments{
  \item{xy}{Data, response variable in the last column.}
  \item{lambda}{Vector of desired values for the ridge parameter.}
  \item{mapback}{If TRUE, the scaling that had been applied to the
     original data will be map back to the original scale, so that the
     estimated regression coefficients are now on the scale of the original
     data.}
  \item{x}{Object of type 'rlm', output of \code{ridgelm}.}
  \item{y}{Needed for consistency with the generic. Not used.}
  \item{...}{Needed for consistency with the generic.  Not used.}
}

\details{
   
   Centers and scales the predictors X, and centers the response
   variable Y.  Computes X'X and then solves [(X'X)/n + lambda I]b =
   X'Y/n for b.  The 1/n factors are important, making the diagonal
   elements of (X'X)/n all 1s and thus facilitating choices for the
   lambdas in a manner independent of the data.

   Calling \code{plot} on the output of \code{ridgelm} dispatches to
   \code{plot.rlm}, thus diplaying the ridge traces.
   
}

\value{

   The function \code{ridgelm} returns an object of class 'rlm', with
   components \code{bhats}, the estimated beta vectors, one column per
   lambda value, and \code{lambda}, a copy of the input.

}

\author{
Norm Matloff
}

