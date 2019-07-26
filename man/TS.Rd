\name{TStoX}
\alias{TStoX}
\alias{TStoMat}

\title{Transform Time Series to Rectangular Form}

\description{
Inputs a time series and transforms it to a form suitable for prediction
using \code{lm} etc.
}

\usage{
TStoX(x,lg,y=NULL)
TStoMat(xmat,lg,y)
}

\arguments{
   \item{x}{A vector.}
   \item{lg}{Lag, a positive integer.}
   \item{xmat}{A matrix, data frame etc.}
   \item{y}{A vector.  If NULL, it is set to \code{x}.}
}

\details{
    \code{TStoX} is for transforming vectors, while \code{TStoMat}
    handles the multivariate time series case.
}

\value{

    Let \code{m} denote length of \code{x}, and in the matrix input case, let
    \code{p} be the number of rows in \code{xmat}, 1 otherwise.  The 
    return value is a matrix with \code{m-lg} rows.  There will be
    \code{p*lg+1} columns.

}

\examples{

}

}

\author{
Norm Matloff
}

