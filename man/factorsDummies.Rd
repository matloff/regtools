\name{factorsToDummies}
\alias{factorToDummies}
\alias{dummiesToFactor}

\title{Factor Conversion Utilities}

\description{
Utilities from converting back and forth between factors and dummy
variables.
}

\usage{
factorToDummies(f,fname,omitLast=TRUE)
factorsToDummies(dfr,omitLast=TRUE)
dummiesToFactor(dms,inclLast=FALSE) 
}

\details{
Many R users prefer to use R factors in their coding, or work with data
that is of this type to begin with.  On the other hand, many regression
packages, e.g. \pkg{lars}, disallow factors.  These utilities facilitate
conversion from one form to another.
}

\arguments{
  \item{f}{A factor.}
  \item{fname}{A factor name.}
  \item{dfr}{A data frame.}
  \item{omitLast}{If TRUE, then generate only k-1 dummies from k factor
  levels.}
  \item{dms}{A data frame whose columns are dummy variables.}
  \item{inclLast}{If FALSE, then only k-1 dummies for k factor
  levels are provided.}
}

\value{The function \code{factorToDummies} returns a matrix of dummy
variables, while \code{factorsToDummies} returns a new version of the
input data frame, in which each factor is replaced by columns of
dummies.
}

\author{
Norm Matloff
}

