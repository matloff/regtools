\name{unscale}
\alias{unscale}
\alias{catDFRow}
\alias{constCols}

\title{Miscellaneous Utilities}

\description{
Utilities.
}

\usage{
unscale(scaledx,ctrs=NULL,sds=NULL)
catDFRow(dfRow)
constCols(d)
}

\arguments{
  \item{scaledx}{Matrix.}
  \item{ctrs}{Take the original means to be \code{ctrs}}
  \item{sds}{Take the original standard deviations to be \code{sds}}
  \item{dfRow}{A row in a data frame.}
  \item{d}{A data frame or matrix.}
}

\value{
The function \code{unscale} returns the original object to which
\code{scale} had been applied.  Or, the attributes \code{ctrs} and
\code{sds} can be specified by the user.
}

\details{

The function \code{catDFRow} nicely prints a row of a data frame.

The function \code{constCols} determines which columns of a data frame
or matrix are constant, if any.
}

\author{
Norm Matloff
}

