\name{unscale}
\alias{unscale}
\alias{mmscale}
\alias{catDFRow}
\alias{constCols}
\alias{allNumeric}

\title{Miscellaneous Utilities}

\description{
Utilities.
}

\usage{
unscale(scaledx,ctrs=NULL,sds=NULL)
mmscale(m,scalePars=NULL,p=NULL)
catDFRow(dfRow)
constCols(d)
allNumeric(lst)
}

\arguments{
  \item{scaledx}{A matrix.}
  \item{m}{A matrix.}
  \item{scalePars}{If not NULL, a 2-row matrix, with column storing
     the min and max values to be used in scaling column \code{i}
     \code{m}.}
  \item{ctrs}{Take the original means to be \code{ctrs}}
  \item{lst}{An R list.}
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

