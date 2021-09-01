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
  \item{ctrs}{Take the original means to be \code{ctrs}}
  \item{lst}{An R list.}
  \item{sds}{Take the original standard deviations to be \code{sds}}
  \item{dfRow}{A row in a data frame.}
  \item{d}{A data frame or matrix.}
  \item{scalePars}{If not NULL, a 2-row matrix, with column \code{i} storing
     the min and max values to be used in scaling column \code{i} of \code{m}.
     Typically, one has previously called \code{mmscale} on a dataset and
     saved the resulting scale parameters, and we wish to use those
     same scale parameters on new data.}
  \item{p}{If \code{m} is a vector, this specifies the 
     number of columns it should have as a matrix. The code will try to take 
     care of this by itself if \code{p} is left at NULL.} 
}

\value{
The function \code{unscale} returns the original object to which
\code{scale} had been applied.  Or, the attributes \code{ctrs} and
\code{sds} can be specified by the user.
}

\details{

The function \code{mmscale} is meant as a better-behaved alternative to
\code{scale}.  Using minimum and maximum values, it maps variables to
[0,1], thus avoiding the problems arising from very small standard
deviations in \code{scale}.

The function \code{catDFRow} nicely prints a row of a data frame.

The function \code{constCols} determines which columns of a data frame
or matrix are constant, if any.
}

\author{
Norm Matloff
}

