\name{unscale}
\alias{unscale}

\title{Miscellaneous Utilities}

\description{
Utilities.
}

\usage{
unscale(scaledx,ctrs=NULL,sds=NULL)
}

\arguments{
  \item{scaledx}{Matrix.}
  \item{ctrs}{Take the original means to be \code{ctrs}}
  \item{sds}{Take the original standard deviations to be \code{sds}}
}

\value{
The function \code{unscale} returns the original object to which
\code{scale} had been applied.  Or, the attributes \code{ctrs} and
\code{sds} can be specified by the user.
}

\author{
Norm Matloff
}

