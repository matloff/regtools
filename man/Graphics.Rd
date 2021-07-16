\name{line3d}
\alias{line3d}

\title{Misc. Graphics}

\description{
Graphics utiliites.
}

\usage{
line3d(xyz,clrs=NULL,cex=1.0,
   xlim=NULL,ylim=NULL,
   xlab=NULL,ylab=NULL,legendPos=NULL)
}

\arguments{
  \item{xyz}{A matrix or data frame of at least 3 columns, the first
  3 serving as 'x', 'y' and 'z' coordinates of points to be plotted. 
  Grouping, if any, is specified in column 4, in which case \code{xyz}
  must be a data frame.}
  \item{clrs}{Colors to be used in the grouped case.}
  \item{xlim}{As in \code{plot}.}
  \item{ylim}{As in \code{plot}.}
  \item{xlab}{As in \code{plot}.}
  \item{ylab}{As in \code{plot}.}
  \item{legendPos}{As in \code{legend}.}
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

