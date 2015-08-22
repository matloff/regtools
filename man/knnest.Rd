\name{knnest,meany,vary,loclin}
\alias{knnest}
\alias{meany}
\alias{vary}
\alias{loclin}

\title{Regression and Classification Tools}

\description{
Tools for linear, nonlinear and nonparametric regression/
classification modeling and prediction.
}

\usage{
knnest(xydata,k,predpts=xydata[,-ncol(xydata)],
   scalefirst=FALSE,nearf=meany)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
}

\arguments{
  \item{xydata}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{k}{Number of nearest neighbors} 
  \item{predpts}{A matrix or data frame of X values, at which Y is to be
     predicted.}
  \item{predpt}{One point on which to predict, e.g. a row in
     \code{predpts}.}
  \item{scalefirst}{If TRUE, run \code{scale} on the data first.} 
  \item{nearf}{Function to apply to the nearest neighbors of a point..}
     e.g. R formula, \code{data} specification.}
     \code{prcomp}.}
}

\details{
   The \code{knnest} function does k-nearest neighbor regression
   function estimation, in any dimension.  In addition to averaging the
   nearby Y values, one can choose local linear smoothing, conditional
   variance estimation or whatever the user desires.
}

\value{The \code{knnest} function the vector of estimated regression
function values.
}

% \examples{
% }

\author{
Norm Matloff
}

