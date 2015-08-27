\name{knnest,meany,vary,loclin,knnpred}
\alias{knnest}
\alias{knnpred}
\alias{meany}
\alias{vary}
\alias{loclin}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification.
}

\usage{
knnest(xydata,k,scalefirst=NULL,nearf=meany,outfile=NULL)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
knnpred(knnout,predpts) 
}

\arguments{
  \item{xydata}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{k}{Number of nearest neighbors} 
  \item{predpts}{A matrix or data frame of X values, at which Y is to be
     predicted.}
  \item{predpt}{One point on which to predict, e.g. a row in
     \code{predpts}.}
  \item{scalefirst}{If non-NULL, scale the X data first, using
     \code{scale}. If \code{scalefirst} is 'default', use the
     default for \code{scale}; otherwise, \code{scalefirst} is a
     2-element R list, consisting of a centers vector and a scales
     vector.}
  \item{nearf}{Function to apply to the nearest neighbors of a point..}
     e.g. R formula, \code{data} specification.}
     \code{prcomp}.}
  \item{outfile}{File to save the output of \code{knnest} to, including
     attributes if any.
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

