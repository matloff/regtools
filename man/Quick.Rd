\name{qLogit}
\alias{qLogit}
\alias{qLin}
\alias{qKNN}
\alias{qRF}
\alias{qSVM}
\alias{qBoost}
\alias{predict.qLogit}
\alias{predict.qLin}
\alias{predict.qKNN}
\alias{predict.qRF}
\alias{predict.qSVM}
\alias{predict.qBoost}

\title{Quick Classification Wrappers}

\description{
Quick access to multiclass methods, with a uniform interface.  For
advanced work, analysts should use the methods directly.  Analogous to
the role of \code{qplot} in the \pkg{ggplot2} package.
}

\usage{
qLogit(data, yName,classif=TRUE)
qLin(data, yName,classif)
qKNN(data, yName, k, scaleX = TRUE,classif) 
qRF(data, yName,classif) 
qSVM(data, yName,classif) 
qBoost(data, yName,classif) 
}

\arguments{
  \item{data}{Dataframe, training set. Class labels column is an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{k}{Number of nearest neighbors} 
  \item{scaleX}{Scale the features.} 
}

\details{

As noted, these functions are intended for quick, first-level analysis
of multiclass classification data. Emphasis here is on convenience. In
most cases, the full basket of function options is not reflected, and
second-level analysis should use the relevant packages directly.

Each function has a \code{predict} method. Arguments are at least:
\code{object}, the return value of the previously-called \code{*Class}
function, and \code{newx}, a data frame of points to be predicted.  
In some cases, there are additional parameters.
   
}

\value{

The \code{*Class} functions return an R list, with components:
\code{ypreds}, R factor instance of predicted class labels, one element 
for each row of \code{newx}, and  
\code{conditprobs}, vector/matrix of class probabilities; in the 2-class
case, a vector, the probabilities of Y = 1.

}

\examples{

}

\author{
Norm Matloff
}

