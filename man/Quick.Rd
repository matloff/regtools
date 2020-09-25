\name{qeLogit}
\alias{qeLogit}
\alias{qeLin}
\alias{qeKNN}
\alias{qeRF}
\alias{qeSVM}
\alias{qeGBoost}
\alias{qeNeural}
\alias{predict.qeLogit}
\alias{predict.qeLin}
\alias{predict.qeKNN}
\alias{predict.qeRF}
\alias{predict.qeSVM}
\alias{predict.qeGBoost}
\alias{predict.qeNeural}

\title{Quick Classification Wrappers}

\description{
Quick access to multiclass methods, with a uniform interface,
intended for convenient initial exploration of a dataset.  For
advanced work, analysts should use the methods directly.  
}

\usage{
qeLogit(data, yName,classif=TRUE)
qeLin(data, yName,classif)
qeKNN(data, yName, k, scaleX = TRUE,classif) 
qeRF(data, yName,classif) 
qeSVM(data, yName,classif) 
qeGBoost(data, yName,classif) 
}

\arguments{
  \item{data}{Dataframe, training set. Class labels column is an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{k}{Number of nearest neighbors} 
  \item{scaleX}{Scale the features.} 
}

\details{

As noted, these functions are intended for quick, first-level analysis
of regression or multiclass classification problems.  Emphasis here is
on convenience. In most cases, the full basket of function options is
not reflected, and second-level analysis should use the relevant
packages directly.

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

