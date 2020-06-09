\name{krsFit}
\alias{krsFit}

\title{Tools for Neural Networks}

\description{
Tools to complement existing neural networks software, notably R's
\pkg{keras} package.
}

\usage{
krsFit(x, y, hidden, acts = rep("relu", length(hidden)), conv = NULL, 
    classif = TRUE, nClass = NULL, nEpoch = 30)
predict.krsFit(krsFitOut,newx) 

}

\arguments{
  \item{x}{X data, predictors, one row per data point, in the training
     set.  Must be a matrix.}
  \item{y}{Numeric vector of Y values.  In classification case
     must be integers, not an R factor, and take on the values 1,2,...,
     \code{nClass}}.
   \item{hidden}{Vector of number of units per 
     hidden layer, or the rate for a dropout layer.} 
  \item{acts}{Vector of names of the activation functions.}
  \item{conv}{R list specifying the convolutional layers, if any.}
  \item{classif}{If TRUE, indicates a classification problem.}
  \item{nClass}{Number of classes.}
  \item{nEpoch}{Number of epochs.}
  \item{krsFitOut}{An object returned by \code{krstFit}.}
  \item{newx}{New data points to be predicted.}  
}

\details{

}

\examples{

\dontrun{
library(keras)
data(peDumms) 
ped <- peDumms[,c(1,20,22:27,29,32,31)]
# predict wage income
x <- ped[,-11] 
y <- ped[,11] 
z <- krsFit(x,y,c(50,50,50),classif=FALSE,nEpoch=25) 
preds <- predict(z,x) 
mean(abs(preds-y))  # something like 25000


x <- ped[,-(4:8)] 
y <- ped[,4:8] 
y <- dummiesToInt(y,FALSE) - 1
z <- krsFit(x,y,c(50,50,0.20,50),classif=TRUE,nEpoch=175,nClass=6) 
preds <- predict(z,x)
mean(preds == y)   # something like 0.39

}

}

\author{
Norm Matloff
}

