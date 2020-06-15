\name{krsFit}
\alias{krsFit}

\title{Tools for Neural Networks}

\description{
Tools to complement existing neural networks software, notably 
a more "R-like" wrapper to fitting data with R's \pkg{keras} package.  
}

\usage{
krsFit(x, y, hidden, acts = rep("relu", length(hidden)), conv = NULL, 
    xShape = NULL, classif = TRUE, nClass = NULL, nEpoch = 30, 
    scaleX = TRUE, scaleY = TRUE) 
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
  \item{xShape}{Vector giving the number of rows and columns, and in the
     convolutional case with multiple channels, the number of channels.}
  \item{classif}{If TRUE, indicates a classification problem.}
  \item{nClass}{Number of classes.}
  \item{nEpoch}{Number of epochs.}
  \item{krsFitOut}{An object returned by \code{krstFit}.}
  \item{newx}{New data points to be predicted.}  
}

\details{
The \code{krstFit} function is a wrapper for the entire pipeline
in fitting the R \pkg{keras} package to a dataset:  Defining the model,
compiling, stating the inputs and so on.  As a result, the wrapper
allows the user to skip those details (or not need to even know them),
and define the model in a manner more familiar to R users.
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

# obtain MNIST training and test sets; the following then uses the
# example network of 

# https://databricks-prod-cloudfront.cloud.databricks.com/public/4027ec902e239c93eaaa8714f173bcfc/2961012104553482/4462572393058129/1806228006848429/latest.html

# converted to use the krsFit wrapper

x <- mntrn[,-785] / 255 
y <- mntrn[,785]
xShape <- c(28,28)

# define convolutional layers
conv1 <- list(type='conv2d',filters=32,kern=3)
conv2 <- list(type='pool',kern=2)
conv3 <- list(type='conv2d',filters=64,kern=3) 
conv4 <- list(type='pool',kern=2)
conv5 <- list(type='drop',drop=0.5)

# call wrapper, 1 dense hidden layer of 128 units, then dropout layer
# with proportion 0.5
z <- krsFit(x,y,conv=list(conv1,conv2,conv3,conv4,conv5),c(128,0.5),                    classif=T,nClass=10,nEpoch=10,xShape=c(28,28),scaleX=F,scaleY=F)

# try on test set
preds <- predict(z,mntst[,-785]/255)
mean(preds == mntst[,785])  # 0.98 in my sample run

}

}

\author{
Norm Matloff
}

