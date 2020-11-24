\name{Classification Calibration}
\alias{knnCalib}
\alias{plattCalib}

\title{}

\description{

Alternatives to Platt scaling for calibration of posterior probabilities
in classification models.

}

\usage{
knnCalib(y,trnScores,newScores,k)
plattCalib(y,trnScores,newScores,k)
calibWrap(qeout,scores,calibMethod,k = NULL,plotsPerRow=2,nBins=0)
reliabDiagram(y,probs,nBins,plotGraph=TRUE)
}

\arguments{
  \item{y}{Class labels from the training set, an R factor.}
  \item{trnScores}{Scores from the training set.}
  \item{newScores}{Scores from the data points to be predicted.}
  \item{k}{Number of nearest neighbors (\code{knnCalib}.}
  \item{qeout}{Output from one of the \code{qe*}-series functions.}
  \item{scores}{Training set scores.}
  \item{calibMethod}{At present, either 'knnCalib' or 'plattCalib.}
  \item{plotsPerRow}{Number of reliability diagrams per screen row.}
  \item{plotGraph}{If TRUE, plot the graph of the probability-smoothed Y
     pairs.}
}

\details{

In many applications of classification methods, the posterior
probabilities, i.e. the conditional class probabilities, are of major
interest.  However, some machine learning methods for 
classification, notably SVM, do not inherently generate estimates of these
probabilities, and for some that do, the estimates may be poor.

A widely-used approach to this problem is Platt scaling, which works by
assuming a logistic model for the posterior probability as a function of
the classification score, e.g. the "decision values" in SVM. But this
model may be flawed.

The function \code{knnCalib} provides a model-free alternative to the
problem, using a k-Nearest Neighbor approach.  We also supply a
multi-input version of Platt.  

Studies of Platt and related methods often plot \emph{reliability
diagrams} as a visual goodness-of-fit criterion.  The function
\code{reliabDiagram} graphs such a plot.  Whether the plot is drawn or
not, the (fittedYCounts, actualYCounts) pairs are returned.

As a convenience, \code{calibWrap} is a wrapper for the process.

}

\author{
Norm Matloff
}

