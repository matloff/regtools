\name{mvrlm,predict.mvrlm}
\alias{mvrlmpwplot,predict.mvrlmpwplot}

\title{Linear Methods for Classification}

\description{
Multiclass prediction using linear models.
}

\usage{
mvrlm(x,y,yname)
\method{predict}{mvrlm}(object,...) 
}

\arguments{
\item{x}{X data matrix or data frame.}
\item{y}{Vector, factor or matrix; see Details}
\item{yname}{Basename for dummy variable Y columns}
\item{mvrlmObj}{Object of type returned by \code{mvrlm}}
\item{newx}}{Data frame of X data to be predicted, consistent with the
   \code{x} input to \code{mvrlm}.
}

\details{

Alternative to logit for multiclass prediction, using purely linear
models.  Faster than \cde{glm}, and may be as effective or better.

The argument \code{y} will eventually be a matrix of dummy variables for
the Y data.  It can be input that way to begin with, but the original
argument can be either a vector of class codes or a class factor, to be
converted to dummies.

}

\value{

The \code{mvrlm} function returns an object of class \code{"mvrlm"}.
The corresponding \code{predict} returns a vector of class names, the
predicted classes.

}

\author{
Norm Matloff
}

