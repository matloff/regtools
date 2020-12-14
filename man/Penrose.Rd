\name{Penrose Linear}
\alias{penroseLM}
\alias{penrosePoly}
\alias{predict.penroseLM}
\alias{predict.penrosePoly}

\title{Penrose-Inverse Linear Models and Polynomial Regression}

\description{

Provides mininum-norm solutions to linear models, identical to OLS in
standard situations, but allowing exploration of overfitting in the
overparameterized case. Also provides a wrapper for the polynomial
case.
}

\usage{
penroseLM(d,yName)
penrosePoly(d,yName,deg,maxInteractDeg=deg)
\method{predict}{penroseLM}(object,newx)
\method{predict}{penrosePoly}(object,newx)

}

\arguments{
  \item{d}{Dataframe, training set.} 
  \item{yName}{Name of the class labels column.}
  \item{deg}{Polynomial degree.}
}

\details{

First, provides a convenient wrapper to the \pkg{polyreg} package for
polynomial regression. (See \code{qePoly} here for an even higher-level
wrapper.)  Note that this computes true polynomials, with
cross-product/interaction terms rather than just powers, and that dummy
variables are handled properly (to NOT compute powers).

Second, provides a tool for exploring the "double descent" phenomenon,
in which prediction error may improve upon fitting past the
interpolation point.

}

\author{
Norm Matloff
}

