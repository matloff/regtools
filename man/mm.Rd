\name{mm}
\alias{mm}

\title{Method of Moments, Including Possible Regression Terms}

\description{

Method of Moments computation for almost any statistical problem that
has derivatives with respect to theta.  Capable of handling models that
include parametric regression terms, but not need be a regression
problem.  (This is not \emph{Generalized} Method of Moments; see the
package \pkg{gmm} for the latter.)

}

\usage{
mm(m,g,x,init=rep(0.5,length(m)),eps=0.0001,maxiters=1000) 
}

\arguments{
  \item{m}{Vector of sample moments, "left-hand sides" of moment
     equations.}
  \item{g}{Function of parameter estimates, forming the "right-hand
     sides." This is a multivariate-valued function, of dimensionality
     equal to that of \code{m}}.
  \item{init}{Vector of initial guesses for parameter estimates.  If 
     components are named, these will be used as labels in the output.}
  \item{eps}{Convergence criterion.} 
  \item{maxiters}{Maximum number of iterations.}
  \item{x}{Input data.}
}

\details{
 
   Standard Newton-Raphson methods are used to solve for the parameter
   estimates, with \code{numericDeriv} being used to find the
   approximate derivatives.
}

\value{

   R list consisting of components \code{tht}, the vector of parameter
   estimates, and \code{numiters}, the number of iterations performed. 

}

\examples{
x <- rgamma(1000,2)
m <- c(mean(x),var(x))
g <- function(x,theta) {  # from theoretical properties of gamma distr.
   g1 <-  theta[1] / theta[2]
   g2 <-  theta[1] / theta[2]^2
   c(g1,g2)
}
# should output about 2 and 1
mm(m,g,x)

\dontrun{
library(mfp)
data(bodyfat)
# model as a beta distribution 
g <- function(x,theta) {
   t1 <- theta[1]
   t2 <- theta[2]
   t12 <- t1 + t2
   meanb <- t1 / t12
   m1 <- meanb 
   m2 <- t1*t2 / (t12^2 * (t12+1)) 
   c(m1,m2)
}
x <- bodyfat$brozek/100
m <- c(mean(x),var(x))
# about 4.65 and 19.89
mm(m,g,x)
}

}

\author{
Norm Matloff
}

