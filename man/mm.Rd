\name{mm}
\alias{mm}

\title{Method of Moments, Including Regression Terms}

\description{
Method of Moments computation, capable of handling models that include
parametric regression terms.
}

\usage{
mm(m,g,x,init=rep(0.5,length(m)),eps=0.0001,maxiters=1000) 
}

\arguments{
  \item{m}{Vector of sample moments, "left-hand sides" of moment
     equations.}
  \item{g}{Function of parameter estimates, forming the "right-hand
     sides."}
  \item{init}{Vector of initial guesses for parameter estimates.  If 
     components are named, these will be used as labels in the output.}
  \item{eps}{Convergence criterion.} 
  \item{maxiters}{Maximum number of iterations.}
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
# should output about 2 and 1
x <- rgamma(1000,2)
m <- c(mean(x),var(x))
g <- function(x,theta) {
   g1 <-  theta[1] / theta[2]
   g2 <-  theta[1] / theta[2]^2
   c(g1,g2)
}
mm(m,g,x)

\dontrun{
library(mfp)
data(bodyfat)
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
mm(m,g,x)
}

}

\author{
Norm Matloff
}

