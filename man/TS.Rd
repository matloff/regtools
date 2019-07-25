\name{TStoX}
\alias{TStoX}
\alias{TStoMat}

\title{Transform Time Series to Rectangular Form}

\description{
Inputs a time series and transforms it to a form suitable for prediction
using \code{lm} etc.
}

\usage{
TStoX(x,lg,y=NULL)
TStoMat(xmat,lg,y)
}

\arguments{
   \item{x}{A vector.}
   \item{lg}{Lag, a positive integer.}
   \item{xmat}{A matrix, data frame etc.}
   \item{y}{A vector.}
}

\details{
    \code{TStoX} is for transforming vectors, while \code{TStoMat}
    handles the multivariate time series case.
}

\value{

    Matrix.  In the univariate case, there will   

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

