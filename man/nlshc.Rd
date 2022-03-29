\name{nlshc}
\alias{nlshc}

\title{Heteroscedastic Nonlinear Regression}

\description{
Extension of \code{nls} to the heteroscedastic case.
}

\usage{
nlshc(nlsout,type='HC')
}

\arguments{
  \item{nlsout}{Object of type 'nls'.}
  \item{type}{Eickert-White algorithm to use.  See documentation for
  \pkg{nls}.}
}

\details{
   Calls \code{nls} but then forms a different estimated covariance 
   matrix for the estimated regression coefficients, applying the
   Eickert-White technique to handle heteroscedasticity.  This then
   gives valid statistical inference in that setting.

   Some users may prefer to use \code{nlsLM} of the package
   \pkg{minpack.lm} instead of \code{nls}. This is fine, as both
   functions return objects of class 'nls'.
}

\value{
Estimated covariance matrix
}

\examples{
# simulate data from a setting in which mean Y is 
# 1 / (b1 * X1 + b2 * X2)
n <- 250
b <- 1:2
x <- matrix(rexp(2*n),ncol=2)
meany <- 1 / (x \%*\% b)  # reg ftn
y <- meany + (runif(n) - 0.5) * meany  # heterosced epsilon
xy <- cbind(x,y)
xy <- data.frame(xy)
# see nls() docs
nlout <- nls(X3 ~ 1 / (b1*X1+b2*X2),
   data=xy,start=list(b1 = 1,b2=1))
nlshc(nlout)
}

\references{ Zeileis A (2006), Object-Oriented Computation of Sandwich
Estimators.  \emph{Journal of Statistical Software}, \bold{16}(9),
1--16, \url{https://www.jstatsoft.org/v16/i09/}.  }

\author{
Norm Matloff
}

