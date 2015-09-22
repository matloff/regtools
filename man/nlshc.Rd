\name{nlshc}
\alias{nlshc}

\title{Heteroscedastic Nonlinear Regression}

\description{
Extension of \code{nls} to the heteroscedastic case.
}

\usage{
nlshc(nlsout)
}

\arguments{
  \item{nlsout}{Object of type 'nls'.}
}

\details{
   Applies Eickert-White technique to handle heteroscedasticity,
   so that one gets valid statistical inference in that setting.
}

\value{
Estimated covariance matrix for the given nonlinear regression problem.
}

\examples{
# simulate data from a setting in which mean Y is 
# 1 / (b1 * X1 + b2 * X2)
n <- 250
b <- 1:2
x <- matrix(rexp(2*n),ncol=2)
meany <- 1 / (x \%*\% b)
y <- meany + (runif(n) - 0.5) * meany
xy <- cbind(x,y)
xy <- data.frame(xy)
nlout <- nls(X3 ~ 1 / (b1*X1+b2*X2),
   data=xy,start=list(b1 = 1,b2=1))
# row 1 should be 0.01208942,-0.01226646
nlshc(nlout)
}

\author{
Norm Matloff
}

