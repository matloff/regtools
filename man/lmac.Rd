\name{lmac,makeNA,coef.lmac,vcov.lmac,pcac}
\alias{lmac}
\alias{pcac}
\alias{coef.lmac}
\alias{vcov.lmac}
\alias{makeNA}

\title{Available Cases Method for Missing Data}

\description{
Various estimators that handle data via the Available Cases Method
}

\usage{
lmac(xy,nboot=0) 
makeNA(m,probna)
coef.lmac(lmacout)
vcov(lmacout)
pcac(indata,scale=FALSE) 
}

\arguments{
  \item{xy}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{indata}{Matrix or data frame.}
  \item{nboot}{If positive, number of bootstrap samples to take.}
  \item{probna}{Probability that an element will be NA.}
  \item{lmacout}{An object of class \code{'lmac'}.}
  \item{scale}{If TRUE, call \code{cor} instead of \code{cov}.}
}

\details{

   The Available Cases (AC) approach applies to statistical methods that
   depend only on products of k of the variables, so that cases having
   non-NA values for those k variables can be used, as opposed to using
   only cases that are fully intact in all variables, the Complete Cases
   (CC) approach.  In the case of linear regression, for instance, the
   estimated coefficients depend only on covariances between the
   variables (both predictors and response).  This approach assumes thst
   the cases with missing values have the same distribution as the
   intact cases.

   The \code{lmac} function forms OLS estimates as with \code{lm}, but
   applying AC, in contrast to \code{lm}, which uses the CC method.

   The \code{pcac} function is an AC substitute for \code{prcomp}. The
   data is centered, corresponding to a fixed value of TRUE in
   \code{prcomp}.  It is also scaled if \code{scale} is TRUE,
   corresponding \code{scale. = TRUE} in \code{prcomp;.} Due to AC,
   there is a small chance of negative eigenvalues, in which case
   \code{stop} will be called.

   The \code{makeNA} function is used to insert random NA values into
   data, for testing purposes.
   
}

\value{

For \code{lmac}, an object of class \code{'lmac'}, with components

\itemize{

   \item{coefficients}, as with \code{lm}; 
   accessible directly or by calling \code{coef}, as with \code{lm}

   \item{fitted.values}, as with \code{lm}

   \item{residuals}, as with \code{lm}

   \item{r2}, (unadjusted) R-squared

   \item{cov}, for \code{nboot >0} the estimated covariance matrix
   of the vector of estimated regression coefficients; accessible
   directly or by calling \code{coef}, as with \code{lm} 

}

For \code{pcac}, an R list, with components

\itemize{

   \item{sdev}, as with \code{prcomp}

   \item{rotation}, as with \code{prcomp}

}

}

\author{
Norm Matloff
}

