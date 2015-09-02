\name{lmac,makeNA,coef.lmac,vcov.lmac}
\alias{lmac}
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
}

\arguments{
  \item{xy}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{nboot}{If positive, number of bootstrap samples to take.}
  \item{probna}{Probability that an element will be NA.}
  \item{lmacout}{An object of class \code{'lmac'}.}
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

   The \code{makeNA} function is used to insert random NA values into
   data, for testing purposes.
   
}

\value{

For \code{lmac}, an object of class \code{'lmac'}, with components

\itemize{

   \item \code{coefficients}, as with \code{lm}; 
   accessible directly or by calling \code{coef}, as with \code{lm}

   \item \code{fitted.values}, as with \code{lm}

   \item \code{residuals}, as with \code{lm}

   \item \code{r2}, (unadjusted) R-squared

   \item \code{cov}, for \code{nboot >0} the estimated covariance matrix
   of the vector of estimated regression coefficients; accessible
   directly or by calling \code{coef}, as with \code{lm} }
}

\examples{
data(prgeng)
pe <- prgeng
# dummies for MS, PhD
pe$ms <- as.integer(pe$educ == 14)
pe$phd <- as.integer(pe$educ == 16)
# computer occupations only
pecs <- pe[pe$occ >= 100 & pe$occ <= 109,]
pecs1 <- pecs[,c(1,7,9,12,13,8)]
pecs1 <- as.matrix(pecs1)
pecs2 <- makeNA(pecs1,0.10)
coef(lmac(pecs2))
}

\author{
Norm Matloff
}

