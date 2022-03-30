\name{lmac,makeNA,coef.lmac,vcov.lmac,pcac,loglinac,tbltofakedf}
\alias{lmac}
\alias{pcac}
\alias{coef.lmac}
\alias{vcov.lmac}
\alias{loglinac}
\alias{tbltofakedf}
\alias{makeNA}
\alias{NAsTo0s}
\alias{ZerosToNAs}

\title{Available Cases Method for Missing Data}

\description{
Various estimators that handle missing data via the Available Cases Method
}

\usage{
lmac(xy,nboot=0) 
makeNA(m,probna)
NAsTo0s(x)
ZerosToNAs(x,replaceVal=0)
\method{coef}{lmac}(object,...)
\method{vcov}{lmac}(object,...)
pcac(indata,scale=FALSE) 
loglinac(x,margin) 
tbltofakedf(tbl)
}

\arguments{
  \item{replaceVal}{Value to be replaced by NA.}
  \item{xy}{Matrix or data frame, X values in the first columns, Y
     in the last column.}
  \item{indata}{Matrix or data frame.}
  \item{x}{Matrix or data frame, one column per variable.}
  \item{nboot}{If positive, number of bootstrap samples to take.}
  \item{probna}{Probability that an element will be NA.}
  \item{scale}{If TRUE, call \code{cor} instead of \code{cov}.}
  \item{tbl}{An R table.}
  \item{m}{Number of synthetic NAs to insert.}
  \item{object}{Output from \code{lmac}.}
  \item{...}{Needed for consistency with generic function.  Not used.}
  \item{margin}{A list of vectors specifying the model, as in
     \code{loglin}.}

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
   data is centered, corresponding to a fixed value of \code{center =
   TRUE} in \code{prcomp}.  It is also scaled if \code{scale} is TRUE,
   corresponding \code{scale = TRUE} in \code{prcomp}. Due to AC,
   there is a small chance of negative eigenvalues, in which case
   \code{stop} will be called.

   The \code{loglinac} function is an AC substitute for \code{loglin}.
   The latter takes tables as input, but \code{loglinac} takes the raw
   data. If you have just the table, use \code{tbltofakedf} to
   regenerate a usable data frame.

   The \code{makeNA} function is used to insert random NA values into
   data, for testing purposes.
   
}

\value{

   For \code{lmac}, an object of class \code{lmac}, with components
   
   \itemize{
   
      \item{coefficients}, as with \code{lm}; 
      accessible directly or by calling \code{coef}, as with \code{lm}
   
      \item{fitted.values}, as with \code{lm}
   
      \item{residuals}, as with \code{lm}
   
      \item{r2}, (unadjusted) R-squared
   
      \item{cov}, for \code{nboot > 0} the estimated covariance matrix
      of the vector of estimated regression coefficients; accessible
      directly or by calling \code{vcov}, as with \code{lm} 
   
   }
   
   For \code{pcac}, an R list, with components
   
   \itemize{
   
      \item{sdev}, as with \code{prcomp}
   
      \item{rotation}, as with \code{prcomp}
   
   }
   
   For \code{loglinac}, an R list, with components
   
   \itemize{
   
      \item{param}, estimated coefficients, as in \code{loglin}
   
      \item{fit}, estimated expected call counts, as in \code{loglin}
   
   }

}

\examples{
n <- 25000
w <- matrix(rnorm(2*n),ncol=2)  # x and epsilon
x <- w[,1]
y <- x + w[,2]
# insert some missing values
nmiss <- round(0.1*n)
x[sample(1:n,nmiss)] <- NA
nmiss <- round(0.2*n)
y[sample(1:n,nmiss)] <- NA
acout <- lmac(cbind(x,y))
coef(acout)  # should be near pop. values 0 and 1
}

\author{
Norm Matloff
}

