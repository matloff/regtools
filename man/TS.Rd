\name{TStoX}
\alias{TStoX}
\alias{TStoXmv}

\title{Transform Time Series to Rectangular Form}

\description{
Input a time series and transform it to a form suitable for prediction
using \code{lm} etc.
}

\usage{
TStoX(x,lg)
TStoXmv(xmat,lg,y)
}

\arguments{
   \item{x}{A vector.}
   \item{lg}{Lag, a positive integer.}
   \item{xmat}{A matrix, data frame etc., a multivariate time series.
       Each column is a time series, over a common time period.}
   \item{y}{A time series, again on that common time period.  If NULL in
       \code{TStoXmv}, then \code{y} is set to \code{x} (i.e. for a 
       univariate time series in which older values predict newer ones).}
}

\details{

Similar to \code{stats::embed}, but in lagged form, with applications
such as \code{lm} in mind.

\code{TStoX} is for transforming vectors, while \code{TStoXmv}
handles the multivariate time series case.  Intended for use with
\code{lm} or other regression/machine learning  model, predicting 
\code{y[i]} from observations \code{i-lg, i-lg+1,...,i-1}.
}

\value{


    As noted, the idea is to set up something like \code{lm(Y ~ X)}.
    Let \code{m} denote length of \code{x}, and in the matrix input
    case, the number of rows in \code{xmat}.  Let \code{p} be 1 in the
    vector case, \code{ncol(xmat)} in the matrix case.  The return value
    is a matrix with \code{m-lg} rows.  There will be \code{p*lg+1}
    columns, with "Y," the numbers to be predicted in the last column.

    In the output in the multivariate case, let k denote
    \code{ncol(xmat)}.  Then the first k columns of the output  will be
    the k series at lag \code{lg}, the second k columns will be the k
    series at lag \code{lg-1}, ..., and the \code{lg}-th set of k
    columns will be the k series at lag 1,

}

\examples{

x1 <- c(5,12,13,8,88,6) 
x2 <- c(5,4,3,18,168,0) 
y <- 1:6 
xmat <- cbind(x1,x2) 

TStoX(x1,2)
#      [,1] [,2] [,3]
# [1,]    5   12   13
# [2,]   12   13    8
# [3,]   13    8   88
# [4,]    8   88    6

xy <- TStoXmv(xmat,2,y)
xy
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    5    5   12    4    3
# [2,]   12    4   13    3    4
# [3,]   13    3    8   18    5
# [4,]    8   18   88  168    6

lm(xy[,5] ~ xy[,-5])
# Coefficients:
# (Intercept)    xy[, -5]1    xy[, -5]2    xy[, -5]3    xy[, -5]4
#       -65.6          3.2         18.2         -3.2           NA
# need n > 7 here for useful lm() call, but this illustrates the idea
}

\author{
Norm Matloff
}

