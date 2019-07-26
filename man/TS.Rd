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
   \item{xmat}{A matrix, data frame etc., with each row a time series.}
   \item{y}{A vector.  If NULL, it is set to \code{x}}.
}

\details{
    \code{TStoX} is for transforming vectors, while \code{TStoMat}
    handles the multivariate time series case.  Intended for use with
    \code{lm} or other regression model, predicting \code{y[i]} from
    observations \code{i-lg, i-lg+1,...,i-1}.
}

\value{

    Let \code{m} denote length of \code{x}, and in the matrix input
    case, the number of rows in \code{xmat}.  Let \code{p} be 1 in the
    vector case, \code{ncol(xmat)} in the matrix case.  The return value
    is a matrix with \code{m-lg} rows.  There will be \code{p*lg+1}
    columns, with "Y," the numbers to be predicted in the last column.
    \code{y[lg+1], y[lg+2],...,y[m]}.

    In the matrix case, in a given row, there will be all \code{lg}
    recent observations for the first time series, then all \code{lg}
    recent observations for the second one, and so on, and finally the
    \code{} value.

}

\examples{
z <- sample(1:100,12)
z
# [1] 59 56  6 95 16 94 96 78 17  7 35 87
TStoX(z,3)
      [,1] [,2] [,3] [,4]
#  [1,]   59   56    6   95
#  [2,]   56    6   95   16
#  [3,]    6   95   16   94
#  [4,]   95   16   94   96
#  [5,]   16   94   96   78
#  [6,]   94   96   78   17
#  [7,]   96   78   17    7
#  [8,]   78   17    7   35
#  [9,]   17    7   35   87

zm <- matrix(sample(1:100,24),nrow=2,byrow=TRUE)
zm
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   46   74   88   70   43   21   20   59   91    63     6    40
# [2,]   67   56   47   94   90   14   80   22   15    64    39    79
TStoMat(zm,3,zm[1,])
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#  [1,]   46   74   88   67   56   47   70
#  [2,]   74   88   70   56   47   94   43
#  [3,]   88   70   43   47   94   90   21
#  [4,]   70   43   21   94   90   14   20
#  [5,]   43   21   20   90   14   80   59
#  [6,]   21   20   59   14   80   22   91
#  [7,]   20   59   91   80   22   15   63
#  [8,]   59   91   63   22   15   64    6
#  [9,]   91   63    6   15   64   39   40

xy <- TStoX(Nile,5)
lm(xy[,5] ~ xy[,1:4])
}

\author{
Norm Matloff
}

