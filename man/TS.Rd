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
   \item{xmat}{A matrix, data frame etc., a multivariate time series.
       Each row is a time series, over a common time period.}
   \item{y}{A time series, again on that common time period.  If NULL, 
       it is set to \code{x} (i.e. for a univariate time series in which
       older values predict newer ones).}
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
    columns, with "Y," the numbers to be predicted in the last column,
    consisting of \code{y[lg+1], y[lg+2],...,y[m]}.  The first \code{p}
    columns will play the role of "X," the matrix of predictors.  The
    idea is to set up something like \code{lm(Y ~ X)}.

    In the matrix case, in a given row, there will be all \code{lg}
    recent observations for the first time series, then all \code{lg}
    recent observations for the second one, and so on, and finally the
    \code{y} value.

}

\examples{
set.seed(9999)
z <- sample(1:100,12)
z
# [1] 87 66 79 21 67 81 97 77 92 68 74  3
TStoX(z,3)
#      [,1] [,2] [,3] [,4]
# [1,]   87   66   79   21
# [2,]   66   79   21   67
# [3,]   79   21   67   81
# [4,]   21   67   81   97
# [5,]   67   81   97   77
# [6,]   81   97   77   92
# [7,]   97   77   92   68
# [8,]   77   92   68   74
# [9,]   92   68   74    3
set.seed(9999)
zm <- matrix(sample(1:100,24),nrow=2,byrow=TRUE)
y <- sample(1:5,12,replace=TRUE)
zm
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   96    6   41    3   74   66    8   31    9    36    25    83
# [2,]   10   38   14   19   43   20   40   63   29     1    75     4
y
#  [1] 5 3 3 4 2 5 2 5 3 3 5 4
xy <- TStoMat(zm,5,y)
xy
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
# [1,]   96    6   41    3   74   10   38   14   19    43     5
# [2,]    6   41    3   74   66   38   14   19   43    20     2
# [3,]   41    3   74   66    8   14   19   43   20    40     5
# [4,]    3   74   66    8   31   19   43   20   40    63     3
# [5,]   74   66    8   31    9   43   20   40   63    29     3
# [6,]   66    8   31    9   36   20   40   63   29     1     5
# [7,]    8   31    9   36   25   40   63   29    1    75     4

lm(xy[,11] ~ xy[,1:10])  # toy example, need larger m
}

\author{
Norm Matloff
}

