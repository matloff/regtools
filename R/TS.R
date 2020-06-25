

# routines to convert time series to rectangular data, so that we can
# then fit using lm() or whatever, predicting from the last lg
# observations

# the first function, TStoX(x,lg,y), inputs a univariate time series and
# outputs an "X" matrix in the sense of lm(Y ~ X); here the "Y" vector
# is either supplied as an argument, or by default x

# consider for instance x = (5,12,13,8,88,6) and lg = 2, with y = x; we
# want to redict x from itself, i.e.
 
# predict the 13 from 5, 12
# predict the 8 from 12, 13
# predict the 88 from 13, 8
 
# and
 
# predict the 6 from 8, 88

# our training set would then be
# 
# X =
# 
#    5 12
#   12 13
#   13  8
#    8 88
#  
# Y = (13,8,88,6)

########################## TStoX() #####################################

# inputs a time series, and transforms to rectangular shape suitable for
# lm() or some other regression model, in which any current observation
# is predicted from the last lg ones

# arguments:
# 
#    x:  a univariate time series; m is set to length(x) below
#    lg:  lag, for fitting of a model in which observations at 
#         time t will be predicted from observations at times 
#         t-lg, t-lg+1,...,t-1
#    y:  if non-NULL, a time series to be predicted, vector of length m; 
#        if y is NULL, y will be taken to be x

# value:
# 
#    matrix, suitable for fitting a prediction model; m-lg rows,
#    lg+1 columns; y[lg+1], y[lg+2], ..., y[m] will be in the last column

#    the "X portion" will be 
#    
#    x[1], x[2], ..., x[lg]
#    x[2], x[3], ..., x[lg+1]
#    ...
#    x[m-lg], x[m-lg+1], ..., x[m-1]

TStoX <- function(x,lg,y=NULL) 
{
   lx <- length(x)
   if (lg >= lx) stop('lg must be < length(x)')
   origlg <- lg
   nColX <- lg + 1  # number of columns in the output "X"
   # the first lxl elements in x will be used in the output "X"
   lxl <- lx - lg + 1    # these elements of x will be used in "X"
   lxl2 <- lxl + 1  # these elements of x will not be used in "X"
   mt <- cbind(x[-(lxl2:lx)],
               1:lxl)
   onerow <- function(mtrow) {
      i <- mtrow[2]
      s <- i
      e <- i + lg - 1
      x[s:e]
   }
   tmp <- t(apply(mt,1,onerow))
   if (!is.null(y)) tmp[,lg] <- y[-(1:origlg)]
   tmp
}

# m-variate time series version of TStoX (but y is not optional)

# arguments:

#    each row of xmat is a time series, y is a vector (separate from x)
   
# value:

#    the first row will consist of all m observations for
#    time 1, then all m observations for time 2 etc., through time lg;
#    the second row is similar, but for times 2 through lg+1

TStoMat <- function(xmat,lg,y) {
   m <- nrow(xmat)
   rslt <- NULL
   for (i in 1:m) {
      tmp <- TStoX(xmat[i,],lg,y)[,1:lg]
      rslt <- cbind(rslt,tmp)
   }
   cbind(rslt,y[-(1:lg)])
}

