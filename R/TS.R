

# routines to convert time series to rectangular data, so that we can
# then fit using lm() or whatever, predicting from the last lg
# observations

########################## TStoX() #####################################

# inputs a time series, and transforms rectangular shape suitable for
# lm() or some other regression model, in which any current observation
# is predicted from the last lg ones

# arguments:
# 
#    x:  a time series of length; m is set to length(x)
#    lg:  the code is intended to enable the fitting of a model
#         in which observations at time t will be predicted
#         from observations at times t-lg, t-lg+1,...,t-1
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
#    
#    for matrix x, the first row will consist of all p observations for
#    time 1, then all p observations for time 2 etc., through time lg;
#    the second row is similar, but for times 2 through lg+1

TStoX <- function(x,lg,y=NULL) 
{
   lx <- length(x)
   if (lg >= lx) stop('lg must be < length(x)')
   origlg <- lg
   lg <- lg + 1
   lxl <- lx - lg + 1
   lxl2 <- lxl + 1
   mt <- cbind(x[-(lxl2:lx)],1:lxl)
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

# multivariate time series version
# each row of xmat is a time series, y is a vector

TStoMat <- function(xmat,lg,y) {
   m <- nrow(xmat)
   rslt <- NULL
   for (i in 1:m) {
      tmp <- TStoX(xmat[i,],lg,y)[,1:lg]
      rslt <- cbind(rslt,tmp)
   }
   cbind(rslt,y[-(1:lg)])
}

