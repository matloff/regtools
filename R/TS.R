

# routines to convert time series to rectangular data

########################## TStoXBase() #####################################

# inputs a time series of length m, converts to an n x (p+1) matrix,
# where p is the lag of a quasi-autoregressive/moving average model and
# n = m - p + 1

# two cases:

#   1. straight time series, no external "Y" to be predicted (y = NULL)

#      last col in output will consist of x_{p+1},x_{p+2},...,x_n

#   2. one time series predicting another (y not NULL)

#      last col in output will consist of y_{p+1},y_{p+2},...,y_n

# arguments:
# 
#    x:  a time series of length m, possibly multivariate; 
#        either a vector or an m x p matrix; p is set to 
#        ncol(x) in the latter case, 1 otherwise
#    lg:  the code is intended to enable the fitting of a model
#         in which observations at time t will be predicted
#         from observations at times t-lg, t-lg+1,...,t-1
#    y:  if non-NULL, a time series to be predicted, vector of length m; 
#        if y is NULL, x must be a vector, and y will be taken to be x

# value:
# 
#    matrix, suitable for fitting a prediction model; m-lg rows,
#    lg*p+1 columns; y[-(1:lg)] will be in the last column

#    in the case of vector x, the "X portion" will be 
#    
#    x[1], x[2], ..., x[lg]
#    x[2], x[3], ..., x[lg+1]
#    ...
#    x[m-lg], x[m-lg+2], ..., x[m-1]
#    
#    for matix x, the first row will consist of all p observations for
#    time 1, the second row will have the ones for time 2 etc.

TStoXBase <- function(x,lag,y=NULL) 
{
   lx <- length(x)
   origlag <- lag
   lag <- lag + 1
   lxl <- lx - lag + 1
   lxl2 <- lxl + 1
   mt <- cbind(x[-(lxl2:lx)],1:lxl)
   onerow <- function(mtrow) {
      i <- mtrow[2]
      s <- i
      e <- i + lag - 1
      x[s:e]
   }
   tmp <- t(apply(mt,1,onerow))
   if (!is.null(y)) tmp[,lag] <- y[-(1:origlag)]
   tmp
}

