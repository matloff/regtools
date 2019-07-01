

# routines to convert time series to rectangular data

########################## TStoXBase() #####################################

# inputs a time series of length m, converts to an n X (p+1) matrix, where p
# is the lag of a quasi-autoregressive/moving average model and 
# n = m - p + 1

# two cases:

#   1. straight time series, no external "Y" to be predicted (y = NULL)

#      last col in output will consist of x_{p+1},x_{p+2},...,x_n

#   2. one time series predicting another (y not NULL)

#      last col in output will consist of y_{p+1},y_{p+2},...,y_n

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

########################## TStoXMult() #####################################

# adds ability to predict multiple time series

