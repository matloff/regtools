
# uses output of R's nls() to get an asymptotic covariance 
# matrix in general heteroscedastic case

# arguments:
# 
#    nlsout: object of type 'nls'
# 
# value: approximate covariance matrix for the 
#        estimated parameter vector

nlshc <- function(nlsout,type='HC') {
   # notation: g(t,b) is the regression model, 
   # where t is the vector of variables for a 
   # given observation; b is the estimated parameter
   # vector; x is the matrix of predictor values
   b <- coef(nlsout)
   m <- nlsout$m
   # y - g:
   resid <- m$resid()
   # row i of hmat will be deriv of g(x[i,],b) 
   # with respect to b
   hmat <- m$gradient()
   # calculate the artificial "x" and "y" of 
   # the algorithm
   xhm <- hmat
   yresidhm <- resid + hmat %*% b
   # -1 means no constant term in the model
   lmout <- lm(yresidhm ~ xhm - 1)
   # vcovHC(lmout); was getting NAs for some data sets
   sandwich::vcovHC(lmout,type)
}
