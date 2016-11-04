
# ridge regression, similar to MASS::lm.ridge()

# X will be scaled and centered, using scale(); est. beta vector obtained by
# solving 

#    (X'X)/n + lambda I = X'Y/n

# to make choice of lambda easier, since (X'X)/n will have 1s on diag

# arguments:

#    xy: data matrix, "Y" in last column

#    lambda: set of lambda values to try

# value: object of class 'rlm', with components

#     bhats: matrix of est reg coefs, one col for each lambda value; if
#        mapback is TRUE, these coefs will be mapped back to the
#        original predictors' scale
#     lambda: copy of the input lambda

ridgelm <- function(xy,lambda=seq(0.01,1.00,0.01),mapback=TRUE) {
   p <- ncol(xy) - 1; n <- nrow(xy)
   x <- xy[,1:p]
   y <- xy[,p+1]
   x <- scale(x); y <- y - mean(y)
   tx <- t(x)
   xpx <- tx %*% x / n
   xpy <- tx %*% y / n
   mapftn <- function(lambval) 
      qr.solve(xpx + lambval*diag(p),xpy)
   tmp <- Map(mapftn,lambda)
   tmp <- Reduce(cbind,tmp)
   if (mapback) {
      sds <- attr(x,'scaled:scale')
      for (i in 1:p) tmp[i,] <- tmp[i,] / sds[i]
   }
   result <- list(bhats=tmp,lambda=lambda)
   class(result) <- 'rlm'
   result
}

plot.rlm <- function(x,y,...) {  
   lamb <- x$lambda
   bhs <- t(x$bhats)
   matplot(lamb,bhs,type='l',pch='.',xlab='lambda',ylab='beta-hat')
}

# print.rlm <- function(x,...) print(t(x$bhats)) 

