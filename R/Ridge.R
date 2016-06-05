
# ridge regression, similar to MASS::lm.ridge()

# X will be scaled and centered, using scale(); est. beta vector obtained by
# solving 

#    (X'X)/n + lambda I = X'Y/n

# to make choice of lambda easier, since (X'X)/n will have 1s on diag

# arguments:

#    xy: data matrix, "Y" in last column

#    lambda: set of lambda values to try

# value: object of class 'rlm', with components

#     bhats: matrix of est reg coefs, one col for each lambda value
#     lambda: copy of the input lambda

ridgelm <- function(xy,lambda=seq(0.01,1.00,0.01)) {
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
   result <- list(bhats=tmp,lambda=lambda)
   class(result) <- 'rlm'
   result
}

plot.rlm <- function(ridgelm.out) {
   lamb <- ridgelm.out$lambda
   bhs <- t(ridgelm.out$bhats)
   matplot(lamb,bhs,type='l',pch='.',xlab='lambda',ylab='beta-hat')
}

print.rlm <- function(ridgelm.out) print(t(ridgelm.out$bhats)) 

