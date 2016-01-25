
# Augmented Method of Moments

# overview:

#    motivated by linear mixed effects models, but more general
# 
#    say the parameter vector theta has length k, so we need k equations
# 
#    r of the equations will be classical Method of Moments, and k-r will
#    be regression type, i.e. the (X'X)^{-1} betahat = X'Y type; these
#    could also be linear approximations, as in glm()

# arguments:
# 
#    mm(x): vector of sample moments ("left-hand sides" of MM eqns); 
#           x is the data, one observation per row; might be more
#           general than moments
#    g(x,theta): 
#       a vector-valued function, specifying the "right-hand sides" 
#       of the MM eqns; x as above, and theta is the vector of 
#       parameters to be estimated; it is required that the second 
#       argument of g() be named 'theta'
#    data: our x in m() and g()
#    lg: length(m), i.e. number of equations
#    init: initial guess for theta; R list with names corresponding
#          to the parameters in g
#    eps: convergence criterion; iterations stop at 1000, or whe
#         sum(abs(g)) < eps
#    maxiters: max number of iterations

amm <- function(m,g,x,lg,init=rep(0.5,lg),eps=0.0001,maxiters=1000) {
   tht <- unlist(init)
   mvec <- m(data)
   for (i in 1:maxiters) {
      # current g values
      gvec <- getgvec(g,tht)
      if (max(abs(mvec - gvec)) < eps) {
         if (!is.null(names(init))) 
            names(tht) <- names(init)

         result <- list(tht=tht,numiters=i)
         return(tht)
      }
      # Jacobian
      jcb <- getjcb(g,tht)
      tht <- tht + solve(jcb,mvec-gvec)
   }
   print('max iterations exceeded')
}

getgvec <- function(g,tht) {
   theta <- tht
   g(theta)
}

getjcb <- function(g,tht) {
   theta <- tht
   attr(numericDeriv(quote(g(theta)),'theta'),'gradient')
}

# test case; should output about 2 and 1
# x <- rgamma(1000,2)
# m <- function(x) c(mean(x),var(x))
# g <- function(theta) {
#    g1 <-  theta[1] / theta[2]
#    g2 <-  theta[1] / theta[2]^2
#    c(g1,g2)
# }
# mm(m,g,x,2)

