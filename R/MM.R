
# Method of Moments, including regression terms

# overview:

#    motivated by linear mixed effects models, but more general
# 
#    say the parameter vector theta has length k, so we need k
#    equations; their left-hand sides are specified by the argument g,
#    while the right-hand sides are given by mm; the user integrates
#    regression terms into these two function arguments

# arguments:
# 
#    m: a vector of sample moments ("left-hand sides" of MM eqns); 
#       x is the data, one observation per row; might be more
#       general than moments
#    g(x,theta): 
#       a vector-valued function, specifying the "right-hand sides" 
#       of the MM eqns; x as above, and theta is the vector of 
#       parameters to be estimated; it is required that the second 
#       argument of g() be named 'theta'
#    x: our x in m and g()
#    init: initial guess for theta; R list with names corresponding
#          to the parameters in g
#    eps: convergence criterion; iterations stop at 1000, or whe
#         sum(abs(g)) < eps
#    maxiters: max number of iterations

mm <- function(m,g,x,init=rep(0.5,length(m)),eps=0.0001,maxiters=1000) {
   tht <- init
   # mvec <- m(data)
   mvec <- m
   for (i in 1:maxiters) {
      # g values for current iteration
      # gvec <- getgvec(g,tht)
      # browser()
      gvec <- g(x,tht)
      if (max(abs(mvec - gvec)) < eps) {
         if (!is.null(names(init))) 
            names(tht) <- names(init)
         result <- list(tht=tht,numiters=i)
         return(tht)
      }
      # not done, so get new Jacobian and update tht
      jcb <- getjcb(g,x,tht)
      tht <- tht + solve(jcb,mvec-gvec)
   }
   print('max iterations exceeded')
}

# getgvec <- function(g,tht) {
#    theta <- tht
#    g(theta)
# }

getjcb <- function(g,x,tht) {
   theta <- tht
   attr(numericDeriv(quote(g(x,theta)),'theta'),'gradient')
}

# test case; should output about 2 and 1
# x <- rgamma(1000,2)
# m <- c(mean(x),var(x)
# g <- function(theta) {
#    g1 <-  theta[1] / theta[2]
#    g2 <-  theta[1] / theta[2]^2
#    c(g1,g2)
# }
# mm(m,g,x)

