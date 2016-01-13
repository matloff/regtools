
# Method of Moments

# arguments:
# 
#    g(x,theta): 
#       a vector-valued function, specifying the "left-hand
#       sides" of the MM eqns (the right-hand sides will be 0s);
#       x is the data, one observation per row, and theta is the
#       vector of parameters to be estimated; it is required that the
#       arguments of g() are 'x' and 'theta
#    data: our data, x in g()
#    lg: length(g), i.e. number of equations
#    eps: convergence criterion; iterations stop at 1000, or whe
#         sum(abs(g)) < eps
#    init: initial guess for theta; R list with names corresponding
#          to the parameters in g
#    maxiters: max number of iterations

mm <- function(g,data,lg,init,eps=0.0001,maxiters=1000) {
   tht <- unlist(init)
   for (i in 1:maxiters) {
      # current g values
      gvec <- getgvec(g,data,tht)
      if (max(abs(gvec)) < eps) {
         if (!is.null(names(init)) 
            names(tht) <- names(init)
         return(tht)
      }
      # Jacobian
      jcb <- getjcb(g,data,lg,tht)
      tht <- tht - solve(t(jcb),gvec)
   }
   print('max iterations exceeded')
}

getgvec <- function(g,data,tht) {
   x <- data
   theta <- tht
   g(x,theta)
}

getjcb <- function(g,data,lg,tht) {
   x <- data
   theta <- tht
   attr(numericDeriv(quote(g(x,theta)),'theta'),'gradient')
}


# > x <- rgamma(1000,2)
# > g
# function(x,theta) {
#    m1 <- mean(x) - theta[1] / theta[2]
#    m2 <- var(x) - theta[1] / theta[2]^2
#    c(m1,m2)
# }
# > mm(g,x,2,c(0.5,0.5))
# [1] 1.948537 1.006271

