
# multivariate polynomials, aimed at use in polynomial regression

# in the latter context, it is assumed that in the training set and in
# future new cases, the predictors/features are the same set, in the
# same order; checks should be done to ensure that the colnames are the
# same in both cases

# no goal of efficiency just clarity

# arguments:

#    x: data matrix, each row a data point
#    deg: ultimate degree of the polynomial version of x
#    maxInteractDeg: maximum degree of interaction terms

# value:  polynomial version of x, along with an R attribute giving the
# original column names of x

polyGen <- function(x,deg,maxInteractDeg)
{
   require(rje)
   if (is.null(colnames(x))) stop('x needs column names')

   # vehicle for the recursion
   worker <- function(p,d)  # all polys of first p vars, degree d
   {
      if (d == 1) {
         res <- powerSetMat(p)
      } else if (p == 1) {
         res <- rep(1,d+1)
         for (i in 1:d) res[i+1] <- x^i
      } else {  
        # both p,d > 1
        # will recurse first on p, then d; the first will change the
        # matrix from p-1 to p columns and new rows, while the 
        # second will only add more rows
        res <- NULL
        tmp <- worker(p-1,d-1)
        # for each row in tmp, generate the new rows including this new
        # feature
        for (i in 1:nrow(tmp)) {
           rw <- tmp[i,]
           s <- sum(rw)
           for (j in 0:(d-1-s)) 
              res <- rbind(res,c(tmp,j))
        }
      }
   }

   attr(res,'clnames') <- colNames(x)
   res
}

