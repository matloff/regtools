
# multivariate polynomials, aimed at use in polynomial regression

# in the latter context, it is assumed that in the training set and in
# future new cases, the predictors/features are the same set, in the
# same order; checks should be done to ensure that the colnames are the
# same in both cases

# no goal of efficiency just clarity

polyGen <- function(x,deg,maxInteractDeg)
{
   require(rje)
   if (is.null(colnames(x))) stop('x needs column names')

   worker <- function(p,d)  # all polys of first p vars, degree d
   {
      if (d == 1) {
         res <- powerSetMat(p)[-1]
      } else {
        # need to recurse first on p, then d
      }
   }

   attr(res,'clnames') <- colNames(x)
   res
}

