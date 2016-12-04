
# undoes 'scale()'

# scaledx is the result of calling 'scale()' on a matrix x; 'unscale()'
# returns the original x

unscale <- function(scaledx) {
   ctrs <- attr(scaledx,'scaled:center')
   sds <- attr(scaledx,'scaled:scale')
   origx <- scaledx
   for (j in 1:ncol(scaledx)) {
      origx[,j] <- origx[,j] * sds[j]
      origx[,j] <- origx[,j] + ctrs[j]
   }
   origx
}

