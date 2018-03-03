
# undoes 'scale()'

# reverses scaling on scaledx, dividing its columns by sds and adding
# ctrs; if either of the latter 2 is NULL, it is obtained via attr(), on
# the assumption that scaledx was produced from x by scale() or similar

# returns the original x; if scaledx 

unscale <- function(scaledx,ctrs=NULL,sds=NULL) {
   if (is.null(ctrs)) ctrs <- attr(scaledx,'scaled:center')
   if (is.null(sds)) sds <- attr(scaledx,'scaled:scale')
   origx <- scaledx
   for (j in 1:ncol(scaledx)) {
      origx[,j] <- origx[,j] * sds[j]
      origx[,j] <- origx[,j] + ctrs[j]
   }
   origx
}

# convenience function to load and prep the 'prgeng' data

getPE <- function()
{
   data(prgeng)
   # dummies for MS, PhD
   pe <- prgeng
   pe$ms <- as.integer(pe$educ == 14)
   pe$phd <- as.integer(pe$educ == 16)
   pe$educ <- NULL
   pe$engl <- NULL
   pe$birth <- NULL
   pe$powspuma <- NULL
   pe$yrentry <- ifelse(pe$yrentry == 0,round(2000-pe$age),pe$yrentry)
   # dummies for the rest
   require(dummies)
   citstatus <- dummy(pe$cit)[,1:4]
   colnames(citstatus) <- c('cit1','cit2','cit3','cit4')
   pe <- cbind(pe,citstatus)
   pe$cit <- NULL
   occcode <- dummy(pe$occ)[,1:5]
   colnames(occcode) <- c('occ1','occ2','occ3','occ4','occ5')
   pe <- cbind(pe,occcode)
   pe$occ <- NULL
   pe$sex <- 2 - pe$sex
   pe <<- pe
}

