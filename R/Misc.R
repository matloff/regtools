
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

# convenience function to load and prep the 'prgeng' data; creates 
# global data frame pe; if Dummies, then factors are changed to 
# dummy variables
getPE <- function(Dummies=TRUE)
{
   data(prgeng)
   pe <- prgeng
   pe$sex <- 2 - pe$sex
   if (!Dummies) {
      pe <<- pe
      return()
   }
   # dummies for MS, PhD etc.
   pe$ms <- as.integer(pe$educ == 14)
   pe$phd <- as.integer(pe$educ == 16)
   pe$educ <- NULL
   pe$engl <- NULL
   pe$birth <- NULL
   pe$powspuma <- NULL
   pe$yrentry <- ifelse(pe$yrentry == 0,round(2000-pe$age),pe$yrentry)
   require(dummies)
   citstatus <- dummy(pe$cit)[,1:4]
   colnames(citstatus) <- c('cit1','cit2','cit3','cit4')
   pe <- cbind(pe,citstatus)
   pe$cit <- NULL
   occcode <- dummy(pe$occ)[,1:5]
   colnames(occcode) <- c('occ1','occ2','occ3','occ4','occ5')
   pe <- cbind(pe,occcode)
   pe$occ <- NULL
   pe <<- pe
}

# make a matrix or data frame (fmt = ''' or 'd') of 
# dummies from a factor f; col names will be 'dms' concatenated
# with the factor levels
factorToDummies <- function(f,fmt='m')
{
   n <- length(f)
   fl <- levels(f)
   ndumms <- length(fl) - 1
   dms <- if(fmt=='m') {
      matrix(nrow=n,ncol=ndumms) 
   }
   else 
      data.frame(nrow=n,ncol=ndumms)
   for (i in 1:ndumms) 
      dms[,i] <- as.integer(f == fl[i])
   colnames(dms) <- paste('dms',fl[-(length(fl))],sep='')
   dms
}

# for each column in df, if factor then replace by dummies, all but last
# class, else just copy column

factorsToDummies <- function(df) 
{
   require(dummies)
   outDF <- data.frame(rep(0,nrow((df))))  # filler start
   for (i in 1:ncol(df)) {
      dfi <- df[,i]
      if (!is.factor(dfi)) {
         outDF <- cbind(outDF,dfi) 
         names(outDF)[ncol(outDF)] <- names(df)[i]
      } else {
         dumms <- factorToDummies(dfi,names(df)[i])
         outDF <- cbind(outDF,dumms)
      }
   }
   outDF[,1] <- NULL  # delete filler
   outDF
}

factorToDummies <- function (f,fname) 
{
    n <- length(f)
    fl <- levels(f)
    ndumms <- length(fl) - 1
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == fl[i])
    colnames(dms) <- paste(fname,'.', fl[-(length(fl))], sep = "")
    dms
}


