
# grid search for good values of tuning parameters

# arguments:

#   dataset: data frame or equivalent, containing the "X" and "Y"
#      columns; will be split by the function to 'dtrn' and 'dtst',
#      located on same level as the call
#   pars: named R list, one component for each grid parameter; component 
#      'x' is the set of desired values for the parameter 'x'
#   regCall: call for given regression/classification method, together with 
#      the associated prediction function and loss evaluation; the 
#      elements of names(pars) will appear; args are
#      'dtrn', 'dtst' and 'comb', the latter being a given 
#      combination of parameter values; see example below
#   nCombs: number of possible combinations of 'pars'to evaluate; NULL
#      means all, otherwise randomly chosen
#   nTst: desired size of holdout set
#   nXval: number of cross-validation runs to perform
#   up: if TRUE, results table will be printed in increasing order of 'smoothed'
#   k: k-NN smoothing parameter for the results; if NULL, then no #   smoothing
#   dispOrderSmoothed: if TRUE and k is non-null, then output will be
#      arranged in order of the 'smoothed' column; otherwise in order of
#      the meanAcc column
#   ...: application-specific values needed by regCall() but constant
#      across combinations

# value:

#   data frame, one column for each element of 'pars', followed by a
#   meanAcc ("mean accuracy") column; then columns for standard errors,
#   Bonferroni CI radii and (if k is non-NULL) smoothed versions of
#   meanAcc

fineTuning <- function(dataset,pars,regCall,nCombs=NULL,nTst=500,nXval=1,
   up=TRUE,k=NULL,dispOrderSmoothed=FALSE,...) 
{
   # holding off for now on smoothing
   if (!is.null(k) && length(pars) > 1) 
      stop('smoothing is currently recommended only for 1 parameter')
   # generate the basic output data frame
   outdf <- expand.grid(pars)
   if (!is.null(nCombs)) {
      idxsToKeep <- sample(1:nrow(outdf),nCombs)
      outdf <- outdf[idxsToKeep,]
   } else nCombs <- nrow(outdf)
   meanAcc <- rep(NA,nCombs)
   seAcc <- rep(NA,nCombs)
   losses <- vector(length=nXval)
   for (combI in 1:nCombs) {
      for (xv in 1:nXval) {
         tmp <- partTrnTst(dataset,nTest=nTst)
         dtrn <- tmp$trn
         dtst <- tmp$tst
         cmbi <- outdf[combI,,drop=FALSE]
         loss <- try(regCall(dtrn,dtst,cmbi))
         if (inherits(loss,'try-error')) {
            cmb1 <- cmbi[1,]
            cat('error in comb ')
            cat(unlist(cmb1),'\n')
            stop()
         } else losses[xv] <- loss
      }
      meanAcc[combI] <- mean(losses)
      seAcc[combI] <- sd(losses) / sqrt(nXval)
   }
   outdf$meanAcc <- meanAcc
   outdf$seAcc <- seAcc 
   zval <- -qnorm(0.025/nrow(outdf))
   outdf$bonfAcc <- zval * seAcc
   outdf <- outdf[order(meanAcc,decreasing=!up),]
   if (!is.null(k)) {
      if (k > nrow(outdf)) {
         warning('reducing k')
         k <- nrow(outdf)
      }
      x <- outdf[,1:length(pars)]
      x <- mmscale(x)
      kout <- kNN(x,meanAcc,x,k,scaleX=FALSE,smoothingFtn=loclin)
      smoothed <- kout$regests
      outdf$smoothed <- smoothed
      if (dispOrderSmoothed) outdf <- outdf[order(smoothed),]
   } 
   row.names(outdf) <- NULL
   output <- list(outdf=outdf,nTst=nTst,nXval=nXval,k=k,
      up=up,dispOrderSmoothed=dispOrderSmoothed)
   class(output) <- 'tuner'
   output
}

# argVec is a row from the grid data frame in fineTuning(), with
# parameter names; converts the values in argVec to variables of those
# names
getNamedArgs <- function(argVec) 
{
   for (nm in names(argVec)) {
      assign(nm,argVec[[nm]],envir=parent.frame())
   }
}

# parallel coordinates plot to visualize the grid; tunerObject is output
# of fineTuning(); disp is number to display, with 0, -m and +m meaning
# cases with the m smallest 'smoothed' value, all cases and the m
# largest values of 'smoothed', respectively
plot.tuner <- function(tunerObject,col='meanAcc',disp=0) {
   require(cdparcoord)
   outdf <- tunerObject$outdf
   outdf$seAcc <- NULL
   outdf$bonfAcc <- NULL
   if (col == 'smoothed') outdf$meanAcc <- NULL
   else outdf$smoothed <- NULL
   if (disp != 0) {
      nc <- ncol(outdf)
      if (abs(disp) < nc - 1) stop('disp too small')
      ord <- order(outdf[,nc],decreasing=(disp > 0))
      outdf <- outdf[ord[1:abs(disp)],]
   }
   nr <- nrow(outdf)
   discparcoord(outdf,k=nr,differentiate=TRUE)
}

# change the display order, between meanAcc and smoothed; ftout is
# output of fineTuning(); returns the full 'tuner' object, updated
reorder.tuner <- function(ftout) {
   if (is.null(ftout$outdf$smoothed)) stop('no smoothing column')
   dispOrderSmoothed <- ftout$dispOrderSmoothed
   up <- ftout$up
   outdf <- ftout$outdf
   if (dispOrderSmoothed) {
      outdf <- outdf[order(outdf$meanAcc,decreasing=!up),]
   } else {
      outdf <- outdf[order(outdf$smoothed,decreasing=!up),]
   }
   ftout$outdf <- outdf
   ftout$dispOrderSmoothed <- !ftout$dispOrderSmoothed
   ftout
}

# partition into training, test sets; if desire training, validation,
# test, use twice

# arguments:

#    fullData:  matrix or data frame, one data point per row
#    nTest:  number of data points for the test set

# value:

#    R list, consisting of the training and test sets

partTrnTst <- function(fullData,nTest=min(1000,round(0.2*nrow(fullData)))) {
   idxs <- sample(1:nrow(fullData),nTest)
   trn <- fullData[-idxs,]
   tst <- fullData[idxs,]
   list(trn=trn,tst=tst)
}
