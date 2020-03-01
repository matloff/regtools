
# grid search for good values of tuning parameters; key aspect is
# smoothing of the results

# arguments:

#   dataset: data frame or equivalent, containing the "X" and "Y"
#      columns; will be split by the function to 'dtrn' and 'dtst',
#      same level as the call
#   pars: named R list, one component for each parameter; component 'x'
#      is the set of desired values for the parameter 'x'
#   theCall: call for given regression/classification method, together with 
#      the associated prediction function and loss evaluation; the 
#      elements of names(pars) will appear; args are
#      'dtrn', 'dtst' and 'comb', the latter being a given 
#      combinaton of parameter values; see example below
#   nCombs: number of possible combinations of 'pars'to evaluate; NULL
#      means all
#   nTst: desired size of holdout set
#   nXval: number of cross-validation runs to perform
#   k: k-NN smoothing parameter for the results

# value:

#   data frame, one column for each element of 'pars', plus 2*'nTst'
#   columns for the (smoothed) results

# example of 'regCall':

# theCall <- function(dtrn,dtst,cmbi) {
#    getNamedArgs(cmbi)
#    ctout <- ctree(status ~ .,dtrn,
#       control=ctree_control(
#          minsplit=minsplit,
#          minprob=minprob,
#          maxdepth=maxdepth,
#          alpha=alpha))
#    preds <- predict(ctout,dtst)
#    mean(preds == dtst$status)
# }

# fineTuning(dataset=wpbc,pars=pars,regCall=theCall,nCombs=50,nTst=50,nXval=1,k=3)

fineTuning <- 
   function(dataset,pars,regCall,nCombs=NULL,nTst=500,nXval,k=NULL,up=TRUE) 
{
   # generate the basic output data frame
   outdf <- expand.grid(pars)
   if (!is.null(nCombs)) {
      idxsToKeep <- sample(1:nrow(outdf),nCombs)
      outdf <- outdf[idxsToKeep,]
   } else nCombs <- nrow(outdf)
   meanAcc <- rep(NA,nCombs)
   losses <- vector(length=nXval)
   for (combI in 1:nCombs) {
      for (xv in 1:nXval) {
         tstIdxs <- sample(1:nrow(dataset),nTst)
         dtrn <- dataset[-tstIdxs,]
         dtst <- dataset[tstIdxs,]
         cmbi <- outdf[combI,,drop=FALSE]
         losses[xv] <- regCall(dtrn,dtst,cmbi)
      }
      meanAcc[combI] <- mean(losses)
   }
   outdf$meanAcc <- meanAcc
   outdf <- outdf[order(meanAcc,decreasing=!up),]
   if (!is.null(k)) {
      x <- outdf[,1:length(pars)]
      kout <- kNN(x,meanAcc,x,k)
      smoothed <- kout$regests
      outdf$smoothed <- smoothed
      outdf <- outdf[order(smoothed),]
   } else outdf <- outdf[order(meanAcc),]
   row.names(outdf) <- NULL
   output <- list(outdf=outdf,nTst=nTst,nXval=nXval,k=k)
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

# parallel coordinates plot to visualize the grid
plot.tuner <- function(tunerObject,topProp=NULL) {
   require(lattice)
   outdf <- tunerObject$outdf
   nr <- nrow(outdf)
   if (!is.null(topProp)) {
      m <- floor(nrow(outdf))
      outdf <- outdf[(nr-m+1):nr,]
   }
   parallelplot(outdf)

}

