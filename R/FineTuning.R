
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

# example of 'theCall':

# function() {
#    rfout <- randomForest(dtrn[,1:5],dtrn[,6],
#       nodesize=comb[1],maxnodes=comb[2])
#    preds <- predict(rfout,dtst)
#    mean(abs(preds - dtst[,6]))
# }

# example

fineTuning <- function(dataset,pars,regCall,nCombs=NULL,nTst=500,nXval,k=NULL) {
   # generate the basic output data frame
   outdf <- expand.grid(pars)
   if (!is.null(nCombs)) {
      idxsToKeep <- sample(1:nrow(outdf),nCombs)
      outdf <- outdf[idxsToKeep,]
   }
   meanLoss <- rep(NA,nCombs)
   losses <- vector(length=nXval)
   for (combI in 1:nCombs) {
      for (xv in 1:nXval) {
         tstIdxs <- sample(1:nrow(dataset),nTst)
         dtrn <- dataset[-tstIdxs,]
         dtst <- dataset[tstIdxs,]
         cmbi <- as.numeric(outdf[combI,])
         losses[xv] <- regCall(dtrn,dtst,cmbi)
      }
      meanLoss[combI] <- mean(losses)
   }
   outdf$meanLoss <- meanLoss
   outdf <- outdf[order(meanLoss),]
   if (!is.null(k)) {
      x <- outdf[,1:length(pars)]
      kout <- kNN(x,meanLoss,x,k)
      smoothed <- kout$regests
      outdf$smoothed <- smoothed
      outdf <- outdf[order(smoothed),]
   } else outdf <- outdf[order(meanLoss),]
   row.names(outdf) <- NULL
   output <- list(outdf=outdf,nTst=nTst,nXval=nXval,k=k)
   class(output) <- 'tuner'
   output
}

