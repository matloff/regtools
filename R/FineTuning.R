
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
#      elements of names(pars) will appear; will make use of 
#      'dtrn' and 'dtst', and 'comb', the latter being a given 
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

fineTuning <- function(dataset,pars,regCall,nCombs=NULL,nTst,nXval) {
   # generate the basic output data frame
   outdf <- expand.grid(pars)
   if (!is.null(nCombs)) {
      idxsToKeep <- sample(1:nrow(dataset),nCombs)
      outdf <- outdf[idxsToKeep,]
   } 
   for (xv in 1:nXval) {
      tstIdxs <- sample(1:nrow(dataset),nTst)
      assign('dtrn',dataset[-tstIdxs,]
      assign('dtst',dataset[tstIdxs,]


   }
   
}
