
# grid search for good values of tuning parameters; key aspect is
# smoothing of the results

# arguments:

#   pars: named R list, one component for each parameter; component 'x'
#      is the set of desired values for the parameter 'x'
#   theCall: character string giving the call for given regression/
#      classification method, together with the associated prediction
#      function and loss evaluation; the elements of names(pars) 
#      will appear; the data is assumed to be either global or at the
#      same level is the call
#   nCombs: number of possible combinations of 'pars'to evaluate; NULL
#      means all
#   nTst: desired size of holdout set
#   nXval: number of cross-validation runs to perform
#   k: k-NN smoothing parameter for the results

# value:

#   data frame, one column for each element of 'pars', plus 'nTst'
#   columns for the (smoothed) results

# example

fineTuning <- function(pars,regCall,nCombs=NULL,nTst,nXval) {
   
}
