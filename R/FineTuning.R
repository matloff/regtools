
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
#   k: k-NN smoothing parameter for the results; if NULL, then no smoothing
#   dispOrderSmoothed: if TRUE and k is non-null, then output will be
#      arranged in order of the 'smoothed' column; otherwise in order of
#      the meanAcc column
#   showProgress: if TRUE, rows of the output will be printed as they
#      become available
#   specCombs: a data frame in which the user specifies which
#      hyperparameter parameter combinations to evaluate; there both for
#      completeness but also for use in fineTuningPar(); one labeled
#      column for each hyperparameter
#   ...: application-specific values needed by regCall() but constant
#      across combinations

# value:

#   data frame, one column for each element of 'pars', followed by a
#   meanAcc ("mean accuracy") column; then columns for CIs,
#   Bonferroni CIs,and (if k is non-NULL) smoothed versions of
#   meanAcc

# example
# 
# tc <- function(dtrn,dtst,cmbi,...)
# {
#    knnout <- kNN(dtrn[,-3],dtrn[,3],dtst[,-3],as.integer(cmbi[1]))
#    preds <- knnout$regests
#    mean(abs(preds - dtst[,3]))
# }
# 
# data(mlb)
# mlb <- mlb[,3:6]
# mlb.d <- factorsToDummies(mlb) 
# fineTuning(mlb.d,list(k=c(5,25)),tc,nTst=100,nXval=2) 



fineTuning <- function(dataset,pars,regCall,nCombs=NULL,specCombs=NULL,
   nTst=500,nXval=1,up=TRUE,k=NULL,dispOrderSmoothed=FALSE,
   showProgress=TRUE,...) 
{
   # parameter checks
   if (!interactive()) showProgress <- FALSE
   ### if (!is.null(specCombs)) nCombs <- nrow(specCombs)
   # holding off for now on general smoothing
   if (!is.null(k) && length(pars) > 1) 
      stop('smoothing is currently recommended only for 1 parameter')

   # generate the basic output data frame 
   outdf <- makeOutdf(pars,nCombs,specCombs)
   nCombs <- nrow(outdf)

   meanAcc <- rep(NA,nCombs)
   seAcc <- rep(NA,nCombs)
   losses <- vector(length=nXval)
   done <- FALSE

   # OK, ready to loop through the combinations
   for (combI in 1:nCombs) {
      for (xv in 1:nXval) {
         # create training and test sets
         tmp <- partTrnTst(dataset,nTest=nTst)
         dtrn <- tmp$trn
         dtst <- tmp$tst
         cmbi <- outdf[combI,,drop=FALSE]

         loss <- try(
            R.utils::withTimeout(
               regCall(dtrn,dtst,cmbi,...),
               timeout = 300.0)  # later, make it an arg
         )
         if (is.null(loss) || inherits(loss,'try-error')) {
            cmb1 <- cmbi[1,]
            cat('error in comb ')
            cat(unlist(cmb1),'\n')
            if (!interactive()) stop()
            resp <- readline('continue? ')
            if (substr(resp,1,1) == 'n') {
              done <- TRUE
              break
            }
         } else losses[xv] <- loss
      }
      meanAcc[combI] <- mean(losses)
      if (showProgress) {
         catDFRow(cmbi)
         cat(' ',meanAcc[combI],'\n')
      }
      seAcc[combI] <- sd(losses) / sqrt(nXval)
      if (done) break
   }
   outdf$meanAcc <- meanAcc
   outdf$CI <- meanAcc + 1.65*seAcc 
   zval <- -qnorm(0.05/nrow(outdf))
   outdf$bonfCI <- meanAcc + zval * seAcc
   outdf <- outdf[order(meanAcc,decreasing=!up),]
   if (!is.null(k)) 
      outdf <- doSmoothing(outdf,k,pars,meanAcc,dispOrderSmoothed) 
   row.names(outdf) <- NULL
   output <- list(outdf=outdf,nTst=nTst,nXval=nXval,k=k,
      up=up,dispOrderSmoothed=dispOrderSmoothed)
   class(output) <- 'tuner'
   output
}

# creates the basic parameter grid; called by both fineTuning() and
# fineTuningPar() 

# if specCombs is given, it will be returned, with no further processing
# wrt nCombs

makeOutdf <- function(pars,nCombs=NULL,specCombs=NULL) {
   if (!is.null(specCombs)) return(specCombs)
   outdf <- expand.grid(pars,stringsAsFactors=FALSE) 
   if (!is.null(nCombs)) {
      idxsToKeep <- sample(1:nrow(outdf),nCombs)
      outdf <- outdf[idxsToKeep,]
   }
   outdf
}

doSmoothing <- function(outdf,k,pars,meanAcc,dispOrderSmoothed) {
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
   outdf
}

# parallel interface to fineTuning(); will split the set of
# hyperparameter combinations into chunks, have each worker node process
# a chunk, then collect and combine the results into a single object of
# class 'tuner'

# arguments:

#    cls: parallel platform; if a positive integer, then a 'parallel' cluster
#    will be formed on the host machine, with the specified number of cores
 
#    ftCall: character string giving the fineTuning() call to be executed at
#    each worker
 
# value:

#   'tuner' object that combines the ones produced by the worker nodes

fineTuningPar <- function(cls,dataset,pars,regCall,nCombs=NULL,specCombs=NULL,
   nTst=500,nXval=1,up=TRUE,k=NULL,dispOrderSmoothed=FALSE)
{
   # set up cluster
   if (is.numeric(cls)) {
      cls <- parallel::makeCluster(cls)
      partools::setclsinfo(cls)
   } else if (inherits(cls,'cluster')) {
      partoolsenv <- NULL  # for CRAN
      resp <- try(
         parallel::clusterEvalQ(cls,partoolsenv$ncls)
      )
      if (inherits(resp,'try-error')) {
         stop('setclsinfo() not called')
      }
   } else stop('invalid cls')
   parallel::clusterEvalQ(cls,library(partools))
   parallel::clusterEvalQ(cls,library(regtools))
   parallel::clusterEvalQ(cls,library(R.utils))
   parallel::clusterEvalQ(cls,library(h2o))
   parallel::clusterEvalQ(cls,h2o::h2o.init())

   # export all args to the cluster nodes
   argNames <- clusterExportArgs(cls)
   argNames <- argNames[argNames != 'cls']

   # create the full grid data frame, and parcel it out to the cluster
   # nodes
   specCombs <- makeOutdf(pars,specCombs)
   partools::distribsplit(cls,'specCombs')

   # now create the fineTuning() call (in character form)
   ftCall <- 'fineTuning('
   for (i in 1:length(argNames)) {
      nm <- argNames[i]
      ftCall <- paste0(ftCall,nm,'=',nm)
      if (i < length(argNames)) {
         ftCall <- paste0(ftCall,',')
      } else {
         ftCall <- paste0(ftCall,',showProgress=FALSE)')
      }
   }

   # and now do the call, and combine the results
   resp <- partools::doclscmd(cls,ftCall)
   adls <- function(ll1,ll2) partools::addlists(ll1,ll2,rbind)
   combinedChunks <- Reduce(adls,resp)
   outdf <- combinedChunks$outdf
   zval <- -qnorm(0.025/nrow(outdf))
   outdf$bonfAcc <- zval * outdf$seAcc
   combinedChunks$outdf <- outdf
   combinedChunks
}

# parallel coordinates plot to visualize the grid; tunerObject is output
# of fineTuning(); disp is number to display, with 0, -m and +m meaning
# cases with the m smallest 'smoothed' value, all cases and the m
# largest values of 'smoothed', respectively

#  obj: an object returned from fineTuning()
#  col: column to be plotted
#  disp: if non-0, number of lines to plot, largest or smallest values,
#     depending on whether disp is positive or negative
#  jit: avoids plotting coincident lines by adding jitter; amount is
#     jit * range(x) * runif(n,-0.5,0.5)
# plot.tuner <- function(tunerObject,col='meanAcc',disp=0,jit=0.05) {
plot.tuner <- function(x,...) 
{
   arglist <- list(...)
   tmp <- arglist['col'][[1]]
   col <- if (!is.null(tmp)) tmp else 'meanAcc'
   tmp <- arglist['disp'][[1]]
   disp <- if (!is.null(tmp)) tmp else 0
   tmp <- arglist['jit'][[1]]
   jit <- if (!is.null(tmp)) tmp else 0.05

   outdf <- x$outdf
   macol <- which(names(outdf) == 'meanAcc')
   outdf <- outdf[,1:macol]
   if (jit > 0.0) {
      nc <- ncol(outdf)
      for (i in 1:(nc-3)) {
         dfCol <- outdf[,i]
         if (is.numeric(dfCol)) {
            rng <- max(dfCol) - min(dfCol)
            outdf[,i] <- dfCol + jit * rng * runif(length(dfCol),-0.5,0.5)
         }
      }
   }
   if (col == 'smoothed') outdf$meanAcc <- NULL
   else outdf$smoothed <- NULL
   if (disp != 0) {
      nc <- ncol(outdf)
      if (abs(disp) < nc - 1) stop('disp too small')
      ord <- order(outdf[,nc],decreasing=(disp > 0))
      outdf <- outdf[ord[1:abs(disp)],]
   }
   nr <- nrow(outdf)
   cdparcoord::discparcoord(outdf,k=nr,differentiate=TRUE)
}

pltt <- plot.tuner

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

# called from within a function f(), it will export all the arguments of
# the latter, including defaults, to the nodes in cluster cls; it will
# also return the names; ellipsis args are skipped

clusterExportArgs <- function(cls) {
   argNames <- names(formals(sys.function(sys.parent(1))))
   argNames <- argNames[argNames != '...']
   parallel::clusterExport(cls,argNames,envir=parent.frame())
   argNames
}

# argVec is a row from the grid data frame in fineTuning(), with
# parameter names; converts the values in argVec to variables of those
# names; CURRENTLY NOT USED

getNamedArgs <- function(argVec) 
{
   for (nm in names(argVec)) {
      assign(nm,argVec[[nm]],envir=parent.frame())
   }
}

# apply regtools:::fineTuning() to choosing the expansion factors in
# kNN()

# arguments:

#    data: data frame (or matrix with col names), including both "X" and "Y"
#    yName: name of the data column containing "Y"
#    expandVars: indices of the column numbers 

knnFineTune <- function(data,yName,k,expandVars,ws,classif=FALSE,
   seed=9999) 
{
   if (classif) stop('not ready for classification problems')

   ycol <- which(names(data) == yName)
   # may need to shift some of expandVars over, once "Y" is removed
   if (ycol < ncol(data)) {
      topvars <- which(expandVars > ycol)
      if (length(topvars) > 0) {
         expandVars[topvars] <- expandVars[topvars] - 1
      }
   }
   expandNms <- colnames(data[,-ycol])[expandVars]

   theCall <- function(dtrn,dtst,cmbi) {
      x <- dtrn[,-ycol]
      y <- dtrn[,ycol]
      newx <- dtst[,-ycol]
      newy <- dtst[,ycol]
      cmbi <- as.numeric(cmbi)
      knnout <- kNN(x,y,newx,k,expandVars=expandVars,expandVals=cmbi)
      mean(abs(knnout$regests - newy))
   }
   
   # wcols <- paste0('w',1:length(expandVars),'=ws',collapse=',')
   # wcols <- paste0('list(',wcols,')')
   wcols <- paste0(expandNms,'=ws',collapse=',')
   wcols <- paste0('list(',wcols,')')
   fineTuning(dataset=data,pars=eval(parse(text=wcols)),regCall=theCall,
      nXval=10)

}

