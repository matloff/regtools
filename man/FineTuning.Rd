\name{fineTuning}
\alias{fineTuning}

\title{Grid Search Plus More}

\description{
Adds various extra features to grid search for specified tuning 
parameter/hyperparameter combinations:  There is a plot() function, using
parallel coordinates graphs to show trends among the different
combinations; and Bonferroni confidence intervals are computed to avoid
p-hacking.  An experimental smoothing facility is also included.
}

\usage{
fineTuning(dataset, pars, regCall, nCombs = NULL, nTst = 500, 
    nXval = 1, k = NULL, up = TRUE, dispOrderSmoothed = FALSE) 
plot.tuner(tunerObject,col='meanAcc',disp=0) 
}

\arguments{
   \item{dataset}{Data frame etc. containing the data to be analyzed.}
   \item{pars}{R list, showing the desired tuning parameter values.}
   \item{regCall}{Function to be called at each parameter combination,
      performing the model fit etc.}
   \item{nCombs}{Number of parameter combinations to run.  If Null, all
      will be run}.
   \item{nTst}{Number of data points to be in the test set.}
   \item{nXval}{Number of folds to be run for a given data partition and
      parameter combination.}
   \item{k}{Nearest-neighbor smoothing parameter.}
   \item{up}{If TRUE, display results in ascending order of performance
      value.}
   \item{dispOrderSmoothed}{Display in order of smoothed results.}
   \item{tunerObject}{An object output from \code{fineTuning}.}
   \item{col}{Column to be plotted.}
   \item{disp}{Number of lines to be plotted.}
}

\details{

The user specifies the values for each tuning parameter in 
\code{pars}.  This leads to a number of possible combinations of the
parameters.  In many cases, there are more combinations than the user
wishes to try, so \code{nCombs} of them will be chosen at random.

For each combination, the function will run the analysis specified by
the user in \code{regCall}.  The latter must have the call form

\code{ftnName(dtrn,dtst,cmbi}

Again, note that it is \code{fineTuning} that calls this function.  It
will provide the training and test sets \code{dtrn} and \code{dtst}, as
well as \code{cmbi} ("combination i"), the particular parameter
combination to be run at this moment.

Each chosen combination is run in \code{nXval} folds.

}

\examples{

\dontrun{
data(peFactors) 
pef <- peFactors[,c(1,3,5,7:9)]
library(kernlab)  # will do SVM
# here is the user-written function to be called by fineTuning() at each
# combination of tuning parameters
polyCall <- function(dtrn,dtst,cmbi) {
   kout <- ksvm(occ ~ .,data=dtrn,kernel='polydot',
      kpar=list(d=cmbi$d),C=cmbi$C)
   preds <- predict(kout,dtst)
   mean(preds == dtst$occ)
}
ftout <- fineTuning(pef,pars=list(d=c(2:4),C=seq(0.2,1,0.2)),regCall=polyCall,
   nTst=50,nXval=10)
}
plot(ftout)  # overall plot
plot(ftout,disp=5)  # plot the best 5

}

\value{
}

\author{
Norm Matloff
}

