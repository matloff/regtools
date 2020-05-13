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
ctCall <- function(dtrn,dtst,cmbi) {
   ctout <- ctree(occ ~ .,dtrn,
      control=ctree_control(minsplit=cmbi$minsp,
         maxdepth=cmbi$maxd,alpha=cmbi$alpha))
   preds <- predict(ctout,dtst)
   mean(preds == dtst$occ)
}
pha=c(0.01,0.05,0.20)),ctCall,nCombs=NULL,nTst=50,nXval=5)
ftout
#  $outdf
#     minsp maxd alpha meanAcc       seAcc    bonfAcc
#  1     20    5  0.05   0.332 0.028705400 0.09176974                              2     50    2  0.05   0.344 0.018330303 0.05860107
#  3     10    2  0.01   0.356 0.009797959 0.03132359
#  4     50    8  0.20   0.356 0.021354157 0.06826818
#  5     50    2  0.01   0.364 0.013266499 0.04241234
#  6     10    2  0.05   0.364 0.040693980 0.13009663
#  7     50    2  0.20   0.368 0.044988888 0.14382723
#  8     20    8  0.01   0.372 0.034985711 0.11184758
#  9     20    2  0.20   0.372 0.034409301 0.11000482
#  10     5    2  0.01   0.376 0.014696938 0.04698538
#  11    50    5  0.01   0.380 0.038987177 0.12464007
#  12    20    8  0.20   0.380 0.033466401 0.1069904
# ...
# ...
plot(ftout)  # overall plot
plot(ftout,disp=5)  # plot the best 5
}

}

\value{
Object of class **''tuner'**.  Contains the grid results, including
standard errors and Bonferroni confidence intervals (adjusted for the
number of parameter combinations).
}


\author{
Norm Matloff
}

