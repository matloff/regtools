\name{fineTuning}
\alias{fineTuning}

\title{Grid Search Plus More}

\description{
Does standard grid search for specified tuning parameter/hyperparameter
combinations, but with extras:  There is a plot() function, using
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
}

\value{
}

\examples{
}

\author{
Norm Matloff
}

