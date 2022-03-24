\name{fineTuning,knnFineTune}
\alias{fineTuning}
\alias{knnFineTune}
\alias{fineTuningPar}
\alias{plot.tuner}

\title{Grid Search Plus More}

\description{
Adds various extra features to grid search for specified tuning 
parameter/hyperparameter combinations:  There is a plot() function, using
parallel coordinates graphs to show trends among the different
combinations; and Bonferroni confidence intervals are computed to avoid
p-hacking.  An experimental smoothing facility is also included.
}

\usage{
fineTuning(dataset,pars,regCall,nCombs=NULL,specCombs=NULL,nTst=500,
   nXval=1,up=TRUE,k=NULL,dispOrderSmoothed=FALSE,
   showProgress=TRUE,...)
\method{plot}{tuner}(x,...)
knnFineTune(data,yName,k,expandVars,ws,classif=FALSE,seed=9999)
fineTuningPar(cls,dataset,pars,regCall,nCombs=NULL,specCombs=NULL,
   nTst=500,nXval=1,up=TRUE,k=NULL,dispOrderSmoothed=FALSE)
}

\arguments{
   \item{...}{Arguments to be passed on by \code{fineTuning} or
      \code{plot.tuner}.}
   \item{x}{Output object from \code{fineTuning}.}
   \item{cls}{A \code{parallel} cluster.}
   \item{dataset}{Data frame etc. containing the data to be analyzed.}
   \item{data}{The data to be analyzed.}
   \item{yName}{Quoted name of "Y" in the column names of \code{data}.}
   \item{expandVars}{Indices of columns in \code{data} to be weighted in
      distance calculations.}
   \item{ws}{Weights to be used for \code{expandVars}.}
   \item{classif}{Set to TRUE for classification problems.}
   \item{seed}{Seed for random number generation.}
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
   \item{showProgress}{If TRUE, print each output line as it becomes ready.}
   \item{specCombs}{A data frame in which the user specifies 
#      hyperparameter parameter combinations to evaluate.}
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

Each chosen combination is run in \code{nXval} folds.  All specified
combinations are run fully, as opposed to a directional "hill descent"
search that hopes it might eliminate poor combinations early in the process.

The function \code{knnFineTune} is a wrapper for \code{fineTuning} for
k-NN problems.

The function \code{plot.tuner} draws a parallel coordinates plot to
visualize the grid. The argument \code{x} is the output of
\code{fineTuning}.  Arguments to specify in the ellipsis are:
\code{col} is the column to be plotted;
\code{disp} is the number to display, with \code{0}, \code{-m} and
\code{+m} meaning cases with the \code{m} smallest 'smoothed' values, all
cases and the \code{m} largest values of 'smoothed', respectively;
\code{jit} avoids plotting coincident lines by adding jitter in the
amount \code{jit * range(x) * runif(n,-0.5,0.5)}.


}

\value{
Object of class **''tuner'**.  Contains the grid results, including
upper bounds of approximate one-sided 95% confidence intervals, both
univariate and Bonferroni-Dunn (adjusted for the
number of parameter combinations).
}

\examples{

# mlb data set, predict weight using k-NN, try various values of k

tc <- function(dtrn,dtst,cmbi,...)
{
   knnout <- kNN(dtrn[,-3],dtrn[,3],dtst[,-3],as.integer(cmbi[1]))
   preds <- knnout$regests
   mean(abs(preds - dtst[,3]))
}

data(mlb)
mlb <- mlb[,3:6]
mlb.d <- factorsToDummies(mlb)
fineTuning(mlb.d,list(k=c(5,25)),tc,nTst=100,nXval=2)

}


\author{
Norm Matloff
}

