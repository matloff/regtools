\name{qe-Series Wrappers}
\alias{qeLogit}
\alias{qeLin}
\alias{qeKNN}
\alias{qeRF}
\alias{qeSVM}
\alias{qeGBoost}
\alias{qeNeural}
\alias{qeLASSO}
\alias{qePolyLin}
\alias{qePolyLog}
\alias{qeIso}
\alias{qeCompare}
\alias{predict.qeLogit}
\alias{predict.qeLin}
\alias{predict.qeKNN}
\alias{predict.qeRF}
\alias{predict.qeSVM}
\alias{predict.qeGBoost}
\alias{predict.qeNeural}
\alias{predict.qeLASSO}
\alias{predict.qePolyLin}
\alias{predict.qePolyLog}
\alias{predict.qeIso}
\alias{plot.RF}
\alias{plot.LASSO}

\title{Quick-Explore Regression/Classification Wrappers}

\description{
Quick access to machine learning methods, with a very simple
interface.  Intended for convenient initial exploration of a dataset,
both to gauge the predictive effectiveness of a model and to do simple
prediction of new cases.  Just one call needed to fit, no preliminary
setup of model etc.  The simplicity also makes the series useful
for teaching.  For advanced work, analysts may prefer to use 
the methods directly, in order to utilize specialized options.  
}

\usage{
qeLogit(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
qeLin(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
qeKNN(data,yName,k,scaleX=TRUE,holdout=floor(min(1000,0.1*nrow(data))))
qeRF(data,yName,nTree,minNodeSize,holdout=floor(min(1000,0.1*nrow(data))))
qeSVM(data,yName,gamma=1.0,cost=1.0,kernel='radial',degree=2,
   holdout=floor(min(1000,0.1*nrow(data))))
qeGBoost(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
qeNeural(data,yName,hidden=c(100,100),nEpoch=30,
   holdout=floor(min(1000,0.1*nrow(data))))
qeLASSO(data,yName,alpha=1,holdout=floor(min(1000,0.1*nrow(data))))
qePolyLin(data,yName,deg=2,maxInteractDeg = deg,
   holdout=floor(min(1000,0.1*nrow(data))))
qePolyLog(data,yName,deg=2,maxInteractDeg = deg,
   holdout=floor(min(1000,0.1*nrow(data))))
qeCompare(data,yName,qeFtnList,nReps,opts=NULL,seed=9999)
\method{predict}{qeLogit}(object,newx)
\method{predict}{qeLin}(object,newx)
\method{predict}{qeKNN}(object,newx,newxK=1)
\method{predict}{qeRF}(object,newx)
\method{predict}{qeSVM}(object,newx,k=25)
\method{predict}{qeGBoost}(object,newx)
\method{predict}{qeNeural}(object,newx)
\method{predict}{qeLASSO}(object,newx)
\method{predict}{qePoly}(object,newx)
\method{plot}{qeLASSO}(object,newx)
\method{plot}{qeSVM}(object,newx,k=25)
\method{plot}{qeRF}(object,newx)
}

\arguments{
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{holdout}{If not NULL, form a holdout set of the specified size.
     After fitting to the remaining data, evaluate accuracy on the test set.}
  \item{k}{Number of nearest neighbors. In functions other than
     \code{qeKNN} for which this is an argument, it is the number of 
     neighbors to use in finding conditional probabilities via 
     \code{knnCalib}.} 
  \item{scaleX}{Scale the features.} 
  \item{nTree}{Number of trees.} 
  \item{minNodeSize}{Minimum number of data points in a tree node.} 
  \item{learnRate}{Learning rate.} 
  \item{hidden}{Vector of units per hidden layer.  Fractional values
     indicated dropout proportions.} 
  \item{nEpoch}{Number of iterations in neural net.}
  \item{alpha}{1 for LASSO, 2 for ridge.}
  \item{gamma}{Scale parameter in \code{e1071::svm}.}
  \item{cost}{Cost parameter in \code{e1071::svm}.}
  \item{kernel}{One of 'linear','radial','polynomial' and 'sigmoid'.}
  \item{degree}{Degree of SVM polynomial kernel, if any.}
  \item{qeFtnList}{Character vector of \code{qe*} names.}
  \item{nReps}{Number of holdout sets to generate.}
  \item{opts}{R list of optional arguments for none, some or all of th
     functions in \code{qeFtnList}.}
  \item{seed}{Seed for random number generation.}
}

\details{

As noted, these functions are intended for quick, first-level analysis
of regression or multiclass classification problems.  Emphasis here is
on convenience and simplicity.  Currently k-NN, SVM, random forests,
gradient boosting, linear model, LASSO and polynomial regression are
offered. 

The idea is that, given a new dataset, the analyst can quickly and
easily try fitting a number of models in succession, say first k-NN,
then random forests: 

\preformatted{
# built-in data on major league baseball players
> data(mlb)  
> mlb <- mlb[,3:6]  # position, height, weight, age

# fit models
> knnout <- qeKNN(mlb,'Weight',k=25)
> rfout <- qeRF(mlb,'Weight')

# mean abs. pred. error on holdout set, in pounds
> knnout$testAcc
[1] 11.75644
> rfout$testAcc
[1] 12.6787

# predict a new case
> newx <- data.frame(Position='Catcher',Height=73.5,Age=26)
> predict(knnout,newx)
       [,1]
[1,] 204.04
> predict(rfout,newx)
      11 
199.1714

# how about some other ML methods?
> lassout <- qeLASSO(mlb,'Weight')
> lassout$testAcc
[1] 14.23122
# poly reg, degree 3 
> polyout <- qePolyLin(mlb,'Weight',3)
> polyout$testAcc
[1] 13.55613
> nnout <- qeNeural(mlb,'Weight')
# ...
> nnout$testAcc
[1] 12.2537
# try some nondefault hyperparams
> nnout <- qeNeural(mlb,'Weight',hidden=c(200,200),nEpoch=50)
> nnout$testAcc
[1] 15.17982

}

The optional \code{holdout} argument triggers formation of a holdout set
and the corresponding cross-validation evaluation of predictive power.
Note that if a holdout is formed, the return value will consist of the
fit on the training set, not on the full original dataset.

In most cases, the full basket of options in the wrapped function is not
reflected, and second-level analysis should use the relevant packages
directly.

The \code{qe*} functions do model fit.  Each of them has a
\code{predict} method, and some also have a \code{plot} method.
Arguments for \code{qe*} are at least: \code{data} and \code{yName};
arguments for \code{predict} are at least: \code{object}, the return
value from \code{qe*}, and \code{newx}, a data frame of points to be
predicted.  In some cases, there are additional algorithm-specific
parameters; default values are provided.

An additional benefit is that the \code{predict} functions work
correctly on new cases with R factors.  The proper levels are assigned
to the new cases.  (Of course, if a new case has a level not in the
original data, nothing can be done.)

The function \code{qeLin} handles classification problems as
multivariate-outcome linea models. If one's goal is prediction, it can
be much faster than \code{qeLogit}, often with comparable accuracy.

The \code{qePolyLin} function does polynomial regression of the indicated
degree. In the above example degree 3 means all terms through degree 3,
e.g. \code{Height * Age^2}.  Dummy variables are handled properly, e.g.
no powers of a dummy are generatd.  The logistic polynomial regression version
is \code{qePolyLog}.

The \code{qeCompare} function does quick-and-easy cross-validated
comparisons among the \code{qe*} functions.  The same holdout sets are
generated and used by all the functions.  Default values of
hyperparameters of those functions can be set via \code{opts}.

The \code{qeIso} function is intended mainly for use as a smoothing
method in calibration actions.

}

\value{

The value returned by \code{qe*} functions depends on the algorithm, but
with some commonality, e.g. \code{classif}, a logical value indicating
whether the problem was of classification type.  

If a holdout set was requested, an additional returned component will be
\code{testAcc}, the accuracy on the holdout set.  This will be Mean
Absolute Prediction Error in the regression case, and proportion of
misclassified cases in the classification case.

The value returned by the \code{predict} functions is an
R list with components as follows:

Classification case:

\itemize{

\item \code{predClasses}:  R factor instance of predicted class labels 

\item \code{probs}:  vector/matrix of class probabilities; in the 2-class
case, a vector, the probabilities of Y = 1

}

Regression case: vector of predicted values

}

\examples{

# see also 'details' above

\dontrun{

data(peFactors)  
pef <- peFactors[,c(1,3,5,7:9)]  
# most people in the dataset have at least a Bachelor's degree; so let's
# just consider Master's (code 14) and PhD (code 16) as special
pef$educ <- toSubFactor(pef$educ,c('14','16'))  

# predict occupation; 6 classes, 100, 101, 102, 106, 140, 141, using SVM
svmout <- qeSVM(pef,'occ',holdout=NULL) 
# as example of prediction, take the 8th case, but change the gender and
# age to female and 25; note that by setting k to non-null, we are
# requesting that conditional probabilities be calculated, via
# knnCalib(), here using 25 nearest neighbors
newx <- pef[8,-3] 
newx$sex <- '2'
newx$age <- 25
predict(svmout,newx,k=25)
# $predClasses
#   8 
# 100 
# Levels: 100 101 102 106 140 141
# $dvals
#      102/101    102/100   102/141  102/140  102/106    101/100  101/141
# 8 -0.7774038 -0.5132022 0.9997894 1.003251 0.999688 -0.4023077 1.000419
#    101/140   101/106  100/141  100/140  100/106   141/140    141/106   140/106
# 8 1.000474 0.9997371 1.000088 1.000026 1.000126 0.9460703 -0.4974625 -1.035721
# 
# $probs
#       100  101  102  106 140  141
# [1,] 0.24 0.52 0.12 0.08   0 0.04
#
# so, occupation code 100 is predicted, with a 0.36 conditional
# probability

# if holdout evaluation is desired as well, say 1000 cases, seed 9999:
> svmout <- qeSVM(pef,'occ',holdout=c(1000,9999)) 
> svmout$testAcc
[1] 0.622  # 62% error rate (overall rate for 6 classes)

# linear
# lm() doesn't like numeric factor levels, so prepend an 'a'
pef$occ <- prepend('a',pef$occ)
lmout <- qeLin(pef,'occ')
predict(lmout,pef[1,-3])  # occ 100, prob 0.3316
lmout <- qeLin(pef,'wageinc')
predict(lmout,pef[1,-5])  # 70857.79

qeCompare(mlb,'Weight',c('qeLin','qeKNN','qeRF'),25)
#   qeFtn  meanAcc
# 1 qeLin 13.30490
# 2 qeKNN 13.72708
# 3  qeRF 13.46515
qeCompare(mlb,'Weight',c('qeLin','qeKNN','qeRF'),25,
   list(qeKNN='k=5',qeRF='nTree = 100, minNodeSize = 15'))
#   qeFtn  meanAcc
# 1 qeLin 13.30490
# 2 qeKNN 14.34051
# 3  qeRF 13.02334


}

}

\author{
Norm Matloff
}

