\name{Quick-Explore Wrappers}
\alias{qeLogit}
\alias{qeLin}
\alias{qeKNN}
\alias{qeRF}
\alias{qeSVM}
\alias{qeGBoost}
\alias{qeNeural}
\alias{predict.qeLogit}
\alias{predict.qeLin}
\alias{predict.qeKNN}
\alias{predict.qeRF}
\alias{predict.qeSVM}
\alias{predict.qeGBoost}
\alias{predict.qeNeural}

\title{Quick-Explore Regression/Classification Wrappers}

\description{
Quick access to machine learning methods, with a very simple
interface.  Intended for convenient initial exploration of a dataset,
both to gauge the predictive effectiveness of a model and to do simple
prediction of new cases.  The simplicity also makes the series useful
for teaching.  For advanced work, analysts should use the methods directly.  
}

\usage{
qeLogit(data, holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
qeLin(data,yName, holdout=c(min(1000,round(0.1*nrow(data))),seed=9999)) 
qeKNN(data,yName,k,scaleX=TRUE,newxK=1,
   holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
qeRF(data,yName,nTree,minNodeSize,
   holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
qeSVM(data,yName,gamma=1.0,cost=1.0,
   holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
qeGBoost(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
qeNeural(data,yName,hidden=c(100,100),nEpoch=30,
   holdout=c(min(1000,round(0.1*nrow(data))),seed=9999))
predict.qeLogit(object,newx)
predict.qeLin(object,newx)
predict.qeKNN(object,newx,newxK=1)
predict.qeRF(object,newx)
\method{predict}{qeSVM}(object,newx,k=25)
predict.qeGBoostt(object,newx)
predict.qeNeural(object,newx)

}

\arguments{
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{holdout}{If not NULL, form a holdout set; 2-component vector, 
     c(holdout_size,initial_seed). After fitting to the remaining data,
     evaluate accuracy on the test set.}
  \item{k}{Number of nearest neighbors.  For \code{predict.qeSVM}, this
     is for finding conditional probabilities via \code{scoresToProbs}.} 
  \item{scaleX}{Scale the features.} 
  \item{nTree}{Number of trees.} 
  \item{minNodeSize}{Minimum number of data points in a tree node.} 
  \item{learnRate}{Learning rate.} 
  \item{hidden}{Vector of units per hidden layer.  Fractional values
     indicated dropout proportions.} 
  \item{nEpoch}{Number of iterations in neural net.}
}

\details{

As noted, these functions are intended for quick, first-level analysis
of regression or multiclass classification problems.  Emphasis here is
on convenience and simplicity. 

The idea is that, given a new dataset, the analyst can quickly and
easily try fitting a number of models in succession, say first logistic,
then random forests, then neural networks: 

\code{
qeLogit(data,yColumnName)
qeRF(data,yColumnName)
qeNeural(data,yColumnName)
}

The optional \code{holdout} argument triggers formation of a holdout set
and the corresponding cross-validation evaluation of predictive power.

In most cases, the full basket of options in the wrapped function is not
reflected, and second-level analysis should use the relevant packages
directly.

The \code{qe*} functions do model fit.  Each of them has a
\code{predict} method. Arguments are at least: \code{object}, the return
value of the previously-called \code{*Class} function, and \code{newx},
a data frame of points to be predicted.  In some cases, there are
additional algorithm-specific parameters.

An additional benefit is that the \code{predict} functions work
correctly on new cases with R factors.  The proper levels are assigned
to the new cases.  (Of course, if a new case has a level not in the
original data, nothing can be done.)
   
}

\value{

The value returned by \code{qe*} functions depends on the algorithm, but
with some commonality, e.g. \code{classif}, a logical value indicating
whether the problem was of classification type.  

If a holdout set was requested, an additional returned component will be
\code{testAcc}, the accuracy on the holdout set.  This will be Mean
Absolute Prediction Error in the regression case, and proportion of
correct classification in the classification case.

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

\dontrun{

library(regtools)  
data(peFactors)  
pef <- peFactors[,c(1,3,5,7:9)]  
# most people in the dataset have at least a Bachelor's degree; so let's
# just consider Master's (code 14) and PhD (code 16) as special
pef$educ <- toSubFactor(pef$educ,c('14','16'))  
svmout <- qeSVM(pef,'occ',holdout=NULL) 
# as example of prediction, take the 8th case, but change the gender and
# age to female and 25
newx <- pef[8,-3] 
newx$sex <- '2'
newx$age <- 25
predict(svmout,newx,k=25)
$predClasses
#   8 
# 100 
# Levels: 100 101 102 106 140 141
# 
# $probs
#       100  101  102  106 140 141
# [1,] 0.36 0.44 0.16 0.04   0   0
# so, occupation code 100 is predicted, with a 0.36 conditional
# probability

# logit
lgout <- qeLogit(pe,'occ')
preds <- predict(lgout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$probs[,1])
mean(pe[,3] == '100')
predict(lgout,pe[1,-3])
lgout <- qeLogit(pe,'occ',holdout=c(1000,9999))
lgout$testAcc  


# k-NN
knnout <- qeKNN(pe,'occ',25)
preds <- predict(knnout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$pre == '100')
mean(preds$probs[,1])
predict(knnout,pe[1,-3])
knnout <- qeKNN(pe,'wageinc',25)
preds <- predict(knnout,pe[,-5])
mean(abs(preds-pe[,5]))
predict(knnout,pe[1,-5])
predict(knnout,newx)  # 26791.6

# RF
rfout <- qeRF(pe,'occ',25)
preds <- predict(rfout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$probs[,1])
mean(preds$predClasses == '100')
predict(rfout,pe[1,-3])
rfout <- qeRF(pe,'wageinc',25)
preds <- predict(rfout,pe[,-5])
mean(abs(preds-pe[,5]))
predict(rfout,pe[1,-5])

# SVM
svmout <- qeSVM(pe,'occ',25)
preds <- predict(svmout,pe[,-3])

# gboost
gbmout <- qeGBoost(pe,'occ')
preds <- predict(gbmout,pe[,-3])
mean(abs(preds - pe[,5]))
mean(preds$probs[,1])
mean(preds$pre == '100')
predict(gbimout,pe[1,-3])

# NNs
nnout <- qeNeural(pe,'wageinc',c(5,5),30)
predict(nnout,pe[1,-5])
preds <- predict(nnout,pe[,-5])
mean(abs(preds-pe[,5]))
nnout <- qeNeural(pe,'occ',c(5,5),30)
predict(nnout,pe[1,-3])
preds <- predict(nnout,pe[,-3])
mean(preds == pe[,3])

# lin
pe$occ <- prepend('a',pe$occ)
lmout <- qeLin(pe,'occ')
preds <- predict(lmout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$probs[,1])
mean(pe[,3] == 'a100')
predict(lgout,pe[1,-3])
lmout <- qeLin(pe,'wageinc')
preds <- predict(lmout,pe[,-5])
mean(abs(preds - pe[,5]))
predict(lmout,pe[1,-5])

}
}

\author{
Norm Matloff
}

