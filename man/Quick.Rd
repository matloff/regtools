\name{qeLogit}
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
Quick access to regression and multiclass methods, with a very simple
interface.  Intended for convenient initial exploration of a dataset.  For
advanced work, analysts should use the methods directly.  
}

\usage{
qeLogit(data,yName)
qeLin(data,yName)
qeKNN(data,yName,k,scaleX=TRUE) 
qeRF(data,yName,nTree,minNodeSize) 
qeSVM(data,yName,gamma=1.0,cost=1.0) 
qeGBoost(data,yName,nTree=100,minNodeSize=10,learnRate=0.1) 
qeNeural(data,yName,hidden,nEpoch=30) 
predict.qeLogit(object,newx)
predict.qeLin(object,newx)
predict.qeKNN(object,newx)
predict.qeRF(object,newx)
predict.qeSVM(object,newx,k=25,scaleX=TRUE)
predict.qeGBoostt(object,newx)
predict.qeNeural(object,newx)

}

\arguments{
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{k}{Number of nearest neighbors} 
  \item{scaleX}{Scale the features.} 
  \item{nTree}{Number of trees.} 
  \item{minNodeSize}{Minimum number of data points in a tree node.} 
  \item{learnRate}{Learning rate.} 
  \item{hidden}{Vector of units per hidden layer.  Fractional values
     indicated dropout proportions.} 
  \item{nEpoch}{Number of iterations in neural net.
}

\details{

As noted, these functions are intended for quick, first-level analysis
of regression or multiclass classification problems.  Emphasis here is
on convenience. In most cases, the full basket of options in the wrapped
function is not reflected, and second-level analysis should use the
relevant packages directly.

The \code{qe*} functions do model fit.  Each of them has a
\code{predict} method. Arguments are at least: \code{object}, the return
value of the previously-called \code{*Class} function, and \code{newx},
a data frame of points to be predicted.  In some cases, there are
additional parameters.
   
}

\value{

R list with components as follows:

Classification case:

\itemize{

\item \code{predClasses}:  R factor instance of predicted class labels 

\item \code{probs}:  vector/matrix of class probabilities; in the 2-class
case, a vector, the probabilities of Y = 1

}
}

Regression case: vector of predicted values

}

\examples{

\dontrun{

data(peFactors)
pe <- peFactors[,c(1,3,5,7:9)]

# logit
lgout <- qeLogit(pe,'occ')
preds <- predict(lgout,pe[,-3])
mean(preds$predClasses == pe[,3])
# [1] 0.3853161
mean(preds$probs[,1])
# [1] 0.22851
mean(pe[,3] == '100')
# [1] 0.2285714
predict(lgout,pe[1,-3])
# $predClasses
# [1] "102"
#
# $probs
#  100       101       102       106        140        141
# [1,] 0.2924927 0.2162206 0.3331735 0.0439862 0.02220282
# 0.09192411

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
# [1] 25047.36
predict(lmout,pe[1,-5])


# k-NN
knnout <- qeKNN(pe,'occ',25)
preds <- predict(knnout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$pre == 'a100')
mean(preds$probs[,1])
predict(knnout,pe[1,-3])
knnout <- qeKNN(pe,'wageinc',25)
preds <- predict(knnout,pe[,-5])
mean(abs(preds-pe[,5]))
predict(knnout,pe[1,-5])

# RF
rfout <- qeRF(pe,'occ',25)
preds <- predict(rfout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$probs[,1])
mean(preds$predClasses == 'a100')
predict(rfout,pe[1,-3])
rfout <- qeRF(pe,'wageinc',25)
preds <- predict(rfout,pe[,-5])
mean(abs(preds-pe[,5]))
predict(rfout,pe[1,-5])

# SVM
svmout <- qeSVM(pe,'occ',25)
preds <- prd(svmout,pe[,-3])
mean(preds$predClasses == pe[,3])
mean(preds$probs[,1])

# gboost
gbmout <- qeGBoost(pe,'occ')
preds <- predict(gbmout,pe[,-3])
mean(abs(preds - pe[,5]))
mean(preds$probs[,1])
mean(preds$pre == 'a100')
predict(gbimout,pe[1,-3])

 NNs
nnout <- qeNeural(pe,'wageinc',c(5,5),30)
predict(nnout,pe[1,-5])
preds <- predict(nnout,pe[,-5])
mean(abs(preds-pe[,5]))
nnout <- qeNeural(pe,'occ',c(5,5),30)
predict(nnout,pe[1,-3])
preds <- predict(nnout,pe[,-3])
mean(preds == pe[,3])
}
}

\author{
Norm Matloff
}

