\name{knnest,meany,vary,loclin,predict.knn,preprocessx,kmin,plot.kmin,parvsnonparplot,nonparvsxplot}
\alias{knnest}
\alias{predict.knn}
% \alias{parget.knnx}
\alias{meany}
\alias{vary}
\alias{loclin}
\alias{preprocessx}
\alias{kmin}
\alias{plot.kmin}
\alias{parvsnonparplot}
\alias{nonparvsxplot}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification, including both
for direct usage and as tools for assessing the fit of parametric
models.
}

\usage{
knnest(y,xdata,k,nearf=meany)
preprocessx(x,kmax,xval=FALSE)
meany(predpt,nearxy,nycol) 
vary(predpt,nearxy,nycol) 
loclin(predpt,nearxy,nycol) 
predict.knn(knnout,predpts)
kmin(y,xdata,lossftn=l2,nk=5,nearf=meany) 
plot.kmin(kminout)
parvsnonparplot(lmout,knnout) 
nonparvsxplot(knnout,lmout=NULL) 
}

\arguments{
  \item{y}{Response variable data in the training set. Vector or matrix,
     the latter case for vector-valued response, e.g. multiclass
     classification.}
  \item{x}{Predictor data.}
  \item{xdata}{X and associated neighbor indices. Output of
     \code{preprocessx}.} 
  \item{k}{Number of nearest neighbors} 
  \item{predpts}{A matrix or data frame of X values, at which Y is to be
     predicted.}
  \item{predpt}{One point on which to predict, e.g. a row in
     \code{predpts}.}
  \item{data}{A data matrix.}
  \item{query}{A data matrix.}
  \item{algorithm}{As in \code{FNN::get.knnx}.}
  \item{nearf}{Function to apply to the nearest neighbors of a point.}
  \item{x}{"X" data, predictors, one row per data point, in the training
     set.}
  \item{kmax}{Maximal number of nearest neighbors to find.}
  \item{xval}{Cross-validation flag. If TRUE, then the set of nearest 
     neighbors of a point will not include the point itself.}
  \item{m}{Number of classes in multiclass setting.}
  \item{lossftn}{Loss function to be used in cross-validation
     determination of "best" \code{k}.}
  \item{nk}{Number of values of \code{k} to try in cross-validation.}
  \item{kminout}{Output of \code{kmin}.}
  \item{lmout}{Output of \code{lm}.}
  \item{knnout}{Output of \code{knnest}.}
  \item{nycol}{Number of columns in the input data for "Y" data.}
}

\details{
   
   The \code{knnest} function does k-nearest neighbor regression
   function estimation, in any dimension, i.e. any number of predictor
   variables, and any number of response variables.  This of course 
   includes classification problems case; a scalar Y = 0,1 would
   represent two classes, with the regression function reducing to 
   the conditional probability of class 1, given the predictors.  

   The \code{preprocessx} function does the prep work.  For each row in
   \code{x}, the code finds the \code{kmax} closest rows to that row.
   By separating this computation from \code{knnest}, one can save a lot
   of overall computing time.  If for instance one wants to try the
   number of nearest neighbors \code{k} at 25, 50 and 100, one can call
   \code{preprocessx} with \code{kmax} equal to 100, then reuse the
   results; in calling \code{knnest} for several values of \code{k}, we
   do not need to call \code{preprocessx} again.  
   
   In addition to averaging the neighbor Y values, one can specify other
   types of smoothing by proper specification of the \code{nearf}
   function. For instance, one specifies local linear smoothing by
   setting \code{nearf} to \code{loclin}, one could check
   heteroscedasticity in linear regression models by setting
   \code{nearf} to \code{vary} to estimate conditional variance.

   The X, i.e. predictor, data will be scaled by the code, so as to put
   all predictor variables on an equal footing.

   One can choose the number of nearest neighbors \code{k} through the
   \code{kmin} function, and its output can be plotted via
   \code{plot.kmin}, invoked simply as \code{plot}.

   The function \code{predict.knn} uses the output of \code{knnest} to do
   estimation or prediction on new points.  Since the output of
   \code{knnest} is of class \code{"knn"}, one invokes this function
   with the simpler \code{predict}.  A "1-NN" method is used
   here:  Given a new point u whose "Y" value we wish to predict, the
   code finds the single closest row in the training set, and returns
   the previously-estimated regression function value at that row.

   The functions \code{ovaknntrn} and \code{ovaknnpred} are multiclass
   wrappers for \code{knnest} and \code{knnpred}. Here \code{y} is coded
   0,1,...,\code{m}-1 for the \code{m} classes.

   The tools here can be useful for fit assessment of parametric models.
   The \code{parvsnonparplot} function plots fitted values of
   parameteric model vs. kNN fitted, \code{nonparvsxplot} k-NN fitted
   values against each predictor, one by one.

   % The \code{parget.knnx} function is a wrapper for use of 'parallel'
   % package with \code{get.knnx} of the 'FNN' package.
   
}

\value{

The return value of \code{preprocessx} is an R list. Its \code{x}
component is the scaled \code{x} matrix, with the scaling factors being
recorded in the \code{scaling} component. The \code{idxs} component
contains the indices of the nearest neighbors of each point in the
predictor data, stored in a matrix with \code{nrow(x)} rows and \code{k}
columns.  Row i contains the indices of the nearest rows in \code{x} to
row i of \code{x}.  The first of these indices is for the closest point,
then for the second-closest, and so on.  If cross-validation is
requrested (\code{xval = TRUE}, then any point will not be considered a
neighbor of itself.

The \code{knnest} function returns an expanded version of \code{xdata},
with the expansion consisting of a new component \code{regest}, the
estimated regression function values at the training set points.

The function \code{predict.knn} returns the predicted Y values at
\code{predpts}.  It is called simply via \code{predict}.

The function \code{kmin} returns an R list, with the component
\code{meanerrs} containing the cross-validated mean loss function values
and \code{ks} containing the corresponding values of \code{k};
\code{plot.knn} then plots the former against the latter.

% The \code{parget.knnx} function returns the matrix of indices of nearest
% neighbors, as with the \code{nn.index} component of the return value of
% \code{get.knnx}.

}

\examples{
set.seed(9999)
x <- matrix(sample(1:100,30),ncol=3)
xd <- preprocessx(x[,1],2,TRUE)
ko <- knnest(x[,2],xd,2)
ko$regest # 1st element = 74.5
predict(ko,76)
ko <- knnest(x[,-1],xd,2)
ko$regest # 1st row = (74.5,31.5)
predict(ko,76)

xe <- matrix(rnorm(30000),ncol=3) 
xe[,-3] <- xe[,-3] + 2 
y <- xe %*% c(1,0.5,0.2)  # 2 predictors + epsilon 
x <- xe[,-3]  # exclude epsilon term 
xdata <- preprocessx(x,2500) 
zout <- knnest(y,xdata,200) 
predict(zout,c(1,1))  # about 1.55
kmin(y,xdata,nk=10)

data(prgeng)
pe <- prgeng
# dummies for MS, PhD
pe$ms <- as.integer(pe$educ == 14)
pe$phd <- as.integer(pe$educ == 16)
# computer occupations only
pecs <- pe[pe$occ >= 100 & pe$occ <= 109,]
pecs1 <- pecs[,c(1,7,9,12,13,8)]
# wll predict wage income from age, gender etc.
# prepare nearest-neighbor data
xdata <- preprocessx(pecs1[,1:5],50)
zout <- knnest(pecs1[,6],xdata,50)
# find the est. mean income for 42-year-old women, 52 weeks worked, with
# a Master's
predict(zout,c(42,2,52,0,0))  # 62106
# try k = 25; don't call preprocessx() again
zout <- knnest(pecs1[,6],xdata,25)
predict(zout,c(42,2,52,0,0))  # 69104
# what about a man?; don't call preprocessx(, knnest()) again
predict(zout,c(42,1,52,0,0))  # 89200
# form training and test sets, fit on the former and predict on the
# latter
fullidxs <- 1:nrow(pecs1)
train <- sample(fullidxs,10000)
test <- setdiff(fullidxs,train)
xdata <- preprocessx(pecs1[train,1:5],50)
trainout <- knnest(pecs1[train,6],xdata,50)
testout <- predict(trainout,pecs1[test,-6])
# find mean abs. prediction error (about $25K)
mean(abs(pecs1[test,6] - testout))
# find best cross-validated k, 10

}

\author{
Norm Matloff
}

