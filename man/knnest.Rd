\name{knnest,meany,vary,loclin,knnpred,preprocessx}
\alias{knnest}
\alias{knnpred}
% \alias{parget.knnx}
\alias{meany}
\alias{vary}
\alias{loclin}
\alias{preprocessx}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification.
}

\usage{
knnest(y,xdata,k,nearf=meany)
preprocessx(x,kmax,xval=FALSE)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
knnpred(knnout,predpts) 
}

\arguments{
  \item{y}{Vector of response variable data in the training set.}
  \item{xdata}{X and associated neighbor indices. Output of
     \code{preprocessx}.} 
  \item{xdatarf}{The augmented \code{xdata}, output of \code{knnest} or 
     \code{ovaknntrn}.} 
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
}

\details{

   The \code{preprocessx} function does the prep work.  For each row in
   \code{x}, the code finds the \code{kmax} closest rows to that row.  By
   separating this computation from \code{knnest}, one can save a lot of
   overall computing time.  If for instance one wants to try \code{k} at
   25, 50 and 100, one can call \code{preprocessx} with \code{kmax} equal
   to 100, then reuse the results; in calling \code{knnest} for several
   values of \code{k}, we do not need to call \code{preprocessx} again.

   The \code{knnest} function does k-nearest neighbor regression
   function estimation, in any dimension.  This of course includes the
   2-class classification case, where with Y = 0, 1 representing the two
   classes, the regression function reduces to the conditional
   probability of class 1, given the predictors.

   In addition to averaging the nearby Y values, one can instead choose
   local linear smoothing, conditional variance estimation or whatever
   the user desires.

   The X, i.e. predictor, data will be scaled by the code, so as to put
   all predictor variables on an equal footing.

   The function \code{knnpred} uses the output of \code{knnest} to do
   estimation or prediction on new points.  A "1-NN" method is used
   here:  Given a new point u whose "Y" value we wish to predict, the
   code finds the single closest row in the training set, and returns
   the previously-estimated regression function value at that row.

   The functions \code{ovaknntrn} and \code{ovaknnpred} are multiclass
   wrappers for \code{knnest} and \code{knnpred}. Here \code{y} is coded
   0,1,...,\code{m}-1 for the \code{m} classes.

   % The \code{parget.knnx} function is a wrapper for use of 'parallel'
   % package with \code{get.knnx} of the 'FNN' package.
   
}

\value{

The return value of \code{preprocessx} is an R list. Its \code{x}
component is the scaled \code{x} matrix, with the scaling factors being
recorded in the \code{scaling} component. The \code{idxs} component. is
a matrix, with \code{nrow(x)} rows and \code{m} columns.  Row i contains
the indices of the nearest rows in \code{x} to row i of \code{x}.  The
first of these indices is for the closest point, then for the
second-closest, and so on.  If cross-validation is requrested
(\code{xval = TRUE}, then any point will not be considered a neighbor of
itself.

The \code{knnest} function returns an expanded version of \code{xdata},
with the expansion consisting of a new component \code{regest}, the
estimated regression function values at the training set points.

The function \code{knnpred} returns the predicted Y values at
\code{predpts}.

% The \code{parget.knnx} function returns the matrix of indices of nearest
% neighbors, as with the \code{nn.index} component of the return value of
% \code{get.knnx}.

}

\examples{
xe <- matrix(rnorm(30000),ncol=3)
y <- xe %*% c(1,0.5,0.2)
x <- xe[,-3]
xdata <- preprocessx(x,50)
zout <- knnest(y,xdata,50)
knnpred(zout,c(1,1))  # about 1.5

data(prgeng)
pe <- prgeng
# dummies for MS, PhD
pe$ms <- as.integer(pe$educ == 14)
pe$phd <- as.integer(pe$educ == 16)
# computer occupations only
pecs <- pe[pe$occ >= 100 & pe$occ <= 109,]
pecs1 <- pecs[,c(1,7,9,12,13,8)]
# predict wage income from age, gender etc.
# prepare nearest-neighbor data
xdata <- preprocessx(pecs1[,1:5],50)
zout <- knnest(pecs1[,6],xdata,50)
# find the est. mean income for 42-year-old women, 52 weeks worked, with
# a Master's
knnpred(zout,c(42,2,52,0,0))  # 62106
# try k = 25; don't call preprocessx() again
zout <- knnest(pecs1[,6],xdata,25)
knnpred(zout,c(42,2,52,0,0))  # 69104
# what about a man?; don't call preprocessx(, knnest()) again
knnpred(zout,c(42,1,52,0,0))  # 89200
# form training and test sets, fit on the former and predict on the
# latter
fullidxs <- 1:nrow(pecs1)
train <- sample(fullidxs,10000)
test <- setdiff(fullidxs,train)
xdata <- preprocessx(pecs1[train,1:5],50)
trainout <- knnest(pecs1[train,6],xdata,50)
testout <- knnpred(trainout,pecs1[test,-6])
# find mean abs. prediction error (about $25K)
mean(abs(pecs1[test,6] - testout))

}

\author{
Norm Matloff
}

