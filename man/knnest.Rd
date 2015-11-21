\name{knnest,meany,vary,loclin,knnpred,parget.knnx,makeknnidxs}
\alias{knnest}
\alias{knnpred}
\alias{parget.knnx}
\alias{meany}
\alias{vary}
\alias{loclin}
\alias{makeknnidxs}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification.
}

\usage{
knnest(xydata,k,knnidxs,scalefirst='default',nearf=meany)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
knnpred(knnout,predpts) 
parget.knnx(data,query,k=10,algorithm="kd_tree",cls=NULL) 
}

\arguments{
  \item{xydata}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{k}{Number of nearest neighbors} 
  \item{knnidxs}{Output of \code{makeknnidxs}.  Row i contains the
     indices of the nearest points to point i.}
  \item{predpts}{A matrix or data frame of X values, at which Y is to be
     predicted.}
  \item{predpt}{One point on which to predict, e.g. a row in
     \code{predpts}.}
  \item{data}{A data matrix.}
  \item{query}{A data matrix.}
  \item{algorithm}{As in \code{FNN::get.knnx}.}
  \item{scalefirst}{If non-NULL, scale the X data first, using
     \code{scale}. If \code{scalefirst} is 'default', use the
     default for \code{scale}; otherwise, \code{scalefirst} is a
     2-element R list, consisting of a centers vector and a scales
     vector.}
  \item{nearf}{Function to apply to the nearest neighbors of a point.}
  \item{x}{Like \code{xydata}, but with "X" data only.}
  \item{m}{Maximal number of nearest neighbors to find.}
}

\details{

   The \code{makeknnidxs} function does the prep work.  For each row in
   \code{x}, the code finds the \code{m} closest rows to that row.  By
   separating this computation from \code{knnest}, one can save a lot of
   overall computing time.  If for instance one wants to try \code{k} at
   25, 50 and 100, one can call \code{makeknnidxs} with \code{m} equal
   to 100, then reuse the results.

   The \code{knnest} function does k-nearest neighbor regression
   function estimation, in any dimension.  In addition to averaging the
   nearby Y values, one can instead choose local linear smoothing,
   conditional variance estimation or whatever the user desires.

   Scaling is useful if the predictor variables are of different orders
   of magnitude, or for effecting a weighted Euclidean distance.

   The function \code{knnpred} uses the output of \code{knnest} to do
   estimation or prediction on new points.  If scaling had been used in
   the latter, it is picked up here and applied to \code{predpts}.  A
   "1-NN" method is used here:  Given a new point u whose "Y" value we
   wish to predict, the code finds the single closest row in the
   training set, and returns the previouslyu-estimated regression
   function value.

   The \code{parget.knnx} function is a wrapper for use of 'parallel'
   package with \code{get.knnx} of the 'FNN' package.
   
}

\value{

The return value of \code{makeknnidxs} is a matrix, with \code{nrow(x)}
rows and \code{m} columns.  Row i contains the indices of the nearest
rows in \code{x} to row i of \code{x}.  The first of these indices is
for the closest point, then for the second-closest, and so on.

The \code{knnest} function the vector of estimated regression
function values, based on the estimated regression function values
output by \code{knnest}.  

The function \code{knnpred} returns the predicted Y values at
\code{predpts}.

The \code{parget.knnx} function returns the matrix of indices of nearest
neighbors, as with the \code{nn.index} component of the return value of
\code{get.knnx}.

}

\examples{
data(prgeng)
pe <- prgeng
# dummies for MS, PhD
pe$ms <- as.integer(pe$educ == 14)
pe$phd <- as.integer(pe$educ == 16)
# computer occupations only
pecs <- pe[pe$occ >= 100 & pe$occ <= 109,]
pecs1 <- pecs[,c(1,7,9,12,13,8)]
# predict wage income from age, gender etc.
knnidxs <- makeknnidxs(pecs1[,-6],50)
zout <- knnest(pecs1,50,knnidxs)
# find the est. mean income for 42-year-old women, 52 weeks worked, with
# a Master's
knnpred(zout,c(42,2,52,0,0))  # 62106
# what about a man, all else =?
knnpred(zout,c(42,1,52,0,0))  # 78588
# change to k = 25, RE-USING knnidx; latter done for 50, so and k <= 50
# would be fine
zout <- knnest(pecs1,25,knnidxs)
knnpred(zout,c(42,1,52,0,0))  # 78588
# form training and test sets, fit on the former and predict on the
# latter
fullidxs <- 1:nrow(pecs1)
train <- sample(fullidxs,1000)
test <- setdiff(fullidxs,train)
trainout <- knnest(pesc1[train,],50,scalefirst='default')
trainout <- knnest(pecs1[train,],50,scalefirst='default')
testout <- knnpred(trainout,pecs1[test,-6])
# find mean abs. prediction error
mean(abs(pecs1[test,6] - testout))

}

\author{
Norm Matloff
}

