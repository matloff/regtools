\name{knnest,meany,vary,loclin,knnpred}
\alias{knnest}
\alias{knnpred}
\alias{meany}
\alias{vary}
\alias{loclin}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification.
}

\usage{
knnest(xydata,k,scalefirst=NULL,nearf=meany,outfile=NULL)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
knnpred(knnout,predpts) 
}

\arguments{
  \item{xydata}{Matrix or data frame, X values in the first columns, Y
     in the last.}
  \item{k}{Number of nearest neighbors} 
  \item{predpts}{A matrix or data frame of X values, at which Y is to be
     predicted.}
  \item{predpt}{One point on which to predict, e.g. a row in
     \code{predpts}.}
  \item{scalefirst}{If non-NULL, scale the X data first, using
     \code{scale}. If \code{scalefirst} is 'default', use the
     default for \code{scale}; otherwise, \code{scalefirst} is a
     2-element R list, consisting of a centers vector and a scales
     vector.}
  \item{nearf}{Function to apply to the nearest neighbors of a point.}
  \item{outfile}{File to save the output of \code{knnest} to, including
     attributes if any.}
}

\details{
   The \code{knnest} function does k-nearest neighbor regression
   function estimation, in any dimension.  In addition to averaging the
   nearby Y values, one can choose local linear smoothing, conditional
   variance estimation or whatever the user desires.

   Scaling is useful if the predictor variables are of different orders
   of magnitude, or for effecting a weighted Euclidean distance.

   The function \code{knnpred} uses the output of \code{knnest} to do
   estimation or prediction on new points.  If scaling had been used in
   the latter, it is picked up here and applied to \code{predpts}.  
   
}

\value{

The \code{knnest} function the vector of estimated regression
function values, based on the estimated regression function values
output by \code{knnest}.  

The function \code{knnpred} returns the predicted Y values at
\code{predpts}.
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
zout <- knnest(pecs1,50,scalefirst='default'))
# find the est. mean income for 42-year-old women, 52 weeks worked, with
# a Master's
knnpred(zout,c(42,2,52,0,0))  # 62106
# what about a man, all else =?
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

