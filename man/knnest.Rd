\name{knnest,meany,vary,loclin,predict.knn,preprocessx,kmin,parvsnonparplot,nonparvsxplot,l1,l2}
\alias{knnest}
\alias{predict.knn}
% \alias{parget.knnx}
\alias{meany}
\alias{vary}
\alias{loclin}
\alias{preprocessx}
\alias{kmin}
\alias{parvsnonparplot}
\alias{nonparvsxplot}
\alias{nonparvarplot}
\alias{l2}
\alias{l1}

\title{Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification, including both
for direct usage and as tools for assessing the fit of parametric
models.
}

\usage{
knnest(y,xdata,k,nearf=meany)
preprocessx(x,kmax,xval=FALSE)
meany(predpt,nearxy) 
vary(predpt,nearxy) 
loclin(predpt,nearxy) 
\method{predict}{knn}(object,...)
kmin(y,xdata,lossftn=l2,nk=5,nearf=meany) 
% \method{plot}{kmin}(x,y,...)
parvsnonparplot(lmout,knnout,cex=1.0) 
nonparvsxplot(knnout,lmout=NULL) 
nonparvarplot(knnout,returnPts=FALSE)
l2(y,muhat)
l1(y,muhat)
}

\arguments{
  \item{y}{Response variable data in the training set. Vector or matrix,
     the latter case for vector-valued response, e.g. multiclass
     classification.}  
  \item{x}{X data, predictors, one row per data point, in the training
     set.}
  \item{...}{Needed for consistency with generic.  See Details below for
     `arguments.}
  \item{xdata}{X and associated neighbor indices. Output of
     \code{preprocessx}.} 
  \item{k}{Number of nearest neighbors} 
  \item{object}{Output of \code{knnest}.}
  \item{predpt}{One point on which to predict, as a vector.}
  \item{nearxy}{A set of X neighbors of a point.}
  \item{nearf}{Function to apply to the nearest neighbors of a point.}
  \item{kmax}{Maximal number of nearest neighbors to find.}
  \item{xval}{Cross-validation flag. If TRUE, then the set of nearest 
     neighbors of a point will not include the point itself.}
  \item{lossftn}{Loss function to be used in cross-validation
     determination of "best" \code{k}.}
  \item{nk}{Number of values of \code{k} to try in cross-validation.}
  \item{lmout}{Output of \code{lm}.}
  \item{knnout}{Output of \code{knnest}.}
  \item{cex}{R parameter to control dot size in plot.}
  \item{muhat}{Vector of estimated regression function values.}
  \item{returnPts}{If TRUE, return matrix of plotted points.}
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
   do not need to call \code{preprocessx} again.  Setting \code{xval} to
   TRUE turns out cross-validation: the neighborhood of a point will not
   include the point itself; note that this is set in
   \code{preprocessx}, not in \code{knnest}.
   
   One can specify various types of smoothing by proper specification of
   the \code{nearf} function. The default is \code{meany}, specifying
   the standard averaging of the neighbor Y values.  Another possible
   choice is \code{vary}, specifying calculation of the sample variance
   of those Y values; this is useful in assessing heteroscedasticity in
   a linear model.  
   
   Another choice is to specify local linear smoothing by setting
   \code{nearf} to \code{loclin}.  Here the value of the regression
   function at a point is predicted from a linear fit to the point's
   neighbors.  This may be especially helpful 
   to counteract bias near the edges of the data.  As in any regression
   fit, the number of predictors should be considerably less than the
   number of neighbors.

   The X, i.e. predictor, data will be scaled by the code, so as to put
   all predictor variables on an equal footing.  The scaling parameters
   will be recorded, and then applied later in prediction.

   The function \code{predict.knn} uses the output of \code{knnest} to
   do regression estimation or prediction on new points.  Since the
   output of \code{knnest} is of class \code{'knn'}, one invokes this
   function with the simpler \code{predict}.  The second argument is the
   set of new points, named \code{predpts} within the code.  It is
   specified as a matrix if there is more than one prediction point and
   more than one predictor variable; otherwise, use a vector.
   
   A "1-NN" method is used here:  Given a new point u whose
   Y value we wish to predict, the code finds the single closest row
   in the training set, and returns the previously-estimated regression
   function value at that row.  If u needs to be scaled, specify 
   \code{TRUE} in the third argument of \code{predict};
   otherwise specify FALSE.

   It can be shown that nearest-neighbor (or kernel) regression
   estimates are subject to substantial bias near the fringes of the
   data; the further away from the center of the data, the worse the
   bias.  This can be mitigated by user specification that a local
   linear regression be applied, as follows:  For each new point u to
   predict, the r closest X rows in the training set to u will be found,
   and a linear regression of the corresponding Y values against those X
   values will be computed. The result of that operation will be used to
   predict the Y value at the point u.  The value of r is specified as
   the third argument in the call to \code{predict}; if left
   unspecified, the 1-NN method is used as described above, and it may
   be more accurate than the local-linear approach within the bulk of
   the data set.

   The functions \code{ovaknntrn} and \code{ovaknnpred} are multiclass
   wrappers for \code{knnest} and \code{knnpred}. Here \code{y} is coded
   0,1,...,\code{m}-1 for the \code{m} classes.

   The tools here can be useful for fit assessment of parametric models.
   The \code{parvsnonparplot} function plots fitted values of
   parameteric model vs. kNN fitted, \code{nonparvsxplot} k-NN fitted
   values against each predictor, one by one.

   The functions \code{l2} and \code{l1} are used to define L2 and L1
   loss.

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

One can explore the effects of various numbers of nearest neighbors
\code{k} through the \code{kmin} function.  (This function should be
cosidered experimental.) It will run \code{knnest} for the values of
\code{k} specified in \code{nk}.  If the latter is a number, the range 0
to \code{xdata$kmax} will be divided into \code{nk} equally
subintervals, and the values of \code{k} used will then be the right
endpoints of the subintervals.  The function returns an R list, with the
component \code{meanerrs} containing the cross-validated mean loss
function values and \code{ks} containing the corresponding values of
\code{k}; \code{plot.knn} then plots the former against the latter.

% The \code{parget.knnx} function returns the matrix of indices of nearest
% neighbors, as with the \code{nn.index} component of the return value of
% \code{get.knnx}.

}

\examples{
set.seed(9999)
x <- matrix(sample(1:100,30),ncol=3)
xd <- preprocessx(x[,1],2,TRUE)  # just 1 predictor
ko <- knnest(x[,2],xd,2)  # Y is x[,2]
ko$regest # 1st element = 74.5
predict(ko,matrix(76),TRUE)  # 47.5
ko <- knnest(x[,-1],xd,2)  # Y bivar
ko$regest # 1st row = (74.5,31.5)
predict(ko,matrix(76),TRUE)  # 47.5, 65.0

set.seed(9999)
xe <- matrix(rnorm(30000),ncol=3) 
xe[,-3] <- xe[,-3] + 2 
# xe is 2 predictors and epsilon 
y <- xe \%*\% c(1,0.5,0.2)  # Y
x <- xe[,-3]  # X
xdata <- preprocessx(x,500)  # k as high as 500
zout <- knnest(y,xdata,200) 
predict(zout,matrix(c(1,1),nrow=1),TRUE)  # about 1.55
predict(zout,rbind(c(1,1),c(2,1.2)),TRUE)  # about 1.55, 2.58
predict(zout,rbind(c(0,0)),TRUE)  # about 0.63

\dontrun{
data(prgeng)
pe <- prgeng
# dummies for MS, PhD
pe$ms <- as.integer(pe$educ == 14)
pe$phd <- as.integer(pe$educ == 16)
# computer occupations only
pecs <- pe[pe$occ >= 100 & pe$occ <= 109,]
# for simplicity, let's choose a few predictors
pecs1 <- pecs[,c(1,7,9,12,13,8)]
# will predict wage income from age, gender etc.
# prepare nearest-neighbor data, k up to 50
xdata <- preprocessx(pecs1[,1:5],50)
zout <- knnest(pecs1[,6],xdata,50)  # k = 50
# find the est. mean income for 42-year-old women, 52 weeks worked, with
# a Master's
predict(zout,matrix(c(42,2,52,0,0),nrow=1),TRUE)  # 62106
# try k = 25; don't need to call preprocessx() again
zout <- knnest(pecs1[,6],xdata,25)
predict(zout,matrix(c(42,2,52,0,0),nrow=1),TRUE)  # 69104
# quite a difference; what k values are good?
kmout <- kmin(pecs1[,6],xdata) # at least 50
# what about a man?
zout <- knnest(pecs1[,6],xdata,50)
predict(zout,matrix(c(42,1,52,0,0),nrow=1),TRUE)  # 78588
# form training and test sets, fit on the former and predict on the
# latter
fullidxs <- 1:nrow(pecs1)
train <- sample(fullidxs,10000)
xdata <- preprocessx(pecs1[train,1:5],50)
trainout <- knnest(pecs1[train,6],xdata,50)
testout <- predict(trainout,as.matrix(pecs1[-train,-6]),TRUE)
# find mean abs. prediction error (about $25K)
mean(abs(pecs1[-train,6] - testout))
# examples of fit assessment
# look for nonlinear relations between Y and each X
nonparvsxplot(zout)  # keep hitting Enter for next plot
# there seem to be quadratic relations with age and wkswrkd, so add quad
# terms and run lm()
pecs2 <- pecs1 
pecs2$age2 <- pecs1$age^2 
pecs2$wks2 <- pecs1$wkswrkd^2 
lmout2 <- lm(wageinc ~ .,data=pecs2) 
# check parametric fit by comparing to kNN
parvsnonparplot(lmout2,zout) 
# linear model line somewhat faint, due to large n;
# parametric model seems to overpredict at high end;
# to deal with faintness, reduce size of points
parvsnonparplot(lmout2,zout,cex=0.1) 
# assess homogeneity of conditional variance
nonparvarplot(zout) 
# hockey stick!
}

# Y vector-valued (3 classes)
# 3 clusters, equal wts, coded 0,1,2
n <- 1500 
# within-grp cov matrix
cv <- rbind(c(1,0.2),c(0.2,1)) 
xy <- NULL 
for (i in 1:3) 
   xy <- rbind(xy,rmvnorm(n,mean=rep(i*2.0,2),sigma=cv)) 
y <- rep(0:2,each=n)
xy <- cbind(xy,dummy(y))
xdata <- preprocessx(xy[,-(3:5)],20) # X is xy[,1:2], k <= 20
ko <- knnest(xy[,3:5],xdata,20) 
# find predicted Y for each data pt 
mx <- apply(as.matrix(ko$regest),1,which.max) - 1
# overall correct classification rate
mean(mx == y)  # should be about 0.87

}

\author{
Norm Matloff
}

