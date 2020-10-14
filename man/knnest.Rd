\name{knnest,meany,vary,loclin,predict.knn,preprocessx,kmin,parvsnonparplot,nonparvsxplot,l1,l2,kNN,bestKperPoint}
\alias{kNN}
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
\alias{MAPE}
\alias{bestKperPoint}

\title{k-NN Nonparametric Regression and Classification}

\description{
Full set of tools for k-NN regression and classification, including both
for direct usage and as tools for assessing the fit of parametric
models.
}

\usage{
kNN(x,y,newx=x,kmax,scaleX=TRUE,PCAcomps=0,expandVars=NULL,expandVals=NULL,
   smoothingFtn=mean,allK=FALSE,leave1out=FALSE, classif=FALSE,
   startAt1=TRUE)
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
MAPE(y,yhat)
bestKperPoint(x,y,maxK,lossFtn="MAPE",classif=FALSE)
}

\arguments{
  \item{x}{"X" data, predictors, one row per data point, in the training
     set.}
  \item{y}{Response variable data in the training set. Vector or matrix,
     the latter case for vector-valued response, e.g. multiclass
     classification.  In that case, can be a vector, either (0,1,2,...,)
     or (1,2,3,...), which automatically is converted into a matrix of
     dummies.}  
  \item{newx}{New data points to be predicted.  If NULL in \code{kNN},
     compute regression functions estimates on \code{x} and save for
     future prediction with \code{predict.kNN}}
  \item{scaleX}{If TRUE, call \code{scale} on \code{x} and \code{newx}}
  \item{PCAcomps}{If positive, transform \code{x} and \code{newx} by
     PCA, using the top \code{PCAcomps} principal components.  Disabled.}
  \item{expandVars}{Indices of columns in \code{x} to expand.}
  \item{expandVals}{The corresponding expansion values.}
  \item{smoothingFtn}{Function to apply to the "Y" values in the 
     set of nearest neighbors.}
  \item{allK}{If TRUE, find regression estimates for all \code{k}
     through \code{kmax}}
  \item{leave1out}{If TRUE, omit the 1-nearest neighbor from analysis}
  \item{classif}{If TRUE, compute the predicted class labels, not just
     the regression function values}
  \item{startAt1}{If TRUE, class labels start at 1, else 0.}
  \item{k}{Number of nearest neighbors} 
  \item{...}{Needed for consistency with generic.  See Details below for
     `arguments.}
  \item{xdata}{X and associated neighbor indices. Output of
     \code{preprocessx}.} 
  \item{object}{Output of \code{knnest}.}
  \item{predpt}{One point on which to predict, as a vector.}
  \item{nearxy}{A set of X neighbors of a point.}
  \item{nearf}{Function to apply to the nearest neighbors of a point.}
  \item{kmax}{Maximal number of nearest neighbors to find.}
  \item{maxK}{Maximal number of nearest neighbors to find.}
  \item{xval}{Cross-validation flag. If TRUE, then the set of nearest 
     neighbors of a point will not include the point itself.}
  \item{lossftn}{Loss function to be used in cross-validation
     determination of "best" \code{k}.}
  \item{nk}{Number of values of \code{k} to try in cross-validation.}
  \item{lmout}{Output of \code{lm}.}
  \item{knnout}{Output of \code{knnest}.}
  \item{cex}{R parameter to control dot size in plot.}
  \item{muhat}{Vector of estimated regression function values.}
  \item{yhat}{Vector of estimated regression function values.}
  \item{returnPts}{If TRUE, return matrix of plotted points.}
  \item{kNNout}{Object returned by \code{kNN}.}
  \item{lossftn}{Loss function.}
  
}

\details{

   The \code{kNN} function is the main tool here; \code{knnest} is being
   deprecated.  (Note too \code{qeKNN}, a wrapper for \code{kNN}.)  Here
   are the capabilities:

   In its most basic form, the function will input training data and
   output predictions for new cases \code{newx}.  By default this is
   done for a single value of the number \code{k} of nearest neighbors,
   but the user can request that it be done for all \code{k} through the
   specified maximum.

   In the second form, \code{newx} is set to NULL.  No predictions are
   made; instead, the regression function is estimated on all data
   points in \code{x}, which are saved in the return value.  Future new cases
   can then be predicted from this saved object, via \code{predict.kNN} 
   (called via the generic \code{predict}).

   Note that in predicting a new case in this second form, say \code{z},
   a 1-nearest neighbor scheme is used:  The closest point in \code{x}
   is determined, and the estimated regression function there is then
   used as the estimate at \code{z}.  In this manner, there is almost no
   computational cost in the prediction stage.  The approximation should
   work well in moderately large datasets, and as noted, will be very
   fast.

   In addition, a novel feature allows the user to weight some
   predictors more than others.  This is done by scaling the given
   predictor up or down, according to a specified value.  Normally, this
   should be done with \code{scaleX = TRUE}, which applies
   \code{scale()} to the data.  In other words, first we create a "level
   playing field" in which all predictors have standard deviation 1.0,
   then scale some of them up or down.

   Another feature of note is to allow local-linear smoothing, by
   setting \code{smoothingFtn} to \code{loclin}.  Here the value of the
   regression function at a point is predicted from a linear fit to the
   point's neighbors.  This may be especially helpful to counteract bias
   near the edges of the data.  As in any regression fit, the number of
   predictors should be considerably less than the number of neighbors.

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

x <- rbind(c(1,0),c(2,5),c(0,5),c(3,3),c(6,3))
y <- c(8,3,10,11,4)
newx <- c(0,0)

kNN(x,y,newx,scaleX=FALSE,2)
# $whichClosest
#      [,1] [,2]
# [1,]    1    4
# $regests
# [1] 9.5

knnout <- kNN(x,y,NULL,scaleX=FALSE,2)
knnout

# $whichClosest
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    3
# [3,]    3    2
# [4,]    4    2
# [5,]    5    4

# $regests
# [1] 9.5 6.5 6.5 7.0 7.5

predict(knnout,c(5,2))  # 7.5

\dontrun{
data(mlb) 
mlb <- mlb[,c(4,6,5)]  # height, age, weight
# fit, then predict 75", age 21, and 72", age 32
knnout <- kNN(mlb[,1:2],mlb[,3],rbind(c(75,21),c(72,32)),25) 
knnout$regests
# [1] 201.84 195.72

# fit now, predict later
knnout <- kNN(mlb[,1:2],mlb[,3],NULL,25) 
predict(knnout,c(70,28)) 
# [1] 185.24

data(peDumms) 
names(peDumms) 
ped <- peDumms[,c(1,20,22:27,29,31,32)] 
names(ped) 

# fit, and predict income of a 35-year-old man, MS degree, occupation 101,
# worked 50 weeks, using 25 nearest neighbors
kNN(ped[,-10],ped[,10],c(35,1,0,0,1,0,0,0,1,50),25) $regests
# [1] 66460

# fit, and predict occupation 101 for a 35-year-old man, MS degree, 
# worked 50 weeks, using 25 nearest neighbors
z <- kNN(ped[,-c(4:8)],ped[,4],c(35,1,0,1,50),25,classif=TRUE)
z$regests
# [1] 0.24  24% chance of occupation 101
z$ypreds
# [1] 0  class 0, i.e. not occupation 101; round(0.24) = 0, 
# computed by user request, classif = TRUE

# 
occs <- as.matrix(ped[, 4:8])
z <- kNN(ped[,-c(4:8)],occs,c(35,1,0,1,72000,50),25,classif=TRUE)
z$ypreds
# [1] 3

# predict occupation in general; let's bring occ.141 back in (was
# excluded as a predictor due to redundancy)
names(peDumms)
#  [1] "age"     "cit.1"   "cit.2"   "cit.3"   "cit.4"   "cit.5"   "educ.1" 
#  [8] "educ.2"  "educ.3"  "educ.4"  "educ.5"  "educ.6"  "educ.7"  "educ.8" 
# [15] "educ.9"  "educ.10" "educ.11" "educ.12" "educ.13" "educ.14" "educ.15"
# [22] "educ.16" "occ.100" "occ.101" "occ.102" "occ.106" "occ.140" "occ.141"
# [29] "sex.1"   "sex.2"   "wageinc" "wkswrkd" "yrentry"
occs <- as.matrix(peDumms[,23:28])  # y must be a matrix in kNN()
z <- kNN(ped[,-c(4:8)],occs,c(35,1,0,1,72000,50),25,classif=TRUE)
z$ypreds
# [1] 3   prediction is occ.102


}

}

\author{
Norm Matloff
}

