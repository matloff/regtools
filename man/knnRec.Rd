\name{knnRec,anovaRec,mfRec,predictMany}
\alias{knnRec,anovaRec,mfRec,predictMany}
\alias{predict.knnRec,predict.anovaRec,predict.anovaRec.mfRec}

\title{Tools for Recommender Systems}

\description{Functions for collaborative filtering.}

\usage{ 
   knnRec(ratings) 
   predict.knnRec(object, user, item, k, minMatch = 1)
   anovaRec(ratingsDF,userCvrs=NULL,itemCvrs=NULL) 
   predict.anovaRec(object,user,item,userCvrVals=NULL,itemCvrVals=NULL) 
   mfRec(ratings,rnk=10,nmf=FALSE,niter=20,lambda=0) 
   predict.mfRec(object,user,item,)
   predictMany(object,newxs)

}

\arguments{
   \item{ratings}{A data frame whose first 3 columns are user ID, item
      ID and rating.  All must be numeric, though they need not be
      consecutive nor begin with 1.}
   \item{ratingsDF}{As with \code{ratings}.}
   \item{object}{An object returned by one of the training set
      functions, such as \code{knnRec}.}
   \item{user}{Numeric user ID}
   \item{item}{Numeric item ID.  In the case of \code{mfRec}, specifying
      a negative number -m will result in the m top-rated items for this
      user.}
   \item{k}{Number of nearest neighbors.}
   \item{minMatch}{Minimum number of nearest neighbors to be counted in
      neighborhood mean.}
   \item{userCvrs}{A character vector, listing column names of the user
      covariates.}
   \item{itemCvrs}{A character vector, listing column names of the item
      covariates.}
   \item{userCvrVals}{An R list, with components of the form covariate
      name = covariate value.}
   \item{itemCvrVals}{An R list, with components of the form covariate
      name = covariate value.}
   \item{rnk}{Matrix rank.}
   \item{nmf}{If TRUE, use NMF instead of SVD.}
   \item{niter}{Number of iterations.}
   \item{lambda}{L1 regularizer.}
   \item{newxs}{Data frame consisting of a user column and an item
      column.}
}

\details{

Several methods for collaborative filtering.

\itemize{

   \item The function \code{knnRec} uses a k-Nearest Neighbor method.
   If we desire a rating for user u of item i, the method first finds
   all users who rated i, then averages the ratings of i by the k
   nearest of these users to u.  
   
   Requires a large dataset.

   \item The function \code{anovaRec} gets its name from its ANOVA-like
   decompositions, e.g. 
   
   E(Y_ij) = mu + alpha_i + beta_j + (alphabeta)_ij

   where Y_ij is the rating user i gives item j.  Only 2-way
   interactions are allowed.

   The function \code{anovaRec} allows for covariates ("side
   information").  These may be useful, say, for prediction of new cases
   having very little rating data.

   Very fast, almost no memory usage.

   \item The function \code{mfRec} uses the matrix factorization method.
   One can also request multiple top-rated items.

   In some cases, this will be the most accurate method.  Slower, and
   somewhat memory-intensive.

}

The function \code{knnRec} does the preparation work on the training
set.  Prediction is then done via \code{predict.knnRec}, via the generic
function \code{predict}.  Similarly for \code{anovaRec} for the others.

For \code{anovaRec}, the covariates are divided into user covariates and
item covariates.  To see why, consider the famous MovieLens data, say
using Genre as a covariate.  Since knowledge of the item/film implies
knowledge of the genre, interaction between item and covariate in this
cases makes no sense, but a user/genre interaction may be reasonable.

At present, only categorical covariates are allowed. Use
\code{discretize} to convert continuous variables.

The \code{predict.*} functions operate on a single (user,item) pair.  If
many new pairs are to be predicted, use the wrapper \code{predictMany}.

}

\value{

The function \code{knnRec} returns an object of class \code{'knnRec'},
to be used as input to \code{predict}.  The output of the latter is 
the predicted value.  Similarly for the others.

}

\examples{

m <- rbind( c(5,2,1), c(5,4,1), c(2,2,2), c(2,4,4), c(5,3,6), c(2,1,1), c(2,5,2), c(3,4,3), c(2,3,5), c(3,2,1))
m <- as.data.frame(m)
set.seed(9999)
m <- cbind(m,sample(5:10,10,replace=TRUE))  # add a user covar
colnames(m) <- c('u','v','r','x')
obj <- anovaRec(m,'x')
# userCvrXEffects$x['3','8'] = 0.6
predict(obj,3,3)  # 4.9
predict(obj,3,3,list(x=6))  # 3.9
m <- cbind(m,sample(1:2,10,replace=T))
colnames(m) <- c('u','v','r','x','y')  # add an item covar
obj <- anovaRec(m,userCvrs='x',itemCvrs='y')
predict(obj,3,3,list(x=6),list(y=1))  # 9.1

\dontrun{

library(dslabs)
data(movielens)
ml <- movielens[,c(5,1,6,4)]
z <- sapply(1:nrow(ml),function(rw) length(grep('Action',ml[rw,4])) > 0)
ml$action <- as.integer(z) # dummy for Action genre
set.seed(9999)
tstidxs <- sample(1:nrow(ml),1000)
trn <- ml[-tstidxs,]
tst <- ml[tstidxs,]
anovaout <- anovaRec(ml,userCvrs='action')
predtst <- function(i) 
   predict(anovaout,tst[i,1],tst[i,2],list(action=tst[i,5]))
preds <- sapply(1:1000,predtst)
mean(abs(preds - tst[,3]))  # 0.636

}


}

\author{
Norm Matloff
}

