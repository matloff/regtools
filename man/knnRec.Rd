\name{knnRec,anovaRec}
\alias{knnRec},anovaRec}
\alias{predict.knnRec,predict.anovaRec}

\title{Tools for Recommender Systems}

\description{Functions for collaborative filtering.}

\usage{ 
   knnRec(ratings) 
   predict.knnRec(object, user, item, k, minMatch = 1)
   anovaRec(ratingsDF,userCvrs=NULL,itemCvrs=NULL) 
   predict.anovaRec(object,user,item,userCvrs=NULL,itemCvrs=NULL) 

}

\arguments{
   \item{ratings}{A data frame whose first 3 columns are user ID, item
      ID and rating.  All must be numeric, though they need not be
      consecutive nor begin with 1.}
   \item{ratingsDF}{As with \code{ratings}.}
   \item{object}{An object returned by one of the training set
      functions, such as \code{knnRec}.}
   \item{user}{Numeric user ID}
   \item{item}{Numeric item ID}
   \item{k}{Number of nearest neighbors.}
   \item{minMatch}{Minimum number of nearest neighbors to be counted in
      neighborhood mean.}
   \item{userCvrs}{A character vector, listing column names of the user
      covariates.}
   \item{itemCvrs}{A character vector, listing column names of the item
      covariates.}
}

\details{
The function \code{knnRec} does the preparation work on the training
set.  Prediction is then done via \code{predict.knnRec}, via the generic
function \code{predict}.
}

\value{
The function \code{knnRec} returns an object of class \code{'knnRec'},
to be used as input to \code{predict}.  The output of the latter is 

}

\examples{

m <- rbind( c(5,2,1), c(5,4,1), c(2,2,2), c(2,4,4), c(5,3,6), c(2,1,1), c(2,5,2), c(3,4,3), c(2,3,5), c(3,2,1))
colnames(m) <- c('u','v','r')
predict(z,3,1,1)  # 1
predict(z,3,1,1)  # 1, with warning that k was reduced to 1
predict(z,3,3,1)  # 5
predict(z,3,3,2)  # 5.5

}

\author{
Norm Matloff
}

