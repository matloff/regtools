\name{knnRec,anovaRec}
\alias{knnRec,anovaRec}
\alias{predict.knnRec,predict.anovaRec}

\title{Tools for Recommender Systems}

\description{Functions for collaborative filtering.}

\usage{ 
   knnRec(ratings) 
   predict.knnRec(object, user, item, k, minMatch = 1)
   anovaRec(ratingsDF,userCvrs=NULL,itemCvrs=NULL) 
   predict.anovaRec(object,user,item,userCvrVals=NULL,itemCvrVals=NULL) 

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
   \item{userCvrVals}{An R list, with components of the form covariate
      name = covariate value.}
   \item{itemCvrVals}{An R list, with components of the form covariate
      name = covariate value.}
}

\details{

The function \code{knnRec} does the preparation work on the training
set.  Prediction is then done via \code{predict.knnRec}, via the generic
function \code{predict}.  Similarly for \code{anovaRec}.

The function \code{anovaRec} allows for covariates ("side information").
These may be useful, say, for prediction of new cases having very little
rating data.

The covariates are divided into user covariates and item covariates.  To see why,
consider the famous MovieLens data, say using Genre as a covariate.
Since knowledge of the item/film implies knowledge of the genre,
interaction between item and covariate in this cases makes no sense, but
a user/genre interaction may be reasonable.

The function \code{anovaRec} gets its name from its ANOVA-like
decompositions, e.g.

E(Y_ij) = mu + alpha_i + beta_j + (alphabeta)_ij

where Y_ij is the rating user i gives item j.  Only 2-way interactions
are allowed.

}

\value{

The function \code{knnRec} returns an object of class \code{'knnRec'},
to be used as input to \code{predict}.  The output of the latter is 
the predicted value.  Similarly for \code{anovaRec}.

}

\examples{

m <- rbind( c(5,2,1), c(5,4,1), c(2,2,2), c(2,4,4), c(5,3,6), c(2,1,1), c(2,5,2), c(3,4,3), c(2,3,5), c(3,2,1))
colnames(m) <- c('u','v','r')
predict(z,3,1,1)  # 1
predict(z,3,1,1)  # 1, with warning that k was reduced to 1
predict(z,3,3,1)  # 5
predict(z,3,3,2)  # 5.5

m <- rbind( c(5,2,1), c(5,4,1), c(2,2,2), c(2,4,4), c(5,3,6), c(2,1,1), c(2,5,2), c(3,4,3), c(2,3,5), c(3,2,1))
m <- as.data.frame(m)                                                               set.seed(9999)
m <- cbind(m,sample(5:10,10,replace=T))  # add a user covar
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

