\name{factorsToDummies}
\alias{factorToDummies}
\alias{factorsToDummies}
\alias{dummiesToFactor}
\alias{charsToFactors}
\alias{factorTo012etc}
\alias{getDFclasses}
\alias{hasCharacters}
\alias{hasFactors}
\alias{toAllNumeric}
\alias{toSubFactor}
\alias{toSuperFactor}
\alias{toAllNumeric}
\alias{discretize}
\alias{dummiesToInt}
\alias{xyDataframeToMatrix}

\title{Factor Conversion Utilities}

\description{
Utilities from converting back and forth between factors and dummy
variables.
}

\usage{
xyDataframeToMatrix(xy)
dummiesToInt(dms,inclLast=FALSE)
factorToDummies(f,fname,omitLast=FALSE,factorInfo=NULL)
factorsToDummies(dfr,omitLast=FALSE,factorsInfo=NULL,dfOut=FALSE)
dummiesToFactor(dms,inclLast=FALSE) 
charsToFactors(dtaf)
factorTo012etc(f,earlierLevels = NULL)
discretize(x,endpts)
getDFclasses(dframe)
hasCharacters(dfr)
hasFactors(x)
toAllNumeric(w,factorsInfo=NULL)
toSubFactor(f,saveLevels,lumpedLevel="zzzOther")
toSuperFactor(inFactor,superLevels)
}

\arguments{
  \item{dfOut}{If TRUE, return a data frame, otherwise a matrix.}
  \item{dms}{Matrix or data frame of dummy columns.}
  \item{inclLast}{When forming a factor from dummies, include the last
     dummy as a level if this is TRUE.}
  \item{xy}{A data frame mentioned for prediction, "Y" in last column.}
  \item{saveLevels}{In collapsing a factor, which levels to retain.}
  \item{lumpedLevel}{Name of new level to be created from levels not retained.}
  \item{x}{A numeric vector, except in \code{hasFactors}, where it is a
     data frame.}
  \item{endpts}{Vector to be used as \code{breaks} in call to
     \code{cut}. To avoid NAs, range of the vector must cover the 
     range of the input vector.}
  \item{f}{A factor.}
  \item{inFactor}{Original factor, to be extended.}
  \item{superLevels}{New levels to be added to the original factor.}
  \item{earlierLevels}{Previous levels found for this factor.}
  \item{fname}{A factor name.}
  \item{dfr}{A data frame.}
  \item{w}{A data frame.}
  \item{dframe}{A data frame, for which we wish to find the column classes.}
  \item{omitLast}{If TRUE, then generate only k-1 dummies from k factor
     levels.}
  \item{factorsInfo}{Attribute from output of \code{factorsToDummies}.}
  \item{factorInfo}{Attribute from output of \code{factorToDummies}.}
  \item{dtaf}{A data frame.}
}

\details{

Many R users prefer to express categorical data as R factors, or often
work with data that is of this type to begin with.  On the other hand,
many regression packages, e.g. \pkg{lars}, disallow factors.  These
utilities facilitate conversion from one form to another.

Here is an overview of the roles of the various functions:

\itemize{

\item \code{factorToDummies}: Convert one factor to dummies, yielding a
matrix of dummies corresponding to that factor.

\item \code{factorsToDummies}: Convert all factors to dummies, yielding
a matrix of dummies, corresponding to all factors in the input data
frame.

\item \code{dummiesToFactor}: Convert a set of related dummies to a
factor.

\item \code{factorTo012etc}: Convert a factor to a numeric code,
starting at 0.

\item \code{dummiesToInt}: Convert a related set of dummies to a numeric code,
starting at 0.

\item \code{charsToFactors}:  Convert all character columns in a data
frame to factors.

\item \code{toAllNumeric}: Convert all factors in a data frame to
dummies, yielding a new version of the data frame, including its
original nonfactor columns.

\item \code{toSubFactor}: Coalesce some levels of a factor, yielding a
new factor.

\item \code{toSuperFactor}: Add levels to a factor. Typically used in
prediction contexts, in which a factor in a data point to be predicted
does not have all the levels of the same factor in the training set.

\\item \code{xyDataframeToMatrix}: Given a data frame to be used in
a training set, with "Y" a factor in the last column, change to all
numeric, with dummies in place of all "X" factors and in place of the
"Y" factor.

}

The optional argument \code{factorsInfo} is intended for use in prediction
contexts.  Typically a set of new cases will not have all levels of the
factor in the training set.  Without this argument, only an incomplete
set of dummies would be generated for the set of new cases.


A key point about changing factors to dummies is that, for later
prediction after fitting a model in our training set, one needs to use
the same transformations.  Say a factor has levels 'abc', 'de' and 'f'
(and \code{omitLast = FALSE}).  If we later have a set of say two new
cases to predict, and their values for this factor are 'de' and 'f', we
would generate dummies for them but not for 'abc', incompatible with the
three dummies used in the training set.

Thus the factor names and levels are saved in attributes, and can be
used as input:  The relations are as follows:

\itemize{

\item \code{factorsToDummies} calls \code{factorToDummies} on each
factor it finds in its input data frame

\item \code{factorToDummies} outputs and later inputs \code{factorsInfo}

\item \code{factorsToDummies} outputs and later inputs \code{factorsInfo}

}

Other functions:

\itemize{

\item \code{getDFclasses}: Return a vector of the classes of the columns
of a data frame.

\item \code{discretize}: Partition range of a vector into (not
necessarily equal-length) intervals, and construct a factor from the
labels of the intervals that the input elements fall into.

\item \code{hasCharacters, hasFactors}: Logical scalars, TRUE if the
input data frame has any character or factor columns.

}


}

\value{The function \code{factorToDummies} returns a matrix of dummy
variables, while \code{factorsToDummies} returns a new version of the
input data frame, in which each factor is replaced by columns of
dummies.  The function \code{factorToDummies} is similar, but changes
character vectors to factors.
}

\examples{
x <- factor(c('abc','de','f','de'))
xd <- factorToDummies(x,'x')  
xd 
#      x.abc x.de
# [1,]     1    0
# [2,]     0    1
# [3,]     0    0
# [4,]     0    1
# attr(,"factorInfo")
# attr(,"factorInfo")$fname
# [1] "x"
# 
# attr(,"factorInfo")$omitLast
# [1] TRUE
# 
# attr(,"factorInfo")$fullLvls
# [1] "abc" "de"  "f"  
w <- factor(c('de','abc','abc'))
wd <- factorToDummies(w,'x',factorInfo=attr(xd,'factorInfo')) 
wd 
#      x.abc x.de
# [1,]     0    1
# [2,]     1    0
# [3,]     1    0
# attr(,"factorInfo")
# attr(,"factorInfo")$fname
# [1] "x"
# 
# attr(,"factorInfo")$omitLast
# [1] TRUE
# 
# attr(,"factorInfo")$fullLvls
# [1] "abc" "de"  "f"  

}

\author{
Norm Matloff
}

