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
  \item{xy}{A data frame mentioned for prediction, "Y" in last column.}
  \item{dms}{Matrix of dummy variables corresponding to a categorical
     variable.}
  \item{inclLast}{If TRUE, \code{dms} is the complete set, k columns for
     a k-level categorical variable.}
  \item{saveLevels}{In collapsing a factor, which levels to retain.}
  \item{lumpedLevel}{Name of new level to be created from levels not retained.}
  \item{x}{A numeric vector, except in \code{hasFactors}, where it is a
     data frame.}
  \item{endpts}{Value to be used as \code{breaks} in call to \code{cut}.}
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
  \item{factorsInfo}{Attribute from output of \code{factorToDummies}.}
  \item{dms}{A data frame whose columns are dummy variables.}
  \item{inclLast}{If FALSE, then only k-1 dummies for k factor
     levels are provided.}
  \item{dtaf}{A data frame.}
}

\details{
Many R users prefer to use R factors in their coding, or work with data
that is of this type to begin with.  On the other hand, many regression
packages, e.g. \pkg{lars}, disallow factors.  These utilities facilitate
conversion from one form to another.

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

