\name{factorsToDummies}
\alias{factorToDummies}
\alias{factorsToDummies}
\alias{dummiesToFactor}
\alias{charsToFactors}

\title{Factor Conversion Utilities}

\description{
Utilities from converting back and forth between factors and dummy
variables.
}

\usage{
factorToDummies(f,fname,omitLast=TRUE)
factorsToDummies(dfr,omitLast=TRUE)
dummiesToFactor(dms,inclLast=FALSE) 
charsToFactors(dtaf)
}

\details{
Many R users prefer to use R factors in their coding, or work with data
that is of this type to begin with.  On the other hand, many regression
packages, e.g. \pkg{lars}, disallow factors.  These utilities facilitate
conversion from one form to another.
}

\arguments{
  \item{f}{A factor.}
  \item{fname}{A factor name.}
  \item{dfr}{A data frame.}
  \item{omitLast}{If TRUE, then generate only k-1 dummies from k factor
     levels.}
  \item{dms}{A data frame whose columns are dummy variables.}
  \item{inclLast}{If FALSE, then only k-1 dummies for k factor
     levels are provided.}
  \item{dtaf}{A data frame.}
}

\value{The function \code{factorToDummies} returns a matrix of dummy
variables, while \code{factorsToDummies} returns a new version of the
input data frame, in which each factor is replaced by columns of
dummies.  The function \code{factorToDummies} is similar, but changes
character vectors to factors.
}

\examples{
f <- as.factor(c('abc','de','f','de','abc'))
factorToDummies(f,'f')
# outputs
#      f.abc f.de
# [1,]     1    0
# [2,]     0    1
# [3,]     0    0
# [4,]     0    1
# [5,]     1    0
d <- data.frame(a=1:5,b=f)
factorsToDummies(d,omitLast=FALSE)
# outputs
#   a b.abc b.de
# 1 1     1    0
# 2 2     0    1
# 3 3     0    0
# 4 4     0    1
# 5 5     1    0
d <- data.frame(x=c('abc','de','f'),y=1:3,z=c('I','am','lost'))
str(d)
# 'data.frame':	3 obs. of  3 variables:
#  $ x: chr  "abc" "de" "f"
#  $ y: int  1 2 3
#  $ z: chr  "I" "am" "lost"
d1 <- regtools:::charsToFactors(d)
str(d1)
# 'data.frame':	3 obs. of  3 variables:
#  $ x: Factor w/ 3 levels "abc","de","f": 1 2 3
#  $ y: int  1 2 3
#  $ z: Factor w/ 3 levels "am","I","lost": 2 1 3
}

\author{
Norm Matloff
}

