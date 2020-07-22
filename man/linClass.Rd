\name{linClass,predict.linClass}

\title{Linear Methods for Classification}

\description{
Multiclass prediction using linear models.
}

\usage{
linClass(dta,yName=NULL)
\method{predict}{linClass}(object,...) 
}

\arguments{
\item{dta}{Training set data frame, containing features and class labels}
\item{yName}{Name of the class labels column}
\item{linClassObj}{Object of type returned by \code{linClass}}
\item{newx}{Data frame of X data to be predicted}
}

\details{

Alternative to logit for multiclass prediction, using purely linear
models.  Faster than \code{glm}, and may be as effective or better.

}

\value{

The \code{linClass} function returns an object of class \code{"linClass"}.
The corresponding \code{predict} returns a vector of class names, the
predicted classes.

}

\author{
Norm Matloff
}

