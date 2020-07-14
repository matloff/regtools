
\name{textToXY,textToXYpred}
\alias{textToXY}
\alias{textToXYpred}

\title{Tools for Text Classification}

\description{
  "R-style," classification-oriented wrappers for the \pkg{text2vec} package.
}

\usage{
    textToXY(docs,labels,kTop=50,stopWords='a') 
    textToXYpred(ttXYout,predDocs) 
}

\arguments{
  \item{docs}{Character vector, one element per document.}
  \item{preDocs}{Character vector, one element per document.}
  \item{labels}{Class labels, as numeric, character or factor.  NULL is
     used at the prediction stage.}
  \item{kTop}{The number of most-frequent words to retain; 0 means 
     retain all.}
  \item{stopWords}{Character vector of common words, e.g. prepositions
     to delete. Recommended is \code{tm::stopwords('english')}.
}

\details{

}

\value{

}

\examples{

}

\author{
Norm Matloff
}

