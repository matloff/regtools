
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
  \item{predDocs}{Character vector, one element per document.}
  \item{labels}{Class labels, as numeric, character or factor.  NULL is
     used at the prediction stage.}
  \item{kTop}{The number of most-frequent words to retain; 0 means 
     retain all.}
  \item{stopWords}{Character vector of common words, e.g. prepositions
     to delete. Recommended is \code{tm::stopwords('english')}.}
  \item{ttXYout}{Output object from \code{textToXY}.}
}

\details{

   A typical classification/machine learning package will have as arguments
   a feature matrix X and a labels vector/factor Y.  For a "bag of
   words" analysis in the text case, each row of X would be a document
   and each column a word.

   The functions here are basically wrappers for generating X.  Wrappers
   are convenient in that:

   \itemize{
      \item The \pkg{text2vec} package is rather arcane, so a "R-style" 
      wrapper would be useful.
      \item The \pkg{text2vec} are not directly set up to do
      classification, so the functions here provide the "glue" to do
      that.
   }

   The typical usage pattern is thus:

   \itemize{
      \item Run the documents vector and labels vector/factor through
      \code{textToXY}, generating X and Y.
      \item Apply your favorite classification/machine learning package
      p to X and Y, returning o.
      \item When predicting a new document d, run o and d through
      \code{textToXY}, producing x.
      \item Run x on p's \code{predict} function.
   }
}

\value{

   The function \code{textToXY} returns an R list with components
   \code{x} and \code{y} for X and Y, and a copy of the input
   \code{stopWords}.

   The function \code{textToXY} returns X.

}

\author{
Norm Matloff
}

