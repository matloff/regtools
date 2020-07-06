

# preparation for text classification; inputs text data; outputs overall
# document term matrix, labels factor etc.

# arguments:

# src:  character string, either a directory name or a
# character vector; cases

#    'dir': directory; each subdirectory represents a class, with each
#    file within a subdirectory representing one document from that
#    class; must be called from the target directory; no other
#    subdirectories are allowed

#    'vector': each element contains the text of one document

# labels: taken to the subdirectory names in the 'dir' case, otherwise
#    supplied by the caller

# labels: if src is a file or vector, then vector or factor of class labels
# text: if src is a file or vector, then character string of document
#    tests, one element per document
# hdr: if src is a file, TRUE if file has a header
# kTop: retain records of only the kTop most-frequent words

textToXY <- function(src='dir',labels=NULL,kTop=50) 
{
browser()
   if (inherits(src,'character')) {
      cps <- Corpus(DirSource('.')) 
      labels <- as.factor(list.dirs(recursive=FALSE))
   } else cps <- Corpus(VectorSource(src))
   words <- preProcessText(cps)
   tw <- table(words)
   two <- tw[order(tw,decreasing=FALSE)]
   freqWords <- names(two)[1:kTop]
}

# cps is a tm corpus, returned by e.g. Corpus(); punctuation is removed,
# etc.; return value is character vector  comprising a word list
preProcessText <- function(cps) 
{
   cps <- tm_map(cps,tolower)
   cps <- tm_map(cps,removePunctuation)
   cps <- tm_map(cps,removeNumbers)
   cps <- tm_map(cps,removeWords, stopwords("english"))
   words <- paste(cps,collapse=' ')
   strsplit(words,' ')[[1]]

}
