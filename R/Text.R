

# preparation for text classification; inputs text, label data; outputs 
# X matrix, Y vector

############################  ttXY()  ##################################

# wrapper for text2vec pkg

# arguments:

#   docs: character vector, one element per document
#   labels: R factor, class labels corresponding to docs
#   stopWords: character vector of stop words; suggest 
#      stopWords <- tm::stopwords('english')
#   kTop: number of most-frequent words to retain

ttXY <- function(docs,labels,kTop=50,stopWords=NULL) 
{
   require(text2vec)

   # prep
   x <- data.frame(docs,labels,id=1:length(docs))
   setDT(x)
   setkey(x,id)
   x <- x[J(x$id)]

   # compute vocab, doc term mat
   prep_fun = tolower
   tok_fun = word_tokenizer
   prep_fun = tolower
   tok_fun = word_tokenizer
   itx <- itoken(x$labels,
        preprocessor = prep_fun,
        tokenizer = tok_fun,
        ids = x$id,
        progressbar = FALSE)
   vocab <- create_vocabulary(itx)
   vectorizer <- vocab_vectorizer(vocab)
   dtm <- create_dtm(itx, vectorizer)
   
   # remove stop words
   vocab <- create_vocabulary(itx, stopwords = stop_words)
   vocab <- prune_vocabulary(vocab)
   dtm <- create_dtm(itx, vectorizer)
   list(x=dtm,y=labels)
}

##################  textToXY(), to be replaced  ########################

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
