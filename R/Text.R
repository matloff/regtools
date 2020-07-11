


# preparation for text classification; inputs text, label data; outputs 
# X matrix, Y vector

############################  textToXY()  ##############################

# wrapper for text2vec pkg

# arguments:

#   docs: character vector, one element per document
#   labels: R factor, class labels corresponding to docs
#   stopWords: character vector of stop words; suggest 
#      stopWords <- tm::stopwords('english')
#   kTop: number of most-frequent words to retain

textToXY <- function(docs,labels,kTop=50,stopWords='a') 
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
   itx <- itoken(x$docs,
        preprocessor = prep_fun,
        tokenizer = tok_fun,
        ids = x$id,
        progressbar = FALSE)
   vocab <- create_vocabulary(itx)
   vectorizer <- vocab_vectorizer(vocab)
   dtm <- create_dtm(itx, vectorizer)
   
   # remove stop words
   vocab <- create_vocabulary(itx, stopwords = stopWords)
   vocab <- prune_vocabulary(vocab)
   dtm <- create_dtm(itx, vectorizer)
   nw <- ncol(dtm)
   if (kTop > 0) dtm <- dtm[,(nw-kTop+1):nw]
   dtm <- as.matrix(dtm)
   list(x=dtm,y=labels)
}

