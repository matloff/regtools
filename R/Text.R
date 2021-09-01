


############################  textToXY()  ##############################

# preparation for text classification; inputs text, label data; outputs 
# X matrix, Y vector

# arguments:

#   docs: character vector, one element per document
#   labels: R factor, class labels corresponding to docs
#   stopWords: character vector of stop words; suggest 
#      stopWords <- tm::stopwords('english')
#   kTop: number of most-frequent words to retain

textToXY <- function(docs,labels,kTop=50,stopWords='a') 
{
   # prep
   if (is.null(labels)) labels <- rep(NA,length(docs))
   id <- 1:length(docs)
   x <- data.frame(docs,labels,id=id)
   if (!is.character(x$docs)) x$docs <- as.character(x$docs)
   data.table::setDT(x)  # make data frame a by-reference data.table
   data.table::setkey(x,id)  # sort the table by id

   # compute vocab, doc term mat
   prep_fun <- tolower  # change letters to lower-case
   tok_fun <- text2vec::word_tokenizer  # break text into words
   itx <- text2vec::itoken(x$docs,
        preprocessor = prep_fun,
        tokenizer = tok_fun,
        ids = x$id,
        progressbar = FALSE)
   vocab <- text2vec::create_vocabulary(itx)
   vectorizer <- text2vec::vocab_vectorizer(vocab)
   dtm <- text2vec::create_dtm(itx, vectorizer)  # document-term matrix, one row per doc
   
   # remove stop words
   vocab <- text2vec::create_vocabulary(itx, stopwords = stopWords)
   prunedVocab <- text2vec::prune_vocabulary(vocab)
   vectorizer <- text2vec::vocab_vectorizer(prunedVocab)
   dtm <- text2vec::create_dtm(itx, vectorizer)  # new doc-term matrix

   nw <- ncol(dtm)
   if (kTop > 0) dtm <- dtm[,(nw-kTop+1):nw]
   dtm <- as.matrix(dtm)
   list(x=dtm,y=labels,stopWords=stopWords)
}

textToXYpred <- function(ttXYout,predDocs) 
{

   predX <- textToXY(predDocs,NULL,kTop=0,stopWords=ttXYout$stopWords)$x
   namesTrain <- colnames(ttXYout$x)
   namesTest <- colnames(predX)
   x <- matrix(0,nrow=length(predDocs),ncol=length(namesTrain))
   colnames(x) <- namesTrain
   for (word in namesTrain) 
      if (word %in% namesTest) x[,word] <- predX[,word]
   x
}

