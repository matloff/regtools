
# preparation for text classification; inputs text data; outputs overall
# document term matrix, labels factor etc.

# arguments:

# src:  character string, either a directory name, a file name or a
# character vector; cases

#    'dir': each subdirectory represents a class, with each file 
#    within a subdirectory representing one document from that class;
#    must be called from the target directory; no other subdirectories
#    are allowed
# 
#    'file': each record contains the text of one document; must be .csv
# 
#    'vector': each element contains the text of one document

# labels: if src is a file or vector, then vector or factor of class labels
# text: if src is a file or vector, then character string of document
#    tests, one element per document
# hdr: if src is a file, TRUE if file has a header
# kTop: retain records of only the kTop most-frequent words

textToXY <- function(src='dir',labelPos=NULL,textPos=NULL,hdr=TRUE,kTop=50) 
{
   if (src == 'dir') {
      cps <- Corpus(DirSource('.')) 
      labels <- as.factor(list.dirs(recursive=FALSE))
   } else if (src == 'file' || src == 'vector') {
      if (src == 'file') src <- read.csv(src,header=hdr)
      cps <- Corpus(VectorSource(src))
      labels <- as.factor(labels)
   }

   cps <- tm_map(cps,tolower)
   cps <- tm_map(cps,removePunctuation)
   cps <- tm_map(cps,removeNumbers)
   cps <- tm_map(cps,removeWords, stopwords("english"))
   # cps <- lapply(cps,removeMorePunctuation)

   words <- paste(cps,collapse=' ')
   words <- gsub('\n',' ',words)  
   words <- gsub('"',' ',words)
   words <- gsub(',',' ',words)

   w1 <- strsplit(words,' ')[[1]]
   w1 <- w1[w1 != '']  # caused by '  ' etc.
   tw <- table(w1)

}

# !"#$%&'()*+, \-./:;<=>?@[\\\]^_{|}~` removed by removePunctuation(),
# according to Web
removeMorePunctuation <- function(oneCorpusElt) 
{
   
   tmp <- oneCorpusElt
   tmp <- gsub('-',' ',tmp)
   oneCorpusElt$content <- tmp
   oneCorpusElt
}

