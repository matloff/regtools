
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

# labelPos: if src is a file or vector, index of the class label
# textPos: if src is a file or vector, index of the document text
# hdr: if src is a file, TRUE if file has a header
# kTop: retain records of only the kTop most-frequent words

textToXY <- function(src='dir',labelPos=NULL,textPos=NULL,hdr=TRUE,kTop=50) 
{
   if (src == 'dir') {
      cps <- Corpus(DirSource('.')) 
      labels <- as.factor(list.dirs(recursive=FALSE))
   } else if (src == 'file' || src == 'vector') {
      if (src == 'file') src <- read.csv(src,header=hdr)
      cps <- Corpus(VectorSource(src[,textPos]))
      labels <- as.factor(vec[,labelPos])
   }

   cps <- tm_map(cps,tolower)
   cps <- tm_map(cps,removePunctuation)
   cps <- tm_map(cps,removeNumbers)
   cps <- tm_map(cps,removeWords, stopwords("english"))
   cps <- lapply(cps,removeMorePunctuation)

   words <- paste(cps,collapse=' ')
   w1 <- strsplit(words,' ')[[1]]
   w1 <- w1[w1 != '']
   tw <- table(w1)

}

# !"#$%&'()*+, \-./:;<=>?@[\\\]^_{|}~` removed by removePunctuation(),
# according to Web
removeMorePunctuation <- function(oneCorpusElt) 
{
   
   tmp <- oneCorpusElt$content
   tmp <- gsub('-',' ',tmp)
   oneCorpusElt$content <- tmp
   oneCorpusElt
}

