
# preparation for text classification; inputs text data; outputs overall
# document term matrix, labels factor etc.

# arguments:

# src:  character string, either a directory name or file name; in the
#    directory case, each subdirectory represents a class, with each file 
#    within a subdirectory representing one document from 
#    that class; in the file case, each record represents one 
#    document
# labelPos: if src is a file, position within line for the class label
# textPos: if src is a file, position within line for the document text
# hdr: if src is a file, TRUE if file has a header

textToXY <- function(src='.',labelPos=NULL,textPos=NULL,hdr=NULL) 
{
   dirCase <- file.info(src)$isdir
   if (!dirCase) {
      fl <- read.csv(src,header=hdr)
      labels <- fl[,labelPos]
      docs <- fl[,textPos]
   } else {
      dirs <- list.dirs(recursive=FALSE)
      tmp <- lapply(dirs,getDocs)
   }


}

getDocs <- function(dir) 
{
   currDir <- getwd()
   on.exit(setwd(currdir))
   list(label=dir)
}

