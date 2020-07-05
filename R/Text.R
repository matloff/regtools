
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

prepTextClass <- function(src='.',labelPos=NULL,textPos=NULL) 
{
   dirCase <- file.info(src)$isdir
}

