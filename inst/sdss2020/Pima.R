
pimaExpVars <- function() 
{
   library(mlbench)
   data(PimaIndiansDiabetes2)
   diab <- PimaIndiansDiabetes2
   db <- diab[setdiff(names(diab),c('triceps','insulin'))]
   db <- db[complete.cases(db),]
   x <- as.matrix(db[,-7])
   y <- as.numeric(db[,7] == 'pos')
   plotExpVars(x,y,x,y,25,1:6,1.5,'probIncorrectClass',c(0.2,0.35),leave1out=TRUE)
}


