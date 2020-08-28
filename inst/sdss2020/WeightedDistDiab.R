
library(regtools)
library(mlbench)
data(PimaIndiansDiabetes2)
diab <- PimaIndiansDiabetes2
db <- diab[setdiff(names(diab),c('triceps','insulin'))]
db <- db[complete.cases(db),]
head(db)
x <- as.matrix(db[,-7])
y <- as.numeric(db[,7] == 'pos')
set.seed(9999)
tstidxs <- sample(1:nrow(x),100)
xtst <- x[tstidxs,]
ytst <- y[tstidxs]
xtrn <- x[-tstidxs,]
ytrn <- y[-tstidxs]
plotExpVars(xtrn, ytrn, xtst,ytst,5,1:6,1.5,'propMisclass',c(0.5,0.9))


