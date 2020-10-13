
# apply regtools:::fineTuning() to choosing the expansion factors in
# kNN()

knnFineTune <- function(data,yName,k,expandVars,ws,classif=FALSE,
   seed=9999) 
{
stop('still testing')
   if (classif) stop('not ready for classification problems')
   ycol <- which(names(data) == yName)

   theCall <- function(dtrn,dtst,cmbi) {
      x <- dtrn[,-ycol]
      y <- dtrn[,ycol]
      newx <- dtst[,-ycol]
      newy <- dtst[,ycol]
      knnout <- kNN(x,y,newx,k,expandVars=expandVars,expandVals=cmbi)
      mean(abs(knnout$regests - newy))
   }

   fineTuning(dataset=data,pars=list(w1=ws,w2=ws),regCall=theCall)

}

