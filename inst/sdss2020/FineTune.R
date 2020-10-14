
# apply regtools:::fineTuning() to choosing the expansion factors in
# kNN()

# arguments:

#    data: data frame (or matrix with col names), including both "X" and "Y"
#    yName: name of the data column containing "Y"
#    expandVars: indices of the column numbers 

knnFineTune <- function(data,yName,k,expandVars,ws,classif=FALSE,
   seed=9999) 
{
   if (classif) stop('not ready for classification problems')

   ycol <- which(names(data) == yName)
   # may need to shift some of expandVars over, once "Y" is removed
   if (ycol < ncol(data)) {
      topvars <- which(expandVars > ycol)
      if (length(topvars) > 0) {
         expandVars[topvars] <- expandVars[topvars] - 1
      }
   }
   expandNms <- colnames(data[,-ycol])[expandVars]

   theCall <- function(dtrn,dtst,cmbi) {
      x <- dtrn[,-ycol]
      y <- dtrn[,ycol]
      newx <- dtst[,-ycol]
      newy <- dtst[,ycol]
      cmbi <- as.numeric(cmbi)
      knnout <- kNN(x,y,newx,k,expandVars=expandVars,expandVals=cmbi)
      mean(abs(knnout$regests - newy))
   }
   
   # wcols <- paste0('w',1:length(expandVars),'=ws',collapse=',')
   # wcols <- paste0('list(',wcols,')')
   wcols <- paste0(expandNms,'=ws',collapse=',')
   wcols <- paste0('list(',wcols,')')
   fineTuning(dataset=data,pars=eval(parse(text=wcols)),regCall=theCall,
      nXval=10)

}

