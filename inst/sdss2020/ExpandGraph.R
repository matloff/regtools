
# explore use of the ExpandVars arg

# arguments:
 
#    xtrn: vector or matrix for "X" portion of training data
#    ytrn: vector or matrix for "Y" portion of training data; matrix
#       case is for vector "Y", i.e. multiclass 
#    xtst,ytst: test data analogs of xtrn, ytrn
#    k: number of nearest neighbors
#    eVar: column number of the predictor to be expanded
#    maxEVal: maximum expansion 
#    lossFtn: loss function; internal offerings are 'MAPE' and 'propMisclass'
#    eValIncr: expansion value increment 

# value:

#    mean loss, evaluated from 0 to maxEVal, increments of eValIncr

exploreExpVars <- 
   function(xtrn,ytrn,xtst,ytst,k,eVar,maxEVal,loss,incr=0.05) 
{
   dfr <- data.frame(NULL,NULL)
   for (w in seq(0.05,1.5,eValIncr)) {
      preds <- kNN(xtrn,ytrn,xtst,k,expandVars=eVar,expandVals=w)
      dfr <- rbind(dfr,c(w,mean(loss(preds$regests,ytst)

      
      abs(preds$regests-ytst))))
   } 
   names(dfr) <- c('w',loss)
   frmla <- as.formula(paste0(loss, ' ~ w'))
   lwout <- loess(frmla,data=dfr) 
   lwout$fitted
}


# plot accuracy of applying one or more instances of the ExpandVars arg


