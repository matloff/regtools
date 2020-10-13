
# arguments

#    x: matrix of numeric features
#    y: factor or numeric vector
#    newx: matrix of numeric features in new cases
#    k: number of nearest neighbors
#    mahaThresh: a proportion

# each data point in newx will use as smoothing function loclin()
# instead of mean()

mixedPreds <- function(x,y,newx,k,mahaThresh,scaleX) 
{
   if (is.factor(y)) stop('classification case not yet implemented')
   
   if (scaleX) {
      x <- scale(x,center=TRUE,scale=TRUE)
      xcntr <- attr(x, "scaled:center")
      xscl <- attr(x, "scaled:scale")
      newx <- scale(newx, center = xcntr, scale = xscl)
   }

   # first get distribution of M-dist
   meanx <- mean(x)
   covx <- cov(x)
   mhdists <- mahalanobis(x, meanx,covx)
   outerThresh <- quantile(mhdists,1-mahaThresh)

   # which rows of newx are on the edge of the data?
   newxMhdists <- mahalanobis(newx,meanx,covx)
   outThere <- which(newxMhdists > outerThresh)
   # the rest
   mainstream <- setdiff(1:nrow(newx),outThere)

   # now predict
   predsMainstream <- kNN(x,y,newx[mainstream,],k,scaleX=FALSE)
   predsOutThere <- kNN(x,y,newx[outThere,],k,scaleX=FALSE,
      smoothingFtn=loclin)
   preds <- vector(length=nrow(newx))
   preds[mainstream] <- predsMainstream$regests
   preds[outThere] <- predsOutThere$regests

   list(preds=preds,mainstream=mainstream,outThere=outThere,
      predsMainstream=predsMainstream,predsOutThere=predsOutThere,
      k=k,scaleX=scaleX,outerThresh=outerThresh)
}

# experiments on the value of expanding predictor weights

# generate data, then fit both mean() and loclin(), at various levels of
# the Mahalanobis distance threshold; return vector of ratios of MAPE,
# loclin()/mean()

simLocLin <- function(n,p,k,catOut=FALSE,seed=9999) 
{
   x <- matrix(rexp(n*p),nrow=n)
   y <- rowSums(x)^2 + p*rnorm(n)
   newx <- matrix(rexp(n*p),nrow=n)
   newy <- rowSums(newx)^2 + p*rnorm(n)
   predsZM <- kNN(x,y,newx,50,scaleX=T)$regests
   res <- vector(length = 25)
   mhs <- seq(0.001,0.25,0.001)
   for (i in 1:length(mhs)) {
      mhprop <- mhs[i]
      zLL <- mixedPreds(x,y,newx,50,mhprop,T)
      predszLL <- zLL$preds
      mapeLoclin <- mean(abs(predszLL[zLL$outThere] - newy[zLL$outThere]))
      mapeMean <- mean(abs(predsZM[zLL$outThere] - newy[zLL$outThere]))
      if (catOut) cat(mhprop,mapeLoclin,mapeMean,'\n')
      res[i] <- mapeLoclin / mapeMean
   }
   res
}

doSim <- function() 
{
   res.5000.2.25 <<- simLocLin(5000,2,25,catOut=T)
   res.5000.2.100 <<- simLocLin(5000,2,100,catOut=T)
   res.5000.10.25 <<- simLocLin(5000,10,25,catOut=T)
   res.5000.10.100 <<- simLocLin(5000,10,100,catOut=T)
   res.5000.20.25 <<- simLocLin(5000,20,25,catOut=T)
   res.5000.20.100 <<- simLocLin(5000,20,100,catOut=T)
}

plottingSim <- function() 
{

   plot(seq(0.001,0.25,0.001),res.5000.2.25, type = "l",ylim=c(0,50),
      xlab='MH distance threshold',ylab='MAPE.loclin/mean')
   lines(seq(0.001,0.25,0.001),res.5000.2.100, type = "l")
   lines(seq(0.001,0.25,0.001),res.5000.10.25, type = "l")
   lines(seq(0.001,0.25,0.001),res.5000.10.100, type = "l")
   lines(seq(0.001,0.25,0.001),res.5000.20.25, type = "l")
   lines(seq(0.001,0.25,0.001),res.5000.20.100, type = "l")
   abline(h=1)
   
   # see which line is which
   print(res.5000.2.25[250])
   print(res.5000.2.100[250])
   print(res.5000.10.25[250])
   print(res.5000.10.100[250])
   print(res.5000.20.25[250])
   print(res.5000.20.100[250])
   
   text(0.212,1.234,labels='p=2,k=25')
   text(0.241,0.893,labels='p=2,k=100')
   text(0.199,0.289,labels='p=10')
   text(0.118,0.066,labels='p=20')

}


