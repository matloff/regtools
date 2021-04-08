
# multiclass models; see also th q*() functions

# deprecated:

ovalogtrn <- function(...) 
{
   stop('deprecated; use qLogit()')
}

predict.ovalog <- function(...) 
{
   stop('')
}

##################################################################

#  older AVA, OVA code

##################################################################
# avalogtrn: generate estimated regression functions
##################################################################

# arguments:

#    as in logitClass() above

# value:

#    as in logitClass() above

avalogtrn <- function(trnxy,yname) 
{
   if (is.null(colnames(trnxy))) 
      stop('trnxy must have column names')
   ycol <- which(names(trnxy) == yname)
   y <- trnxy[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   x <- trnxy[,-ycol,drop=FALSE]
   xd <- factorsToDummies(x,omitLast=TRUE)
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   m <- ncol(yd)
   n <- nrow(trnxy)
   classIDs <- apply(yd,1,which.max) - 1
   classcounts <- table(classIDs)
   ijs <- combn(m,2) 
   outmat <- matrix(nrow=ncol(xd)+1,ncol=ncol(ijs))
   colnames(outmat) <- rep(' ',ncol(ijs))
   attr(outmat,'Xcolnames') <- colnames(xd)
   doreg <- function(ij)  # does a regression for one pair of classes
   {
      i <- ij[1] - 1
      j <- ij[2] - 1
      tmp <- rep(-1,n)
      tmp[classIDs == i] <- 1
      tmp[classIDs == j] <- 0
      yij <- tmp[tmp != -1]
      xij <- xd[tmp != -1,]
      coef(glm(yij ~ xij,family=binomial))
   }
   for (k in 1:ncol(ijs)) {
      ij <- ijs[,k]
      outmat[,k] <- doreg(ij)
      colnames(outmat)[k] <- paste(ij,collapse=',')
   }
   if (any(is.na(outmat))) warning('some NA coefficients')
   empirClassProbs <- classcounts/sum(classcounts)
   attr(outmat,'empirClassProbs') <- empirClassProbs
   attr(outmat,'nclasses') <- m
   class(outmat) <- c('avalog','matrix')
   outmat
}

################################################################## 
# predict.avalog: predict Ys from new Xs
##################################################################

# arguments:  
# 
#    coefmat:  coefficient matrix, output from avalogtrn()
#    predx:  as above
# 
# value:
# 
#    vector of predicted Y values, in {0,1,...,m-1}, one element for
#    each row of predx

avalogpred <- function() stop('user predict.avalog()')
predict.avalog <- function(object,...) 
{
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   n <- nrow(predpts)
   predpts <- factorsToDummies(predpts,omitLast=TRUE)
   if (!identical(colnames(predpts),attr(object,'Xcolnames')))
      stop('column name mismatch between original, new X variables')
   ypred <- vector(length = n)
   m <- attr(object,'nclasses')
   for (r in 1:n) {
      # predict the rth new observation
      xrow <- c(1,predpts[r,])
      # wins[i] tells how many times class i-1 has won
      wins <- rep(0,m)  
      ijs <- combn(m,2)  # as in avalogtrn()
      for (k in 1:ncol(ijs)) {
         # i,j play the role of Class 1,0
         i <- ijs[1,k]  # class i-1
         j <- ijs[2,k]  # class j-1
         bhat <- object[,k]
         mhat <- logitftn(bhat %*% xrow)  # prob of Class i
         if (mhat >= 0.5) wins[i] <- wins[i] + 1 else wins[j] <- wins[j] + 1
      }
      pred[r] <- which.max(wins) - 1
   }
   ypred
}

# deprecated
avalogloom <- function(m,trnxy) stop('deprecated')
## ##################################################################
## # avalogloom: LOOM predict Ys from Xs
## ##################################################################
## 
## # arguments: as with avalogtrn()
## 
## # value: LOOM-estimated probability of correct classification\
## 
## avalogloom <- function(m,trnxy) {
##    n <- nrow(trnxy)
##    p <- ncol(trnxy) 
##    i <- 0
##    correctprobs <- replicate(n,
##       {
##          i <- i + 1
##          avout <- avalogtrn(m,trnxy[-i,])
##          predy <- avalogpred(m,avout,trnxy[-i,-p])
##          mean(predy == trnxy[-i,p])
##       })
##    mean(correctprobs)
## }

logitftn <- function(t) 1 / (1+exp(-t))

matrixtolist <- function (rc,m) 
{
   if (rc == 1) {
      Map(function(rownum) m[rownum, ], 1:nrow(m))
   }
   else Map(function(colnum) m[, colnum], 1:ncol(m))
}

# kNN for classification, more than 2 classes

# uses One-vs.-All approach

# ovaknntrn: generate estimated regression function values

# arguments

#    trnxy:  matrix or dataframe, training set; Y col is a factor 
#    yname:  name of the Y column
#    k:  number of nearest neighbors
#    xval:  if true, "leave 1 out," ie don't count a data point as its
#        own neighbor

# value:

#    xdata from input, plus new list components: 
#
#       regest: the matrix of estimated regression function values; 
#               the element in row i, column j, is the probability 
#               that Y = j given that X = row i in the X data, 
#               estimated from the training set
#       k: number of nearest neighbors
#       empirClassProbs: proportions of cases in classes 0,1,...,m

knntrn <- function() stop('use ovaknntrn')
ovaknntrn <- function(trnxy,yname,k,xval=FALSE)
{
   if (is.null(colnames(trnxy))) 
      stop('trnxy must have column names')
   ycol <- which(names(trnxy) == yname)
   y <- trnxy[,ycol]
   if (!is.factor(y)) stop('Y must be a factor')
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   m <- ncol(yd)
   x <- trnxy[,-ycol,drop=FALSE]
   xd <- factorsToDummies(x,omitLast=TRUE)
   xdata <- preprocessx(xd,k,xval=xval)
   empirClassProbs <- table(y) / sum(table(y))
   knnout <- knnest(yd,xdata,k)
   xdata$regest <- knnout$regest
   xdata$k <- k
   xdata$empirClassProbs <- empirClassProbs
   class(xdata) <- c('ovaknn')
   xdata
}

# predict.ovaknn: predict multiclass Ys from new Xs

# arguments:  
# 
#    object:  output of knntrn()
#    predpts:  matrix of X values at which prediction is to be done
# 
# value:
# 
#    object of class 'ovaknn', with components:
#
#       regest: estimated class probabilities at predpts
#       predy: predicted class labels for predpts    

predict.ovaknn <- function(object,...) {
   dts <- list(...)
   predpts <- dts$predpts
   if (is.null(predpts)) stop('predpts must be a named argument')
   # could get k too, but not used; we simply take the 1-nearest, as the
   # prep data averaged k-nearest
   x <- object$x
   if (!is.data.frame(predpts))
      stop('prediction points must be a data frame')
   predpts <- factorsToDummies(predpts,omitLast=TRUE)
   # need to scale predpts with the same values that had been used in
   # the training set
   ctr <- object$scaling[,1]
   scl <- object$scaling[,2]
   predpts <- scale(predpts,center=ctr,scale=scl)
   tmp <- FNN::get.knnx(x,predpts,1)
   idx <- tmp$nn.index
   regest <- object$regest[idx,,drop=FALSE]
   predy <- apply(regest,1,which.max) - 1
   attr(predy,'probs') <- regest
   predy
}

# adjust a vector of estimated condit. class probabilities to reflect
# actual uncondit. class probabilities

# NOTE:  the below assumes just 2 classes, class 1 and class 0; however,
# it applies to the general m-class case, because P(Y = i | X = t) can
# be viewed as 1 - P(Y != i | X = t), i.e. class i vs. all others

# arguments:
 
#    econdprobs: estimated conditional probs. for class 1, for various t, 
#       reported by the ML alg.
#    wrongprob1: proportion of class 1 in the training data; returned as
#       attr() in, e.g. logitClass()
#    trueprob1: true proportion of class 1 
 
# value:
 
#     adjusted versions of econdprobs, estimated conditional class
#     probabilities for predicted cases

# why: say we wish to predict Y = 1,0 given X = t 
# P(Y=1 | X=t) = pf_1(t) / [pf_1(t) + (1-p)f_0(t)]
# where p = P(Y = 1); and f_i is the density of X within class i; so
# P(Y=1 | X=t) = 1 / [1 + {f_0(t)/f_(t)} (1-p)/p]
# and thus
# f_0(t)/f_1(t) = [1/P(Y=1 | X=t) - 1] p/(1-p)

# let q the actual sample proportion in class 1 (whether by sampling
# design or from a post-sampling artificial balancing of the classes);
# the ML algorith has, directly or indirectly, taken p to be q; so
# substitute and work back to the correct P(Y=1 | X=t)

# WARNING: adjusted probabilities may be larger than 1.0; they can be
# truncated, or in a multiclass setting compared (which class has the
# highest untruncated probability?)

classadjust <- function(econdprobs,wrongprob1,trueprob1) {
   wrongratio <- (1-wrongprob1) / wrongprob1
   fratios <- (1 / econdprobs - 1) * (1 / wrongratio)
   trueratios <- (1-trueprob1) / trueprob1
   1 / (1 + trueratios * fratios)
}

#######################  boundaryplot()  ################################

# for binary Y setting, drawing boundary between predicted Y = 1 and
# predicted Y = 0, as determined by the argument regests

# boundary is taken to be b(t) = P(Y = 1 | X = t) = 0.5

# purpose: visually assess goodness of fit, typically running this
# function twice, one for glm() then for say kNN() or e1071::svm(); if
# there is much discrepancy and the analyst wishes to still use glm(),
# he/she may wish to add polynomial terms or use the polyreg package

# arguments:

#   y01,x: Y vector (1s and 0s), X matrix or numerical data frame 
#   regests: estimated regression function values
#   pairs: matrix of predictor pairs to be plotted, one pair per column
#   cex: plotting symbol size
#   band: max distance from 0.5 for a point to be included in the contour 

boundaryplot <- function(y01,x,regests,pairs=combn(ncol(x),2),
   pchvals=2+y01,cex=0.5,band=0.10) 
{
   # e.g. fitted.values output of glm() may be shorter than an X column,
   # due to na.omit default
   if(length(regests) != length(y01))
      stop('y01 and regests of different lengths')

   # need X numeric for plotting
   if (is.data.frame(x)) {
      if (hasFactors(x)) stop('x must be nrongkuiumeric')
      x <- as.matrix(x)
      if (mode(x) != 'numeric') stop('x has character data')
   }

   # Y must be 0s,1s or equiv
   if (length(table(y01)) != 2) stop('y01 must have only 2 values')
   if (is.factor(y01)) y01 <- as.numeric(y01) - 1
   
   p <- ncol(x)
   for (m in 1:ncol(pairs)) {

      i <- pairs[1,m]
      j <- pairs[2,m]
      x2 <- x[,c(i,j)]

      # plot X points, symbols for Y
      plot(x2,pch=pchvals,cex=cex)  

      # add contour
      near05 <- which(abs(regests - 0.5) < band)
      points(x2[near05,],pch=21,cex=1.5*cex,bg='red')
      if (m < ncol(pairs)) readline("next plot")
   }
}

#########################  confusion matrix  ################################

# generates the confusion matrix

# for an m-class problem, 'pred' are the predicted class IDs,
# taking values in 1,2,...,m; if 'actual' is numeric, then the same
# condition holds, but 'actual' can be a factor, which when
# "numericized" uses the same ID scheme (must be consistent with
# 'actual')

confusion <- function(actual,pred) {
   # if (is.factor(actual)) actual <- as.numeric(actual) 
   table(actual,pred)
}

#######################################################################
#######################  calibration  #################################
#######################################################################

# the *Calib() functions generate estimated conditional class
# probabilities from scores, e.g.  SVM decision values

# useful for classification methodology that does not inherently
# generate those probabilities, or for which those probabilities are
# biased or otherwise inaccurate

#########################  knnCalib()  ################################

# for a given new case, the k nearest neighbors in the training set are
# obtained; the class labels for the neighbors are then obtained, and
# the resulting proportions for the different labels are then used as
# the estimated probabilities for the binary class. 

# arguments:

#    y (factor): R factor of labels in training set
#    trnScores (matrix): vector/matrix of scores in training set
#    tstScores (matrix): scores of new case(s)
#    k (int): number of nearest neighbors
#    loclin (boolean): indicate whether local linear regression should be applied
#    smoothingFtn (function): a smoothing function whether it should be applied.

# value: vector of estimated probabilities for the new cases

knnCalib <- function(y,trnScores,tstScores,k, loclin=FALSE, scaleX=NULL, smoothingFtn=NULL) 
{
   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   if (is.vector(tstScores))
      tstScores <- matrix(tstScores,ncol=1)
   if (length(levels(y)) < 2) stop("Y should have at least 2 levels.")

   if(loclin){
      # loclin case
         # turn multcase into 0 and 1 for lm()
         classes <- levels(y)
         probMat <- matrix(NA, nrow(tstScores), length(classes))
         count <- 1

         for(class in classes){

            tmp_y <- ifelse(y == class, 1, 0)

            dat <- as.data.frame(cbind(tmp_y, trnScores))

            knnout <- qeKNN(dat, colnames(dat)[1], k=k, scaleX=scaleX,
               smoothingFtn=smoothingFtn,
               holdout=NULL)

            tstS  <- as.data.frame(tstScores)
            # ensure the test set has the same column names
            colnames(tstS) <- colnames(dat)[2:ncol(trnScores)]

            # knn model prediction
            pred <- predict(knnout, tstS)

             # set all values that are greater than 1 to 1
            pred[pred > 1] <- 1
            # set all values that are lower than 0 to 0
            pred[pred < 0] <- 0

            pred <- as.vector(pred)
            probMat[, count] <- pred
            count <- count + 1
         }
     
   }else{

      # mean case instead of loclin
      original_class <- levels(y)

      dat <- as.data.frame(cbind(trnScores, y))

      yname <- colnames(dat)[ncol(dat)]
      
      dat[[yname]] <- as.factor(dat[[yname]])
      
      levels(dat[[yname]]) <- original_class 

      knnout <- qeKNN(dat, yname, k=k, scaleX=scaleX, holdout=NULL)

      tstS  <- as.data.frame(tstScores)

      out <- predict(knnout, tstS)
      probMat <- out$probs
   }
   probMat
}

scoresToProbs <- knnCalib




#########################   ovaCalib() ###################################
# a wrapper to implement calibration methods via one-vs-all approach

# arguments:

   # trnY(factor): the training labels 
   # trnScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the training sample
   # tstScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the testing sample

   # calibMethod(str): a string that indicate which calibration method to use. 
   #                    options: "knnCalib", 
   #                             "locLinknnCalib", 
   #                             "plattCalib",
   #                             "isoCalib",
   #                             "BBQCalib",
   #                             "ELiTECalib",
   # deg (int): an intege to indicate the degree of polynomial for platt scaling
   # K (int): an integer to indicate the number of nearest neighbor
   # scaleX (boolean): to determine whether we should standardize the data before using k-NN
   # smoothingFtn (function): a smoothing function whether it should be applied.
   # isProb (boolean): indicate whether the scores are probabilities
   # fitAllX (boolean): to determine whether you want to use all scores 
   #                    for fitting algorithms that are not knn-based.

# value: matrix of estimated probabilities for the new cases

ovaCalib <- function(trnY,
   trnScores, 
   tstScores,
   calibMethod,
   deg=NULL,
   se=FALSE,
   K= NULL,
   scaleX = NULL,
   smoothingFtn=NULL,
   isProb = FALSE,
   fitAllX = FALSE)
{
   if (!is.factor(trnY)) stop('Y must be an R factor')

   if (nrow(trnScores) != length(trnY)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }

   if (is.vector(trnScores)){
      trnScores <- matrix(trnScores,ncol=1)
   }

   if (is.vector(tstScores)){
      tstScores <- matrix(tstScores,ncol=1)
   }

   # Get all the levels of y
   classes <- levels(trnY)

   # Compute posterior probabilities matrix probMat 
   if(calibMethod == 'knnCalib'){
      if (is.null(K)) stop('k needs to be provided for k-NN')

         probMat <- knnCalib(trnY, trnScores, tstScores, k=K, scaleX=scaleX) 

   }else if(calibMethod == 'locLinknnCalib'){
         probMat <- knnCalib(trnY, trnScores, tstScores, k=K, loclin=TRUE, 
            scaleX=scaleX, smoothingFtn=smoothingFtn) 
      
   }else{
      # create a empty matrix for storing probabilities
      probMat <- matrix(NA, nrow(tstScores), length(classes))

      if(se){
         # create a empty matrix for storing standard errors
         seMat <-matrix(NA, ncol(tstScores), length(classes))
      }

      # Using all covariates
      if(fitAllX){
         for(i in 1:length(classes)){
            # select the class1
            class1 <- classes[i]
            if(calibMethod == 'plattCalib') {
               # create 1's and 0's for OVA 
               y <- as.factor(ifelse(trnY == class1, 1, 0))
               if(se){
                  result <- plattCalib(y, trnScores, tstScores, deg, se=se)
                  prob <- result$probs
                  seMat[,i] <- result$SE
               }else{
                  prob <- plattCalib(y, trnScores, tstScores, deg, se=se)
               }

            } else if (calibMethod == 'isoCalib') {
               # this package CORElearn uses y as 1 and 2
               y <- as.factor(ifelse(trnY == class1, 1, 2))
               prob <- isoCalib(y, trnScores, tstScores, isProb)

            } else if (calibMethod == 'BBQCalib') {
               # create 1's and 0's for OVA 
               y <- ifelse(trnY == class1, 1, 0)
               # the paper suggests that However, model averaging 
               # is typically superior to model selection (Hoeting et al. 1999)
               # so we use option = 1 for predict_BBQ
               prob <- bbqCalib(y, trnScores, tstScores, option = 1)

            } else if (calibMethod == 'ELiTECalib') {
               # create 1's and 0's for OVA 
               y <- ifelse(trnY == class1, 1, 0)

               prob <- eliteCalib(y, trnScores, tstScores)

            } else stop('invalid calibration method')

            probMat[,i] <- prob
         }

      # Use 1 class of scores at a time
      }else{

            for(i in 1:length(classes)){
               # select the class1
               class1 <- classes[i]
               trnDV <- trnScores[,i]
               tstDV <-  tstScores[,i]

               # Compute posterior probabilities vector prob for class i
               if(calibMethod == 'plattCalib') {

                  # create 1's and 0's for OVA 
                  y <- as.factor(ifelse(trnY == class1, 1, 0))
                  if(se){

                     result <- plattCalib(y, trnDV, tstDV, deg, se=se)
                     prob <- result$probs
                     seMat[,i] <- result$SE

                  }else{
                     prob <- plattCalib(y, trnDV, tstDV, deg, se=se)

                  }

               } else if (calibMethod == 'isoCalib') {

                  # this package CORElearn uses y as 1 and 2
                  y <- as.factor(ifelse(trnY == class1, 1, 2))

                  prob <- isoCalib(y, trnDV, tstDV, isProb)

               # GUESS can only take values within [0,1] as the scores
               # } else if (calibMethod == 'guessCalib'){

               #    y <- ifelse(trnY == class1, 1, 0)

               #    prob <- guessCalib(y, trnDV, tstDV)

               } else if (calibMethod == 'BBQCalib') {

                  # create 1's and 0's for OVA 
                  y <- ifelse(trnY == class1, 1, 0)
                  # the paper suggests that However, model averaging 
                  # is typically superior to model selection (Hoeting et al. 1999)
                  # so we use option = 1 for predict_BBQ
                  prob <- bbqCalib(y, trnDV, tstDV, option = 1)

               } else if (calibMethod == 'ELiTECalib') {

                  # create 1's and 0's for OVA 
                  y <- ifelse(trnY == class1, 1, 0)

                  prob <- eliteCalib(y, trnDV, tstDV)

               } else stop('invalid calibration method')

               probMat[,i] <- prob
            }
      }
      # Simple normalization
      probMat <- t(apply(probMat, 1 ,function(x) x/sum(x, na.rm=TRUE)))
   }

   if(se){
      list(probMat= probMat, seMat= seMat)
   }else{
      # Return the probability matrix
      probMat
   }
}



#########################   avaCalib() ###################################
# a wrapper to implement calibration methods via all-vs-all approach
# this is experimental, not fully tested!

# arguments:

   # trnY(factor): the training labels 
   # trnScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the training sample
   # tstScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the testing sample

   # calibMethod(str): a string that indicate which calibration method to use. 
   #                    options: "knnCalib", 
   #                             "locLinknnCalib", 
   #                             "plattCalib",
   #                             "isoCalib",
   #                             "BBQCalib",
   #                             "ELiTECalib",
   # deg (int): an integer to indicate the degree of polynomial for platt scaling
   # K (int): an integer to indicate the number of nearest neighbor
   # scaleX (boolean): to determine whether we should standardize the data before using k-NN
   # smoothingFtn (function): a smoothing function whether it should be applied.
   # isProb (boolean): indicate whether the scores are probabilities

# value: matrix of estimated probabilities for the new cases

avaCalib <- function(trnY,
   trnScores, 
   tstScores,
   calibMethod,
   deg=NULL,
   K= NULL,
   se = FALSE, 
   isProb = NULL)
{
   if (!is.factor(trnY)) stop('Y must be an R factor')

   if (nrow(trnScores) != length(trnY)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }

   if (is.vector(trnScores)){
      trnScores <- matrix(trnScores,ncol=1)
   }

   if (calibMethod == 'knnCalib') {

      if (is.null(K)) stop('k needs to be provided for k-NN')

      probMat <- knnCalib(trnY,trnScores,tstScores, K)

   } else {
      # Get all the levels of y
      classes <- levels(trnY)
      # AVA case
      probMat <- matrix(NA, nrow(tstScores), choose(length(classes), 2))

      if(se){
         seMat <- matrix(NA, ncol(tstScores), choose(length(classes), 2))
      }
      counter <- 1
      for(i in 1: (length(classes)-1)){
         # Get the class 1 
         class1 <- classes[i]
         for(k in (i+1):length(classes)){
            # Get the class 2
            class2 <- classes[k]

            # Get all corresponding row indicies
            rowIdx <- which(trnY==class1 | trnY== class2)
            
            # Filter train scores matrix
            trnDV <- trnScores[rowIdx,counter]

            tstDV  <- tstScores[,counter]

         
            # we only use the corresponding column
            # for training, assuming the columns follows the order
            # class1vsclass2, class1vsclass3...class2vsclass3...

            if (calibMethod == 'plattCalib') {

               if (is.null(deg)) stop('Deg needs to be provided for platt')

               # create 1's and 0's for OVA 
               y  <- as.factor(ifelse(trnY[rowIdx]==class1, 1, 0))

               if(se){

                  result <- plattCalib(y, trnDV, tstDV, deg, se=se)
                  prob <- result$probs
                  seMat[,i] <- result$SE

               }else{
                  prob <- plattCalib(y, trnDV, tstDV, deg, se=se)
               }

            } else if (calibMethod == 'isoCalib') {

               y <- ifelse(trnY[rowIdx]==class1, 1, 0)

               prob <- isoCalib(y, trnDV, tstDV, isProb)

            } else if (calibMethod == 'BBQCalib') {

               # create 1's and 0's for OVA 
               y <- ifelse(trnY[rowIdx]==class1, 1, 0)
               # the paper suggests that However, model averaging 
               # is typically superior to model selection (Hoeting et al. 1999)
               # so we use option = 1 for predict_BBQ
               prob <- bbqCalib(y, trnDV, tstDV, option = 1)

            } else if (calibMethod == 'ELiTECalib') {

               # create 1's and 0's for OVA 
               y <- ifelse(trnY[rowIdx]==class1, 1, 0)

               prob <- eliteCalib(y, trnDV, tstDV)

            } else stop('invalid calibration method')

            # Store the probability in a matrix
            probMat[,counter] <- prob
            counter <- counter + 1
         }
      }
      # Simple normalization
      probMat <- t(apply(probMat, 1 ,function(x) x/sum(x, na.rm=TRUE)))
   } 
    if(se){
      list(probMat= probMat, seMat= seMat)
   }else{
      # Return the probability matrix
      probMat
   }
}




#########################  fitPlatt()  ################################

# run the logit once and save, rather doing running repeatedly, each
# time we have new predictions to make

# arguments:

#    y (factor): the training labels
#    trnScores(matrix): a vector of decision values or probabilities 
#                       given by the classifier for the training sample
#    degree(int): an integer to indicate the degree of polynomial for the logistic model

# value: a logistic model

fitPlatt <- function(y,trnScores,deg) 
{
   if (!is.factor(y)) stop('Y must be an R factor')
   if (is.vector(trnScores))
      trnScores <- matrix(trnScores,ncol=1)
   tsDF <- as.data.frame(trnScores)
   if (nrow(trnScores) != length(y)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }
   dta <- cbind(y,tsDF)
   res <- qePolyLog(dta,'y',deg=deg,maxInteractDeg=0,holdout=NULL)
}

ppc <- fitPlatt

#########################  predictPlatt()  ################################

# Taking the logistic model and predict on the test scores given the classifier

# arguments:

#    prePlattCalibOut (object): the logisitc model produced by fitPlatt()
#    tstScores(vector): a vector of decision values or probabilities given 
#                       by the classifier for the testing sample
#    se(boolean): to determine whether we want to compute 
#                 the standard error through the delta method

# values:
#        a list that contains the following
#           1. prob: a vector of probabilities
#           2. se: a vector of standard errors if se is set to TRUE

predictPlatt <- function(prePlattCalibOut,tstScores,se=FALSE) 
{
   if (is.vector(tstScores)) {
      tstScores <- matrix(tstScores,ncol=1)
   }
   tsDF <- as.data.frame(tstScores)
   probs <- predict(prePlattCalibOut,tsDF)$probs
   if(se) {
      require("RcmdrMisc")
      SEs = list()
      for (i in 1:length(levels(as.factor(prePlattCalibOut$y))))
      {
         model = prePlattCalibOut$glmOuts[[i]]
         nscores = ncol(tstScores)
         
         alg = "1/(1+exp((-1)*(b0"
         for (j in 1:nscores) {
            if (j != nscores){
               alg = paste(alg,"+b",j,"*",colnames(tstScores)[j], sep = "")
            }
            else{
               alg = paste(alg,"+b",j,colnames(tstScores)[j], sep = "")
            }
         }
         alg = paste(alg,")))", sep = "")
         SE = DeltaMethod(model,alg)$test$SE
         SEs[[i]] = SE
      }
      df.SEs = do.call(rbind, SEs)
      return(list(probs = probs, se = df.SEs))
   } else {
      return(list(probs = probs))
   }
}

#########################  plattCalib()  ################################
# apply ploynomial platt scaling to the posterior probabilities or decision values 
# from SVM or other algorithms

# arguments:

   # trnY(factor): the training labels 
   # trnScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the training sample
   # tstScores(matrix): a matrix of decision values or probabilities given by the classifier
                        # for the test sample
   # deg (int): an integer to indicate the degree of polynomial for platt scaling
   # se (bool): to determine whether standard errors should be computed via delta method

# values:
#        a list that contains the following
#           1. prob: a vector of probabilities
#           2. se: a vector of standard errors if se is set to TRUE

plattCalib <- function(trnY,
   trnScores, 
   tstScores,
   deg,
   se=FALSE){ 

   if(length(levels(trnY)) > 2){
      stop("For multi-class case, please use ovaCalib()")
   }

   plattMod <- fitPlatt(trnY, trnScores, deg=deg)
   pred <- predictPlatt(plattMod, tstScores, se=se) 
   if(se){
      list(probs = pred$probs[,2], SE = pred$se)
   }else{
      pred$probs[,2]
   }
}   


# isotonic regression, AVA

# y, trnScores, newScores, value as above

ExperimentalisoCalib <- function(y,trnScores,newScores)
{
stop('under construction')
   require(Iso)
   # find estimated regression function of yy on xx, at the values
   # newxx
   predictISO <- function(xx,yy,newxx)  # xx, yy, newxx numeric vectors
   {
      xo <- order(xx)
      xs <- xx[xo]  # sorted xx
      ys <- yy[xo]  # yy sorted according to xx
      newxxs <- matrix(newxx[xo],ncol=1)  # same for newxx
      isoout <- pava(ys)
      # get est. reg. ftn. value for each newxx; could interpolate for
      # improved accuracy, but good enough here
      minspots <- apply(newxxs,1,
         function(newxxsi) which.min(abs(newxxsi - xs)))
      isoout[minspots]
   }
   yn <- as.numeric(y)
   do1Pair <- function(ij) 
   {
      # require Iso
      # get pair
      i <- ij[1]
      j <- ij[2]
      # which has y = i or j?
      idxs <- which(yn == i | yn == j)
      # form subsets
      ys <- yn[idxs] - 1  # yn is 1s and 2s
      trnscores <- trnScores[idxs]
      # return probs for this pair
      predictISO(trnscores,ys,newScores)
   }
   pairs <- combn(length(levels(y)),2)
   apply(pairs,2,do1Pair)
}


#########################  isoCalib()  ################################

# wrapper calibrate either training scores or probability by 
# isotonic regression in binary case

# arguments
#   trnY: the training labels
#   trnScores (vector): the training scores output by the classifier
#   tstScores (vector): the test scores
#   isProb (boolean): a boolean value to determine whether scores are probabilities


isoCalib <- function(trnY,trnScores,tstScores, isProb)
{  
   require(CORElearn)

   # note this package uses PAV-based isotonic regression
   if (is.vector(trnScores)){
      trnScores <- matrix(trnScores,ncol=1)
   }

   if (nrow(trnScores) != length(trnY)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }

   if (length(levels(trnY)) != 2){
      stop('This function can only handle binary case. For multiclass case, please use ovaCalib()')
   }

   # change the name of the levels for calibrate()
   levels(trnY) <- c("1","2")

   calibration <- CORElearn::calibrate(trnY, trnScores, class1=1,
      method="isoReg", assumeProbabilities=isProb)
   # apply the calibration to the testing set
   calibratedProbs <- CORElearn::applyCalibration(tstScores, calibration)
   calibratedProbs
}

#########################  bbqCalib()  ################################

# wrapper calibrate either training scores or probability by 
# Bayesian Binning 
# reference: https://cran.r-project.org/web/packages/CalibratR/CalibratR.pdf

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    tstScores: scores of new case(s)
#    option either 1 or 0; averaging=1, selecting=0

# values:
#        prob: a vector of probabilities

bbqCalib <- function(trnY,trnScores, tstScores, option=1)
{
   require(CalibratR)

   bbqmod <-  CalibratR:::build_BBQ(trnY, trnScores)
   pred <- CalibratR:::predict_BBQ(bbqmod, tstScores, option)
   prob <- pred$predictions
   prob
}

#########################  guessCalib()  ################################

# wrapper calibrate either training scores or probability by 
# a GUESS calibration model 
# reference: https://cran.r-project.org/web/packages/CalibratR/CalibratR.pdf

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    tstScores: scores of new case(s)


# guessCalib <- function(trnY,trnScores, tstScores)
# {
#    require(CalibratR)

#    GUESSmod <-  CalibratR:::build_GUESS(trnY,trnScores)
#    pred <- CalibratR:::predict_GUESS(GUESSmod, tstScores)
#    prob  <- pred$predictions
#    return(prob)

# }



#########################  eliteCalib()  ################################

# wrapper calibrate by ELiTe
# author: kenneth
# arguments

#    y: vector of corresponding true class. 
#        1 indicates positive class and 0 indicates negative class.
#    trnScores: vector of uncalibrated decisions scores for training
#    tstScore: vector of uncalibratd decisions scores for testing
#    build_opt: 'AIC', or 'AICc' scoring functions (the Elite paper uses AICc).
#    pred_opt:  set it to 1  for running model averaging, 
#                or to 0 for running model selection

# values:
#        prob: a vector of probabilities

eliteCalib <- function(trnY,trnScores, tstScores, build_opt = "AICc", pred_opt=1)
{
   #require(devtools)
   #install_github("statsmaths/glmgen", subdir="R_pkg/glmgen")
   require(glmgen)
   #follow instruction on 
   # https://github.com/pakdaman/calibration/tree/master/ELiTE/R
   # to install EliTE
   require(ELiTE) 

   if (is.vector(trnScores)){
      trnScores <- matrix(trnScores,ncol=1)
   }

   
   if (nrow(trnScores) != length(trnY)) {
      stop('fewer scores than Ys; did you have nonnull holdout?')
   }

   # use the same function parameter as the elite paper
   eliteMod <- elite.build(trnScores, trnY, build_opt)
   prob <- elite.predict(eliteMod, tstScores, pred_opt)
   prob
}




#########################  getCalibMeasures()  ################################

# wrapper of EliTe error measure for calibration
# arguments
#     y : vector of true class of instances {0,1} 
#     scores : vector of predictions (classification scores) which is in the interval [0, 1]

# values:
#     df: a dataframe that shows RMSE, AUROC, ACC, MCE, ECE 

getCalibMeasures <- function(y, scores){

  require(glmgen)
  require(ELiTE) 
  require(PRROC)
  
  df <- as.data.frame(elite.getMeasures(scores, y))
  
  # change the colname names so that we state AUROC clearly
  colnames(df) <- c("RMSE","AUROC","ACC","MCE","ECE")
  
  # Compute the area under precision and recall curve
  tmp_df <- data.frame(predictions = scores, labels = y)
  pr <- PRROC::pr.curve(scores.class0=tmp_df[tmp_df$labels=="1",]$predictions,
                 scores.class1=tmp_df[tmp_df$labels=="0",]$predictions,
                 curve=F)
  # Add to the dataframe
  df$AUPRC  <- pr$auc.integral
  
  df
}



#########################  combineMeasures()  ################################

# the function extends getCalibMeasures to handle multi-class case when
# the test label has more than 2 classes. It also combines results
# from combineMeasures() for comparing different algorithms by passing the
# output of combinMeasures() to the argument called "prev_result" in the
# function 

# arguments
# y_test (vector): vector of true class of instances
# algorithm (string): give the name of the algorithm you used e.g. "Platt1"
# probMat : a matrix with each row being a probability distribution of the classes 
#           from the test data
# prev_result: pass dataframe returned by combineMeasures() to combine several
#              results into one dataframe if any

# values:
#  df (dataframe): a dataframe that lists all metrics for the given test labels
#                 and the calibrated probability matrix 

combineMeasures <- function(y_test, algorithm, probMat, prev_result=NULL){
   
   if (!is.matrix(probMat)) {
    stop('probMat must be a matrix')
 }
  count <- 1
  ls_result <- list()
  for(l in levels(y_test)){
    tmp <- ifelse(y_test==l, 1, 0)
    res <- getCalibMeasures(tmp, probMat[,count])
    res$class <- l
    res$Algorithm <- algorithm
    ls_result[[count]] <- res
    count <- count + 1
  }
  ls_result <- do.call("rbind", ls_result)
  if(!is.null(prev_result)){
    out <- rbind(prev_result, ls_result)
    return(out)
  }else{
    return(ls_result)
  }
}

####################  calibWrap() and preCalibWrap()  #############################

# calibWrap() is a wrapper; calibrate model in the training set, apply to test data

# preCalibWrap() can be used on the orginal dataset, to set the holdout
# set, run the model etc.

preCalibWrap <- function(dta,yName,qeFtn='qeSVM',qeArgs=NULL,holdout=500)
{
   qecall <- paste0('qeout <- ',qeFtn,'(dta,"',yName,'",',qeArgs,',
      holdout=',holdout,')') 
   eval(parse(text=qecall))

   tmp <- substitute({
   tstIdxs <- qeout$holdIdxs
   trnIdxs <- setdiff(1:nrow(dta),tstIdxs)
   ycol <- which(names(dta) == yName)
   trnX <- dta[trnIdxs,-ycol]
   trnY <- dta[trnIdxs,ycol]
   tstX <- dta[tstIdxs,-ycol]
   tstY <- dta[tstIdxs,ycol]

   if (qeFtn == 'qeSVM') {

      trnScores <- qeout$decision.values
      tstScores <- getDValsE1071(qeout,tstX)
      trnScores <- makeANDconjunction(trnScores,'/')
      tstScores <- makeANDconjunction(tstScores,'/')
      
      # e.g. polyreg feature names can't be numbers, so need to rename
      startsWithDigit <- function(s) {  
         s <- substr(s,1,1)
         s >= '0' && s <= '9'
      }
      cols <- colnames(trnScores)
      if (any(sapply(cols,startsWithDigit))) {
         prependPair <- function(s) {
            tmp <- strsplit(s,'AND')[[1]]
            paste0('a',tmp[1],'AND','a',tmp[2])
         }
         colnames(trnScores) <- paste0('a',cols)
         colnames(tstScores) <- colnames(trnScores)
      }
   }
   })

   eval(tmp, parent.frame())
}

# Now, we have functions that will plot the reliability diagrams and compute
# the calibrated probability matrix for test data.

# arguments

#     
#     trnScores: vector/matrix of scores output from running the
#        classification method on the training set; will have either c
#        or c(c-1)/2 columns, where c is the number of classes
#     tstScores: scores for the data to be predicted
#     calibMethod: currently knnCalib or plattCalib
#     opts: R list of classification-specific parameters, e.g.
#        list(k = 50) for knnCalib
#     plotsPerRow: number of plots per row; 0 means no trellis plotting
#     oneAtATime: if TRUE, show the plots one at a time, and give the
#        user the option to print and/or zoom in.
#     OVA (bool): a boolean value to denote whether this is one-vs-all approach
#               if set to False, it will use all-vs-all approach  
#     isProb (bool): a boolean value to denote whether scores are probabiltiies
#     smoothingFtn (function): a smoothing function whether it should be applied.
#     style: 1 or 2 to denote which multicalss reliability diagram style
#             you prefer
#  values:
#        1. a list that contains a probability matrix 
#        2. it also returns the reliability diagrams for each class

calibWrap <- function(trnY,tstY, trnScores,tstScores,calibMethod,
   opts=NULL,nBins=25,se=FALSE,plotsPerRow=0,oneAtATime=TRUE, OVA=TRUE, 
   isProb = FALSE, fitAllX = FALSE, smoothingFtn=NULL, style= NULL) 
{
   classNames <- levels(trnY)
   nClass <- length(classNames)
   ym <- factorToDummies(tstY,fname='y')
   if(OVA){

      prob <- ovaCalib(trnY,
         trnScores, 
         tstScores,
         calibMethod,
         deg=opts$deg,
         K= opts$k,
         se=se,
         scaleX = opts$scaleX,
         smoothingFtn = smoothingFtn,
         isProb = isProb,
         fitAllX=fitAllX)

   }else{
       prob <- avaCalib(trnY,
         trnScores, 
         tstScores,
         calibMethod,
         deg=opts$deg,
         K= opts$k,
         se=se,
         isProb = isProb)
    }
  
   res <- list(probs=prob)

   
   if (plotsPerRow) {
      nRow <- ceiling(nClass/plotsPerRow)
      par(mfrow=c(nRow,plotsPerRow))
      for (rw in 1:nRow) {
         start <- (rw - 1) * plotsPerRow + 1
         end <- min(rw * plotsPerRow,nClass)
         for (cls in start:end) {
            tmp <- 
               reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE)
         }
      }
      par(mfrow=c(1,1))
   }else if (oneAtATime) {
      for (cls in 1:nClass) {
         reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,classNum=cls)
         while (1) {
            print('you can go to the next plot, or zoom, or print to file')
            cmd <- 
               readline('hit Enter for next plot, or low hi or fname: ')
            if (cmd == '') break
            cmdParts <- pythonBlankSplit(cmd)
            if (length(cmdParts) == 1)
               prToFile(cmd)
            else {
               if (cmdParts[2] == 'ly') cmdParts[2] <- as.numeric(nrow(ym))
               zoom <- as.numeric(cmdParts)
               reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,zoom=zoom,
                  classNum=cls)
            }
         }
      }
   }else{

      # Plot all in one plot
      # if it is not one at a time and plots per row are not specified

      reliabDiagram(tstY,res$probs,nBins,TRUE,
         multiclass=TRUE, multiclassStyle= style)
         while (1) {
            print('you can zoom, or print to file')
            cmd <- 
               readline('hit Enter for next plot, or low hi or fname: ')
            if (cmd == '') break
            cmdParts <- pythonBlankSplit(cmd)
            if (length(cmdParts) == 1)
               prToFile(cmd)
            else {
               if (cmdParts[2] == 'ly') cmdParts[2] <- as.numeric(nrow(ym))
               zoom <- as.numeric(cmdParts)
               reliabDiagram(tstY,res$probs, nBins,TRUE,zoom=zoom, 
                  multiclass=TRUE, multiclassStyle= style)
            }
      }

     
   }
   res
}

# need column names of pairs of features to have 'AND' as delimiter,
# e.g. 'CatcherANDFirst_Baseman' in the mlb data, replacing the original
# delimiter, such as '/ for e1071

# replace old delimiter by 'AND' in the colnames of the decision values
makeANDconjunction <- function(dvals,oldDelim) 
{
   colnames(dvals) <- gsub(oldDelim,'AND',colnames(dvals)) 
   dvals
}

##########################################################################
########################  e1017 routines  ################################
##########################################################################

# for those users of the probability calibration functions on output
# from the e1071 package, here are useful utilities

# calculcate decision values ("scores") for new cases on previously-fit
# e1071 SVM
getDValsE1071 <- function(object,newx) 
{
   require(e1071)
   # need to strip off non-SVM classes, if any
   toDelete <- NULL
   for (i in 1:length(class(object))) {
      if (!class(object)[i] %in% c('svm.formula','svm')) 
         toDelete <- c(toDelete,i)
   }
   if (length(toDelete) > 0) class(object) <- class(object)[-toDelete]
   tmp <- predict(object,newx,decision.values=TRUE)
   attr(tmp,'decision.values')
}

# reliability diagram and associated compilation of statistics

# arguments:

#    y: vector of Y values from training set, 0s and 1s
#    probs: vector of estimated cond. class probabilities
#    nBins: number of bins
#    plotGraph: TRUE means plotting is desired
#    zoom: a tuple of values to set the limit of the axis e.g. (0, 100)
#    classNum (int): an integet to specify the class number in the title of the plot
#    multiclass (bool): determine whether it is a multiclass case.
#    multiclassStyle: 1 or 2 to denote which multicalss reliability diagram style
#             you prefer
#    
# values:
#    one reliability diagram

reliabDiagram <- function(y,probs,nBins,plotGraph=TRUE,zoom=NULL,classNum=NULL, 
   multiclass=FALSE, multiclassStyle= 1) 
{
   if(multiclass){
      # Change the margin
      # this probably needs to be fixed so that it can scale with num of classes 
      par(mar=c(5,4,2,6)) 

      ym <- factorToDummies(y,fname='y')
      nClass <- length(levels(y))
      tokencode<- rep(1:25,5)
      sizecode <- seq(0.1, 1, length.out = nClass)
      breaks <- seq(0,1,1/nBins)

      # generate colors
      if(nClass > 8){
         # requires a lot of colors
         colorParlette <- colors()[c(seq(5,150,5), 655, seq(360, 655,5))]
      }else{
         # get the most distinct 8 colors default
         colorParlette <- 1:8
      }

      record = list()
      for(class in 1:nClass){

         probsBinNums <- findInterval(probs[,class],breaks)
         fittedYCounts <- tapply(probs[,class],probsBinNums,mean)
         actualYCounts <- tapply(ym[,class],probsBinNums,mean)
         axisLimit <- 1

         # record the counts
         record[[levels(y)[class]]] <- cbind(fittedYCounts, actualYCounts)

         color <- colorParlette[class]
         size <- sizecode[class]
         token <- tokencode[class]

         if(plotGraph){

            if (nClass > 25 & multiclassStyle == 2){
               print("Repeated tokens will be used for over 25 classes")
            }

            if (is.null(zoom)) {
               zoomTo <- 1:nBins
               lims <- c(0,axisLimit) 
            }else {
               ftdy <- fittedYCounts
               zoomTo <- which(ftdy >= zoom[1] & ftdy <= zoom[2])
               lims <- zoom
            }

            if (class == 1){
               if (multiclassStyle == 1){

                  # Style 1: color and size varying
                  plot(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     col = rep(color, length(fittedYCounts[zoomTo])), pch = 1, 
                     cex = rep(size, length(fittedYCounts[zoomTo])),
                     xlim=lims,ylim=lims, xlab='Predicted Probability', 
                     ylab='Actual Probability')

               }else if(multiclassStyle == 2){

                  #Style 2: black dots with varying tokens
                  plot(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     pch = rep(token , length(fittedYCounts[zoomTo])), 
                     xlim=lims,ylim=lims, xlab='Predicted Probability', 
                     ylab='Actual Probability')

               }else{
                  # Style 3: color and size fixed
                  plot(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     col = rep(color, length(fittedYCounts[zoomTo])), pch = 1, 
                     xlim=lims,ylim=lims, xlab='Predicted Probability', 
                     ylab='Actual Probability')
               }
            }else{
               if (multiclassStyle == 1){
                  points(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     col =rep(color, length(fittedYCounts[zoomTo])), pch = 1, 
                     cex = rep(size, length(fittedYCounts[zoomTo])))
               
               }else if(multiclassStyle == 2){
                  
                  points(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     pch = rep(token, length(fittedYCounts[zoomTo])))

               }else{
                  points(fittedYCounts[zoomTo], actualYCounts[zoomTo], 
                     col =rep(color, length(fittedYCounts[zoomTo])), pch = 1)
               }
            }
         }
      }
      if (plotGraph){
         # determine the size of the font
         if(nClass <= 20){
            fontsize = 1
         }else if(nClass > 20 & nClass <= 30){
            fontsize = 0.7
         }else if(nClass > 30 & nClass <= 40){
            fontsize = 0.5
         }else{
            fontsize = 0.2
         }

         if (multiclassStyle == 1 | multiclassStyle == 3){
            # a dash diagonal line
            abline(0,1, lty=2)
            legend("topleft", legend = levels(y), col = colorParlette[1:nClass], pch = 19,
                  inset=c(1,0), xpd=TRUE, bty="n", cex = fontsize)
         }else{
            abline(0,1,col='red')
            legend("topleft", legend = levels(y), pch = tokencode[1:nClass],
               inset=c(1,0), xpd=TRUE, bty="n", cex = fontsize)
         }
      }
   # Reset the margin back to default 
   par(mar=c(5,4,4,2)+0.1) 
   record

   }else{
      breaks <- seq(0,1,1/nBins)
      probsBinNums <- findInterval(probs,breaks)
      fittedYCounts <- tapply(probs,probsBinNums,sum)
      actualYCounts <- tapply(y,probsBinNums,sum)
      axisLimit <- max(max(fittedYCounts),max(actualYCounts))
      if (plotGraph) {
         if (is.null(zoom)) {
            zoomTo <- 1:nBins
            lims <- c(0,axisLimit) 
      }else {
         ftdy <- fittedYCounts
         zoomTo <- which(ftdy >= zoom[1] & ftdy <= zoom[2])
         lims <- zoom
      }

         plot(fittedYCounts[zoomTo],actualYCounts[zoomTo],
            xlim=lims,ylim=lims,xlab='fittedYCounts',ylab='actualYCounts')
         abline(0,1,col='red')
         if (!is.null(classNum)) {
            topLabel <- paste('Class',classNum)
            title(main=topLabel,col='blue')
         }
      }
   
      cbind(fittedYCounts,actualYCounts)
   }
}

logOddsToProbs <- function(x) 
{
   u <- exp(-x)
   1 / (1+u)
}

# arguments:

#    y: labels in training set; a 0s and 1s vector 
#    scores: values that your ML algorithm predicts from

ROC <- function(y,scores) 
{
   n <- length(y)
   numPos <- sum(y)
   numNeg <- n - numPos
   scoreOrder <- order(scores,decreasing=T)
   tpr <- vector(length = n)
   fpr <- vector(length = n)
   for (i in 1:n) {
      # scoresSorted = sort(scores); h = scoresSorted[i]
      whichGteH <- scoreOrder[1:i]
      numTruePos <- sum(y[whichGteH] == 1)
      numFalsePos <- i - numTruePos
      tpr[i] <- numTruePos / numPos
      fpr[i] <- numFalsePos / numNeg
   }
   plot(fpr,tpr,type='l',pch=2,cex=0.5)
   abline(0,1)

}


#########################  crossEntropy()  ################################

# it calculates crossEntropy for probability calibrationn algorithms
# 
# arguments:
# calibWrapOut: output of function calibWrap() 

crossEntropy = function(calibWrapOut) {
   p = calibWrapOut$ym
   phat = calibWrapOut$probs
   x = 0
   for (i in 1:nrow(p)) {
      x = x - sum(p[i,]*log(phat[i,]))
   }  
   return(x)
}

#########################  KLDivergence()  ################################

# it calculates Kullback_Leibler divergence for probability calibration 
# algorithms
# 
# arguments:
# calibWrapOut: output of function calibWrap() 

KLDivergence = function(calibWrapOut) {
   require(philentropy)
   p = calibWrapOut$ym
   phat = calibWrapOut$probs
   x = 0
   for (i in 1:ncol(p)) {
      df = rbind(p[i,], phat[i,])
      x = x + philentropy::KL(df)
   }
   return(x)
}

#########################  PairwiseCoupling()  ################################

# it uses pairwise coupling first described in Hastie & Tibshirani 1998 
# and the algorithm proposed in Wu et al., 2003 to calculate class 
# probability from pairwise probabilities generated in AVA classifiation.
# 
# arguments:
# K: number of classess
# probs: pairwise probabilities of one sample

# code credit:
# Claas Heuer, September 2015
#
# Reference: Probability Estimates for Multi-class Classification by Pairwise Coupling, Wu et al. (2003)

PairwiseCoupling = function(K, probs)
{
   Q <- matrix(0,K,K)
   Q[lower.tri(Q)] <- 1 - probs
   Qt <- t(Q)
   Q[upper.tri(Q)] <- 1 - Qt[upper.tri(Qt)]
   diag(Q) <- rowSums(Q)
   Q <- Q / (K-1)
   
   p <- rbeta(K,1,1)
   p <- p/sum(p)
   
   # updating the prob vector until equilibrium is reached
   for(i in 1:1000) p <- Q%*%p
   
   out = round(t(p), digits=4)
   
   return(out)
}
