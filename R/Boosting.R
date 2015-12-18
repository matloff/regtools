
# "BerkBoost" -- inspired by Berk, p.21

# arguments:
# 
#    x: predictor matrix
#    y: response vector
#    mdl: 'lm' or 'glm'
#    wtftn(y,muhat): function to weight each observation
#    lossftn(y,muhat): loss function for observation
#    niters: number of iterations
#    maxwt: lm() weights will be truncated to this
# 
# value:
# 
#    boosted beta-hat vector

#############  EXPERIMENTAL  ##############

bboostlm <- function(x,y,
      mdl='lm',wtftn=l2inv,lossftn=l1,niters=1000,maxwt=100) {
   x <- as.matrix(x)
   linmod <- mdl == 'lm'
   if (linmod) tmp <- lm(y ~ x) else 
      tmp <- glm(y ~ x,family=binomial)
   fit <- tmp$fitted.values
   sumloss <- sum(lossftn(y,fit))
   bhmat <- c(coef(tmp),sumloss)
   for (i in 1:niters) {
      w <- wtftn(y,fit)
      w <- pmin(w,maxwt)
      if (linmod) tmp <- lm(y ~ x,weights=w) else 
         tmp <- glm(y ~ x,family=binomial,weights=w)
      fit <- tmp$fitted.values
      sumloss <- sum(lossftn(y,fit))
      newbhmat <- c(coef(tmp),sumloss)
      bhmat <- rbind(bhmat,newbhmat)
   }
   p <- ncol(bhmat) - 1
   p1 <- p + 1
   w <- 1 / bhmat[,p1]
   w <- pmin(w,1.0e+04)
   w <- w / sum(w)
   rslt <- list()
   rslt$bhat = w %*% bhmat[,1:p] 
   rslt$mdl <- mdl
   class(rslt) <- "bboost"
   rslt
}

# pred() for "bboost" class
#
# arguments:
#    bboost: output of bboostlm()
#    predx: X vectors to predict at, one row per case
#
# value:
#    estimated regression values at the points predx

#############  EXPERIMENTAL  ##############
predict.bboost <- function(bbout,predx) {
   bhat <- as.vector(bbout$bhat)
   predx1 <- as.matrix(cbind(1,predx))
   regvals <- predx1 %*% bhat
   if (bbout$mdl == 'glm') 
      regvals  <- 1 / (1 + exp(-regvals))
   regvals
}

l2 <- function(y,muhat) (y - muhat)^2
l1 <- function(y,muhat) abs(y - muhat)
l2inv <- function(y,muhat) 1 / (y - muhat)^2
l1inv <- function(y,muhat) 1 / abs(y - muhat)

l1inv05 <- function(y,muhat) 1 / abs(muhat - 0.5)

predwrong <- function(y,muhat) {
   predy <- round(muhat)
   as.numeric(y != predy)
}

