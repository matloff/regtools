
library(mvtnorm)
n <- 500
cv <- rbind(c(1,0.2),c(0.2,1))
xy <- NULL
for (i in 1:3)
  xy <- rbind(xy,rmvnorm(n,mean=rep(i*0.5,2),sigma=cv))
xy <- cbind(xy,rep(0:2,each=n))