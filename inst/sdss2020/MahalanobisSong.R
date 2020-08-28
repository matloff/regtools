
ibrary(regtools)
load('YearData.save')  # obtain separately, data frame 'yr'
yr <- yr[,seq(2,91,5)]
idxs <- sample(1:nrow(yr),100000)
yr1 <- yr[idxs,]
idxs <- sample(1:nrow(yr1),5000)
trn <- yr1[-idxs,]
tst <- yr1[idxs,]
xtrn <- trn[,-1]
ytrn <- trn[,1]
xtst <- tst[,-1]
ytst <- tst[,1]
knnout <- kNN(xtrn,ytrn,xtst,25)
mhd <- knnout$mhdists
far <- which(mhd > 150)
xn <- xtst[far,]
yn <- ytst[far]
preds <- kNN(xtrn,ytrn,xn,25)$regests
mean(abs(preds - yn))  
preds <- kNN(xtrn,ytrn,xn,25,smoothingFtn=loclin)$regests
mean(abs(preds - yn)) 

