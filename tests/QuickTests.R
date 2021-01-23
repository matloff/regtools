
# built-in data on major league baseball players
data(mlb)  
mlb <- mlb[,3:6]  # position, height, weight, age

set.seed(9999)

# fit models
knnout <- qeKNN(mlb,'Weight',k=25)
rfout <- qeRF(mlb,'Weight')

# mean abs. pred. error on holdout set, in pounds
knnout$testAcc
# [1] 11.75644
rfout$testAcc
# [1] 12.6787

# predict a new case
newx <- data.frame(Position='Catcher',Height=73.5,Age=26)
predict(knnout,newx)
       [,1]
# [1,] 204.04
predict(rfout,newx)
      11 
# 199.1714

set.seed(9999)
# how about some other ML methods?
lassout <- qeLASSO(mlb,'Weight')
lassout$testAcc
# [1] 12.31019
# poly reg, degree 3 
polyout <- qePolyLin(mlb,'Weight',3)
polyout$testAcc
# [1] 13.83444
nnout <- qeNeural(mlb,'Weight')
# ...
nnout$testAcc
# [1] 10.23094
# try some nondefault hyperparams
nnout <- qeNeural(mlb,'Weight',hidden=c(200,200),nEpoch=50)
nnout$testAcc
# [1] 13.40559

# predict player position, 6 categories
knnout <- qeKNN(mlb,'Position',k=25)
rfout <- qeRF(mlb,'Position')
knnout$testAcc
# [1] 0.7524752
rfout$testAcc
# [1] 0.6138614
table(mlb$Pos) / sum(table(mlb$Pos))
#          Catcher    First_Baseman       Outfielder   Relief_Pitcher 
#       0.07487685       0.05418719       0.19113300       0.31034483 
#   Second_Baseman        Shortstop Starting_Pitcher    Third_Baseman 
#       0.05714286       0.05123153       0.21674877       0.04433498 

# kNN worse than always guessing Relief_Pitcher, RF about the same
z <- qePolyLog(mlb,'Position',holdout=NULL)
predict(z,mlb[8,-1])
# $predClasses
# [1] "Outfielder"
# $probs
#      Catcher First_Baseman Outfielder Relief_Pitcher Second_Baseman Shortstop
# [1,]   0.125         0.125      0.125          0.125          0.125     0.125
#      Starting_Pitcher Third_Baseman
# [1,]            0.125         0.125
z <- qePolyLog(mlb[,c(1,3)],'Position',holdout=NULL)
predict(z,mlb[8,3])

set.seed(9999)
lgout <- qeLogit(mlb,'Position')
lgout$testAcc
# [1] 0.6732673
newx <- data.frame(Height=73.5,Age=26,Weight=200)
predict(lgout,newx)
# $predClasses
# [1] "Relief_Pitcher"
# 
# $probs
#         Catcher First_Baseman Outfielder Relief_Pitcher Second_Baseman
# [1,] 0.06527784    0.05201025   0.214516      0.3336662     0.03421254
#       Shortstop Starting_Pitcher Third_Baseman
# [1,] 0.03345139        0.2252583    0.04160745

# day2
d2 <- day2[,-(13:14)]
z <- pcaKNN(0.6,d2,'tot',k=25,holdout=NULL)
newx <- d2[8,-13]
predict(z,newx)
#         [,1]
# [1,] 1440.44

qeCompare(mlb,'Weight',
   c('qeLin','qePolyLin','qeKNN','qeRF','qeLASSO','qeNeural'),25)
#       qeFtn  meanAcc
# 1     qeLin 13.30490
# 2 qePolyLin 13.33584
# 3     qeKNN 13.72708
# 4      qeRF 13.46515
# 5   qeLASSO 13.27564
# 6  qeNeural 14.01487

