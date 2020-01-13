# test helper function
# this function is used to test whether expanding a single variable
# with different expanding values will improve our prediction results.
# it will outputs a scattering point graph

# x is the training set
# y is the target property we want to predict
# expandCols is an integer containing the index of variable we want to expand
# expandMaxVal is an integer indicating the largest possible value that variable can be expanded
# interval specifies the interval between the current expanding value and next expanding value
# all indicates whether we want to test all different k values

# we compare the result we get before expanding the variable and the result after expand the variable
testSingleExpandVar <- function(x, y, kmax, expandCol, expandMaxVal, interval = 1, all = TRUE)
{
  res <- NULL
  for(i in seq(1, expandMaxVal, interval)){
    knnout <- basicKNN(x,y,x,kmax,scaleX = TRUE, expand = expandCol, expandVars = i, allK = all, leave1out = TRUE)
    # when @all == FALSE, regests is not matrix and cannot be used in findOverallLoss
    if(!is.matrix(knnout$regests)) {
      dim(knnout$regests) <- c(1,length(knnout$regests))
    }
    mape <- findOverallLoss(knnout$regests, y)
    #print(mape)
    res <- c(res, min(mape))
  }
  #print(res)
  plot(seq(1, expandMaxVal, interval), res,
  main = "MAPE-ExpandVar Value",
  xlab = "Expand variable's value",
  ylab = "MAPE")
}



# this function is used to test how different combinations of expanding
# variables will affect our prediction results
# it will output a scatter point graph

# x, y, all are all the same as above
# but now expandCols is an vector containing all cols we want to expand
# expandVals containing all the corresponding expanding values

# this function should not be used when expandCols is bigger than 10
# because it will take too long for function to finish
testMultipleExpandVar <- function(x, y, kmax, expandCols, expandVals, all = TRUE)
{
  require(ggplot2)
  numExpandVar <- length(expandCols)
  
  # base case -- no expansion
  knnout <- basicKNN(x,y,x,kmax, scaleX = TRUE, allK = all, leave1out = TRUE)
  
  if(!is.matrix(knnout$regests)) {
    dim(knnout$regests) <- c(1,length(knnout$regests))
  }
  
  mape <- findOverallLoss(knnout$regests, y)
  d <- data.frame(0, min(mape))
  names(d) <- c("Number_Of_Expansion", "MAPE")
  
  # expand case
  for (i in 1:numExpandVar) {
    combCases <- combn(expandCols, i)
    numCases <- ncol(combCases)
    for (j in 1:numCases) {
      # get their position
      # it works fine because expandCols should not have repeated values
      logicalVec <- expandCols %in% combCases[, j]
      knnout <- basicKNN(x,y,x,kmax, scaleX = TRUE, expand = combCases[, j],
                         expandVars = expandVals[logicalVec], allK = all, leave1out = TRUE)

      if(!is.matrix(knnout$regests)) {
        dim(knnout$regests) <- c(1,length(knnout$regests))
      }
      
      mape <- findOverallLoss(knnout$regests, y)
      
      tmp <-data.frame(i, min(mape))
      names(tmp) <- c("Number_Of_Expansion", "MAPE")
      d <- rbind(d, tmp)
    }
  }
  # plotting scatter point graphs
  ggplot(d, aes(x=Number_Of_Expansion, y=MAPE)) + geom_point()
}



# First test dataset: day1
# predict @tot based on the following 11 predictors
# @instant, @season, @yr, @mnth, @holiday, @weekday, @workingday
# @temp, @atemp, @hum, @windspeed
data(day1)
day1x <- day1[,c(1, 3:7, 8, 10:13)]

if(FALSE) {
  # expand single predictor
  # expand @tot
  testSingleExpandVar(day1x, day1$tot, 25, 1, 100)
  # expand @temp
  testSingleExpandVar(day1x, day1$tot, 25, 8, 100)
  # expand @atemp
  testSingleExpandVar(day1x, day1$tot, 25, 9, 100)
  # expand @hum
  testSingleExpandVar(day1x, day1$tot, 25, 10, 100)
  # expand @windspeed
  testSingleExpandVar(day1x, day1$tot, 25, 11, 100)
  
  # expand multiple variables
  testMultipleExpandVar(day1x, day1$tot, 25, 1:11, rep(2,11))
}



# Second test dataset: peDumms
# Description of dataset for next 3 dataset:
# @age, with a U(0,1) variate added for jitter
# @cit, citizenship; 1-4 code various categories of citizens; 5 means noncitizen (including permanent residents)
# @educ: 01-09 code no college; 10-12 means some college; 13 is a bachelor's degree, 14 a master's,
# 15 a professional degree and 16 is a doctorate
# @occ, occupation
# @birth, place of birth
# @wageinc, wage income
# @wkswrkd, number of weeks worked
# @yrentry, year of entry to the U.S. (0 for natives)
# @powpuma, location of work
# @gender, 1 for male, 2 for female
# peDumms, same but with categorical variables converted to dummies;
# due to the large number of levels the birth and PUMA data is not included

# predict @wageinc based on the following predictors
# @age, @cit.2-5, @educ.2-16, @occ.101/102/106/140/141, @sex1, @wrkswrkd
data(peDumms)
# we filter out rows whose wageinc is 0
peDumms <- peDumms[-(peDumms$wageinc == 0), ]
x <- peDumms[, c(1, 3:6, 8:22, 24:29, 32)]
y <- peDumms$wageinc

if(FALSE) {
  # expand single predictor
  # expand @wkswrkd
  testSingleExpandVar(x, y, 30, 27, 50, interval = 5)
  # expand @sex
  testSingleExpandVar(x, y, 30, 26, 10)
  testSingleExpandVar(x, y, 30, 26, 50, interval = 5)
  # expand @cit.5
  testSingleExpandVar(x, y, 30, 5, 10)
  testSingleExpandVar(x, y, 30, 5, 50, interval = 5)
  
  # expand multiple variables
  # expand @cit.2-5
  testMultipleExpandVar(x, y, 30, 2:5, rep(2, 4))
  # expand @occ.101-141
  testMultipleExpandVar(x, y, 30, 21:25, rep(5, 5))
}



# Third test dataset: peFactors
data(peFactors)
# preprocess the dataset
# filter out those whose wageinc is 0
peFactors <- peFactors[-(peFactors == 0),]
# add new column @native, dummy variable indicates whether someone is native
native <- peFactors$yrentry == 0
# add new column @male, dummy indicates whether someone is male
male <- peFactors$sex == 1
peFactors <- cbind(peFactors, native, male)
# convert factors to numerical variables
cols <- sapply(peFactors, is.factor)
peFactors[,cols] <- lapply(peFactors[,cols], as.numeric)

# predict wageinc based on the following predictors:
# @age, @educ, @occ, @wkswrkd, @native, @male
x <- peFactors[,c(1:3,5,9,12:13)]
y <- peFactors$wageinc

# best k: around 60
if(FALSE) {
  # expand single predictor
  # expand @age
  testSingleExpandVar(x,y,80,1,10)
  # expand @cit
  testSingleExpandVar(x,y,80,2,10)
  # expand @educ
  testSingleExpandVar(x,y,80,3,10)
  # expand @occ
  testSingleExpandVar(x,y,80,4,10)
  # expand @wkswrkd
  testSingleExpandVar(x,y,80,5,10)
  # expand @native
  testSingleExpandVar(x,y,80,6,10)
  # expand @male
  testSingleExpandVar(x,y,80,7,10)
  
  # expand multiple variables
  testMultipleExpandVar(x,y,60,1:7,rep(2,7), FALSE)
}

# Fourth test dataset:MNIST of handwritten digits
# This is tested how knn works for dataset with larget p
require(keras)
require(tensorflow)

MNIST <- dataset_mnist(path = "mnist.npz")
# the origin dataset might take very long time to finish
# we only use test set of MNIST and predict test set itself
x <- MNIST$test$x
y <- MNIST$test$y

# x is a 3d matrix, we want to convert it to 2d matrix
x <- matrix(x, nrow = 10000, ncol = 28*28)
# convert y from an array to an vector
y <- factor(c(as.matrix(y)))
# convert y to dummy variables
# we don't need to omit last col because y is not predictor
y <- factorToDummies(y, "digit", omitLast = FALSE)

if(FALSE) {
  # expand single predictor
  # expand point at the center
  testSingleExpandVar(x,y,)
}








