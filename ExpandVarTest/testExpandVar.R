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
testSingleExpandVar <- function(x, y, kmax, expandCol, expandMaxVal, interval = 1, all = FALSE)
{
  require(ggplot2)
  res <- NULL
  allExpandVals <- seq(0, expandMaxVal, interval)
  for(i in allExpandVals){
    knnout <- kNN(x,y,x,kmax,scaleX = TRUE, expandVars = expandCol, expandVals = i, allK = all, leave1out = TRUE)
    # when @all == FALSE, regests is not matrix and cannot be used in findOverallLoss
    if(!is.matrix(knnout$regests)) {
      dim(knnout$regests) <- c(1,length(knnout$regests))
    }
    mape <- findOverallLoss(knnout$regests, y)
    res <- c(res, min(mape))
  }
  
  dat <- as.data.frame(cbind(allExpandVals, res))
  total_pts <- length(allExpandVals)
  # using 3 percent of points to do local regression
  span_pts <- 0.03*total_pts
  ggplot(dat, aes(x=allExpandVals,y=res)) + geom_smooth(method=loess, span=span_pts, colour = "black") +
    labs(title = "MAPE vs. Expanded Variable's Value") +  ylab("MAPE") + xlab("Expanded Variable's Value") +
    theme(plot.title = element_text(hjust = 0.5))
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
      knnout <- kNN(x,y,x,kmax, scaleX = TRUE, expandVars = combCases[, j],
                         expandVals = expandVals[logicalVec], allK = all, leave1out = TRUE)

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

# using k = 25
if(FALSE) {
  # expand single predictor
  # expand @instant
  testSingleExpandVar(day1x, day1$tot, 25, 1, 10, 0.01)
  testSingleExpandVar(day1x, day1$tot, 25, 1, 100, 0.1)
  testSingleExpandVar(day1x, day1$tot, 25, 1, 1000, 0.1)
  # expand @temp
  testSingleExpandVar(day1x, day1$tot, 25, 8, 10, 0.01)
  testSingleExpandVar(day1x, day1$tot, 25, 8, 100, 0.1)
  testSingleExpandVar(day1x, day1$tot, 25, 8, 1000, 0.1)
  # expand @hum
  testSingleExpandVar(day1x, day1$tot, 25, 10, 10, 0.01)
  testSingleExpandVar(day1x, day1$tot, 25, 10, 100, 0.1)
  testSingleExpandVar(day1x, day1$tot, 25, 10, 1000, 0.1)
  # expand @windspeed
  testSingleExpandVar(day1x, day1$tot, 25, 11, 10, 0.01)
  testSingleExpandVar(day1x, day1$tot, 25, 11, 100, 0.1)
  testSingleExpandVar(day1x, day1$tot, 25, 11, 1000, 0.1)
  # TODO: expand multiple variables
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
peDummsX <- peDumms[, c(1, 3:6, 8:22, 24:29, 32)]
peDummsY <- peDumms$wageinc

# using k = 30
if(FALSE) {
  # expand single predictor
  # expand @age
  testSingleExpandVar(peDummsX, peDummsY, 30, 1, 10, 0.01)
  testSingleExpandVar(peDummsX, peDummsY, 30, 1, 5, 0.01)
  testSingleExpandVar(peDummsX, peDummsY, 30, 1, 100, 0.1)
  testSingleExpandVar(peDummsX, peDummsY, 30, 1, 1000, 0.1)
  # expand @wkswrkd
  testSingleExpandVar(peDummsX, peDummsY, 30, 27, 10, 0.01)
  testSingleExpandVar(peDummsX, peDummsY, 30, 27, 100, 0.1)
  testSingleExpandVar(peDummsX, peDummsY, 30, 27, 1000, 0.1)
  
  # TODO: expand multiple variables
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

# Fifth test dataset: forest cover dataset
# Download dataset
if(FALSE){
  temp <- tempfile()
  download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz", temp)
  forestCoverType <- read.table(gzfile(temp, "covtype.data"), header = FALSE, sep = ",")
  unlink(temp)
}

# Dataset description
# Elevation                               quantitative    meters                       Elevation in meters
# Aspect                                  quantitative    azimuth                      Aspect in degrees azimuth
# Slope                                   quantitative    degrees                      Slope in degrees
# Horizontal_Distance_To_Hydrology        quantitative    meters                       Horz Dist to nearest surface water features
# Vertical_Distance_To_Hydrology          quantitative    meters                       Vert Dist to nearest surface water features
# Horizontal_Distance_To_Roadways         quantitative    meters                       Horz Dist to nearest roadway
# Hillshade_9am                           quantitative    0 to 255 index               Hillshade index at 9am, summer solstice
# Hillshade_Noon                          quantitative    0 to 255 index               Hillshade index at noon, summer soltice
# Hillshade_3pm                           quantitative    0 to 255 index               Hillshade index at 3pm, summer solstice
# Horizontal_Distance_To_Fire_Points      quantitative    meters                       Horz Dist to nearest wildfire ignition points
# Wilderness_Area (4 binary columns)      qualitative     0 (absence) or 1 (presence)  Wilderness area designation
# Soil_Type (40 binary columns)           qualitative     0 (absence) or 1 (presence)  Soil Type designation
# Cover_Type (7 types)                    integer         1 to 7                       Forest Cover Type designation
forestCoverTypeX <- forestCoverType[,-55]
forestCoverTypeY <- forestCoverType[,55]
# using k =
if(FALSE){
  
}
