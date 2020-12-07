######################### loadData() ##########################

# generate a list of data set 

loadData <- function(){
  
  ############## fall detection ################
  fall <- read.csv("../data/falldetection.csv")
  
  # Drop the class column to specify columns names 
  # that need to be standardized
  fall.std.cols <-  colnames(fall)[-1]

  ############## Letter  #######################
  # Get the url
  letterurl <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data'
  
  # Read in the letter data
  letter <- read.table(letterurl,
             sep = ',', fill = F, strip.white = T)
  
  # define the columns that should be standardized
  letter.std.cols <- colnames(letter)[-1]
  
  ############## pendigits #######################
  penurl1 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/pendigits/pendigits.tes'
  penurl2 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/pendigits/pendigits.tra'
  
  # pendigits test set
  pendigits.tes <- read.table(penurl1,
                       sep = ',', fill = F, strip.white = T)
  
  # pendigits train set
  pendigits.tra <- read.table(penurl2,
                       sep = ',', fill = F, strip.white = T)
  
  # Define the columns to be standardized  
  pendigits.std.cols <- colnames(pendigits.tra)[1:16]
    
  ############## Covtype  #######################
  
  # Get the current directory
  current.dir <- getwd()
  filename <- "covtype.data.gz"
  pathname <- paste(current.dir, "/", filename, sep = "")
  
  # Check the data file is in the existing directory
  if(!(file_test("-f", pathname))){
    
    # if not, then download the file
    url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz'
    file <- basename(url)
    download.file(url, file)
    
  }
    
  # open it otherwise
  covtype <- read.table(gzfile("../data/covtype.data.gz"), 
                        sep = ',', 
                        fill = F, 
                        strip.white = T) 

  colnames(covtype) <- c("elevation(meter)", 
                         "aspect", 
                         "slope",
                         "horizontal_distance_to_hydrology",
                         "vertical_distance_to_hydrology",
                         "horizontal_distance_to_roadways",
                         "hillshade_9am",
                         "hillshade_noon",
                         "hillshade_3pm",
                         "horizontal_distance_to_fire_points",
                         paste(rep("wilderness_area",4), seq(1,4),sep=""),
                         paste(rep("soil_type",40), seq(1,40),sep=""),
                         "cover_type"
                         )
  
  # Get the columns named that need to standardize
  covtype.std.cols <- c("elevation(meter)", 
                        "aspect", 
                        "slope",
                        "horizontal_distance_to_hydrology",
                        "vertical_distance_to_hydrology",
                        "horizontal_distance_to_roadways",
                        "hillshade_9am",
                        "hillshade_noon",
                        "hillshade_3pm",
                        "horizontal_distance_to_fire_points")
  
  return(list(fall=fall,
              fall.std.cols = fall.std.cols,
              pendigits.tes = pendigits.tes, 
              pendigits.tra = pendigits.tra,
              pendigits.std.cols = pendigits.std.cols,
              letter=letter, 
              letter.std.cols = letter.std.cols,
              covtype=covtype, 
              covtype.std.cols = covtype.std.cols))
  
}