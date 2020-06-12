
# ops dealing with images

############################  imgTo2D()  ##################################

# converts a vector storing an image into a 2-D point cloud, with rows
# of the form (i,j,x), where x is the intensity at pixel (i,j)

# arguments:

#   img: the image, in col-major order
#   nr: number of rows in the image
#   hasClassID: if TRUE, last element of img is a class ID

# value:

#   matrix with rows of the form (i,j,x) or (i,j,x,c), where x is the image
#   intensity at pixel (i,j) and c is a class ID; number of rows will be nr,
#   number of columns length(img) / nr

imgTo2D <- function(img,nr,hasClassID=FALSE) 
{
   if (mode(img) == 'list') img <- sapply(img,function(t) t)
   lImg <- length(img)
   if (hasClassID) {
      classID <- img[lImg]
      img <- img[-lImg]
      lImg <- lImg - 1
   }
   
   nc <- lImg / nr
   if (round(nc) != nc) stop('noninteger number of columns')
   img <- matrix(img,nrow=nr,byrow=TRUE)
   output <- NULL
   for (j in 1:nc) {
      tmp <- cbind(1:nr,j,img[,j])
      output <- rbind(output,tmp)
   }

   if (hasClassID) output <- cbind(output,classID)

   output
}

############################  imgToMatrix()  ##############################

# assumes that the specified directory contains image files in, e.g.,
# JPG format; converts them to a matrix; pixel values only, no labels;
# matrix has one row per image

imgFilesToMatrix <- function(imgDir,fmt) 
{
   imgFiles <- dir(imgDir,pattern=fmt,full.names=TRUE)
   res <- lapply(imgFiles,imgFileToVector)
   # check for unequal sizes
   tmp <- t(sapply(res,function(fl) attr(fl,'dims')))
   if (nrow(unique(tmp)) > 1) 
      stop('images are of different sizes')
   res <- do.call(rbind,res)
   attr(res,'dims') <- tmp[1,]
   res
}

imgFileToVector <- function(fl) 
{
   require(magick)
   tmp <- image_read(fl)
   ii <- image_info(tmp)
   dims <- c(ii[[2]],ii[[3]])
   v <- as.numeric(as.vector(tmp[[1]])) * 255
   channels <- length(v) / prod(dims)
   dims <- c(dims,channels)
   names(dims) <- c('width','height','channels')
   attr(v,'dims') <- dims
   v
}
