
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

# reads in a set of ordinary image files, say JPEGs, and converts the
# entire data to an nxp matrix, for n images and p pixels per image

# arguments:
#    imgDir: character vector specifying where the image files reside
#    fmt: image name suffix
#    minSize: action to be taken if the files are found to be of 
#       different sizes; if NULL, simply stop; if not, this must 
#       be a 2-component vector, say (r,s), in which case the 
#       middle rxs portion of each image will be selected

# value:

#   matrix as described above, plus an R attribute giving the number of
#   rows and columns per image

imgFilesToMatrix <- function(imgDirs='.',fmt='jpg',minSize=NULL) 
{
   imgFiles <- dir(imgDir,pattern=fmt,full.names=TRUE)
   res <- lapply(imgFiles,imgFileToVector)
   # check for unequal sizes
   tmp <- t(sapply(res,function(fl) attr(fl,'dims')))
   if (nrow(unique(tmp)) > 1) {
      minxy <- apply(tmp,2,min)
      if (is.null(minSize)) {
         print('images are of different sizes')
         print('smallest numbers of rows, cols:')
         print(minxy)
      }
      for (img in imgFiles) {
         # take the middle minxy of each image
      }
   }
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
