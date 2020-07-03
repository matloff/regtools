
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
#    imgDir: directory where the image files reside
#    fmt: image name suffix
#    minSize: action to be taken if the files are found to be of 
#       different sizes; if NULL, simply stop; if not, this must 
#       be a 2-component vector, say (r,s), in which case the 
#       middle rxs portion of each image will be selected

# value:

#   matrix as described above, plus an R attribute giving the number of
#   rows and columns per image

imgFilesToMatrix <- function(imgDir='.',fmt='jpg',minSize=NULL) 
{
   imgFiles <- dir(imgDir,pattern=fmt,full.names=TRUE)
   res <- lapply(imgFiles,imgFileToVector)
   # now element i of the R list res will be the vector version of image
   # i, with the R attribute 'dims' to show the numbers of rows and
   # columns in this image

   # check for unequal image sizes
   tmp <- t(sapply(res,function(fl) attr(fl,'dims')))
   if (nrow(unique(tmp)) > 1) {  # unequal sizes
      minxy <- apply(tmp,2,min)  # smallest numbers of rows and cols
      if (is.null(minSize)) {
         print('images are of different sizes')
         print('smallest numbers of rows, cols:')
         print(minxy)
         stop()
      }

      # for determining the center region of an image
      centerX <- round((1+minxy[1])/2)
      centerY <- round((1+minxy[2])/2)

      # take the middle minxy of each image
      for (i in 1:length(imgFiles)) {
         xPix <- tmp[i,1]
         yPix <- tmp[i,2]
         midX <- round((1+xPix)/2)
         midY <- round((1+xYix)/2)
         imgVec <- res[[i]]
         x1 <- midX - (centerX - 1)
         x2 <- midX + minxy[1] - centerX
         y1 <- midY - (centerY - 1)
         y2 <- midY + minxy[2] - centerY
         res[[i]] <- res[[i]][x1:x2,y1:y2]
      }
   }
   res <- do.call(rbind,res)
   attr(res,'dims') <- tmp[1,]
   res
}

# reads in the specified image file (JPEG etc.), forms a vector of its
# pixel intensities, returning the vector along with an R attribute
# 'dims' showing the number of rows and columns in the image 
imgFileToVector <- function(fl) 
{
   # require(magick)
   tmp <- magick::image_read(fl)
   ii <- magick::image_info(tmp)
   dims <- c(ii[[2]],ii[[3]])
   v <- as.numeric(as.vector(tmp[[1]])) * 255
   channels <- length(v) / prod(dims)
   dims <- c(dims,channels)
   names(dims) <- c('width','height','channels')
   attr(v,'dims') <- dims
   v
}

# wrapper for OpenImageR::Augmentation(), applied to a set of images in
# the matrix m, one per row of m

augMatrix <- function(m,nr,nc,...) 
{
   # require(OpenImageR)
   # convert input matrix to an R list of matrices, one such matrix for
   # each row in input matrix
   mList <- lapply(1:nrow(m),function(mRow) matrix(m[mRow,],nrow=nc,ncol=nc))
   # apply Augmentation() to each such created matrix, with your
   # favorite Augmentation() arguments
   augout <- lapply(mList,function(oneM) OpenImageR::Augmentation(oneM,...))
   # convert back to form of input matrix, now with each row being the
   # transformed version of the original row
   t(sapply(augout,function(augOutElt) as.vector(augOutElt)))
}

