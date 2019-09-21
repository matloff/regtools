
# ops dealing with image

# converts a vector storing an image into a 2-D point cloud, with rows
# of the form (i,j,x), where x is the intensity at pixel (i,j)

# arguments:

#   img: the image, in row-major order
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

