

# wrappers and other routines for Keras analysis of image data

require(keras)
require(car)

########################  krsFit()  ###################################

# easy wrapper for R wrapper to Keras

# dense and convolutional layers only

# arguments:

#    x: nxp matrix of predictor values
#    y: numeric vector of outcome values; in classification case
#       integers, not an R factor, and take on the values 0,1,...,nClass-1
#    hidden: vector of number of units per hidden layer, or proportions
#       for dropout
#    acts: vector of activation functions
#    conv: convolutional/pooling layers (see below)
#    classif: if TRUE, classification problem, otherwise regression
#    nClass: in the classification case, number of classes
#    nEpoch: desired number of epochs

# value:

#    R list: 
#         Keras model
#         object returned by Keras Fit()
#         classif
#         mmScaleX,mmScaleY

# by default both x and y will be internally scaled, then unscaled during predict()

# examples of 'conv' elements

# list(type = 'conv2d',xShape = c(28,28,3),kern = 5)

# the only pooling offered is max pool; ReLU is used for the activation
# function at each conv2d layer

krsFit <- function(x,y,hidden,acts=rep('relu',length(hidden)),conv=NULL,
             xShape=NULL,classif=TRUE,nClass=NULL,nEpoch=30,
             scaleX=TRUE,scaleY=TRUE) 
{
   # scaling
   if (scaleX) {
      x <- mmscale(x)
      mmScaleX <- attr(x,'minmax')
   } else mmScaleX <- NULL
   if (classif) {
      y <- to_categorical(y,nClass)
      mmScaleY <- NULL
   } else {
      if (scaleY) {
         y <- mmscale(y)
         mmScaleY <- attr(y,'minmax')
      } else mmScaleY <- NULL
   }
   
   # build model
   model <- keras_model_sequential()

   # convolutional layers, if any
   if (!is.null(conv)) {
      layer <- conv[[1]]
      if (layer$type != 'conv2d') stop('invalid first layer')
      # convert x to tensor
      xShape <- conv$xShape
      x <- matrixToTensor3(x,xShape) 
      layer_conv_2d(model,filters=layer$filters,kernel_size=layer$kern,
         activation='relu',input_shape=xShape)
      for (i in seq(2,length(conv),1)) {
         layer <- conv[[i]]
         if (layer$type == 'pool') {
            layer_max_pooling_2d(model,pool_size = layer$kernn)
         } else if (layer$type == 'conv2d') {
            layer_conv_2d(model,filters=layer$filters,kernel_size=layer$kern,
               activation='relu')
         } else if (layer$type == 'drop') {
            layer_dropout(model,layer$drop)
         } else stop('invalid layer type')
      }
      layer_flatten(model)
   }

   # hidden layers
   if (is.null(conv)) {
      layer_dense(model,units = hidden[1], activation = acts[1],
         input_shape = ncol(x)) 
      firstHidden <- 2
   } else firstHidden <- 1
   nHidden <- length(hidden)
   for (i in seq(firstHidden,nHidden,1)) {
      hi <- hidden[i]
      if (hi >= 1) {
         layer_dense(model,units = hidden[i], activation = acts[i])
      }
      else {
         layer_dropout(model,hi)
      }
   }
   # output layer and determine loss ftn etc.
   if (classif) {
      layer_dense(model,units = nClass, activation = "softmax")
      lossFtn <- 'categorical_crossentropy'
      metrics <- 'accuracy'
   } else {
      layer_dense(model,units = 1)
      lossFtn <- 'mse'
      metrics <- 'mae'
   }
   # summary(model)

   compile(model,
     loss = lossFtn, 
     # batch_size = batchSize,
     optimizer = optimizer_rmsprop(),
     metrics = metrics)

   fitOut <- fit(model,
     x, y,
     epochs = nEpoch, batch_size = 128,
     validation_split = 0.2
   )

   res <- list(model=model,fitOut=fitOut,classif=classif,x=x,
             xShape=xShape,mmScaleX=mmScaleX,mmScaleY=mmScaleY)
   class(res) <- 'krsFit'
   res
}

predict.krsFit <- function(krsFitOut,newx) 
{
   model <- krsFitOut$model
   mm <- krsFitOut$mmScaleX
   if (!is.null(mm)) newx <- mmscale(newx,mm)
   if (!is.null(krsFitOut$xShape)) {
      newx <- matrixToTensor3(newx,krsFitOut$xShape) 
   }
   preds <- predict(model,newx)
   if (krsFitOut$classif) {
      preds <- apply(preds,1,which.max) - 1
   } else {
      mm <- krsFitOut$mmScaleY
      if (!is.null(mm))
         preds <- mm[1] + preds * (mm[2] - mm[1])
   }
   preds
}

# takes image in vector form and converts to 3D tensor; xShape is the
# number of rows, number of columns and optionally number of channels
matrixToTensor3 <- function(x,xShape) 
{
   nrw <- xShape[1]
   ncl <- xShape[2]
   if (length(xShape) == 3) {
      nch <- xShape[3]
   } else {
      nch <- ncol(x) / (nrw*ncl)
   }
   array_reshape(x, c(nrow(x),nrw,ncl,nch))
}

########################  krsFitImg()  ###################################

# non-convolutional models only

# arguments:

#    x: the pixel intensities, as a matrix of nImg rows 
#       and nr x nc columns, for images of size nr x nc
#    y: the vector of class labels, values 0,1,2,...
#    neurons: vector of units in each hidden layer
#    acts: vector activation function names, hidden layers; can be 
#       'relu','sigmoid','softmax','tanh', etc., or custom --
#       https://keras.io/api/layers/activations/
#    nClass: number of classes

krsFitImg <- function(x,y,hidden,acts=rep('relu',length(hidden)),
                nClass,nEpoch=30) 
{
   x <- x / 255
   krsFit(x=x,y=y,hidden=hidden,classif=TRUE,nClass=nClass,
      nEpoch=nEpoch, scaleX=FALSE,scaleY=FALSE)
}

##########################  diagNeural()  #################################

# neural networks are closely related to polynomial regression, and it
# is well known that such models tend to suffer from multicollinearity
# at higher degrees; this function checks for multicollinearity at each
# layer, by computing the condition number of X'X, where X, n x m, is
# the set of "new features" created by this layer (here m is the number
# of units in the layer)

# arguments:

#    fitOut: return value from krsFit()

diagNeural <- function(krsFitOut) 
{
   model <- krsFitOut$model
   modLayers <- model$layers
   modLayers[[length(modLayers)]] <- NULL  # delete output layer
   nLayers <- length(modLayers)
   condNums <- rep(NA,nLayers)
   n0Cols <- rep(0,nLayers)
   nConstCols <- rep(0,nLayers)
   vif10 <- rep(NA,nLayers)
   for (i in 1:nLayers) {
      layer <- modLayers[[i]]
      layerOut <- keras_model(inputs = model$input, outputs = 
         layer$output)
      # compute "new features"
      output <- predict(layerOut,krsFitOut$x)
      # 0 or other constant columns
      cco <- constCols(output)
      nConstCols[i] <- length(cco)
      if (length(cco) > 0) {
         tmp <- apply(output[,cco,drop=FALSE],2,function(w) all(w == 0))
         n0Cols[i] <- sum(tmp)
      }
      # condition numbers
      xpx <- t(output) %*% output
      eigs <- eigen(xpx)$values
      condNums[i] <- sqrt(eigs[1] / eigs[length(eigs)])
      # VIF comp
      z <- runif(nrow(output))
      zz <- cbind(z,output)
      zz <- as.data.frame(zz)
      lmout <- lm(z ~ .,data=zz)
      v <- try(vif(lmout))
      if (!inherits(v, "try-error")) vif10[i] <- mean(v >= 10)
   }
   list(condNums=condNums,n0Cols=n0Cols,nConstCols=nConstCols,vif10=vif10)
}

