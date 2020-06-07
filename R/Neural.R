

# wrappers and other routines for Keras analysis of image data

library(keras)

########################  krsFit()  ###################################

# easy wrapper for R wrapper to Keras

# arguments:

#    x: nxp matrix of predictor values
#    y: numeric vector of outcome values; in classification case
#       integers, not an R factor, and take on the values 0,1,...,nClass-1
#    hidden: vector of number of units per hidden layer, or proportions
#       for dropout
#    acts: vector of activation functions
#    conv: R list of convolutional layers
#    classif: if TRUE, classification problem, otherwise regression
#    nClass: in the classification case, number of classes
#    nEpoch: desired number of epochs

# value:

#    R list: 
#         Keras model
#         object returned by Keras Fit()
#         classif
#         mmScaleX,mmScaleY

# both x and y will be internally scaled, then unscaled during predict()

krsFit <- function(x,y,hidden,acts=rep('relu',length(hidden)),
             conv=NULL,classif=TRUE,nClass=NULL,nEpoch=30) 
{
   if (!is.null(conv)) stop('convolutional layers not yet enabled')

   # scaling
   x <- mmscale(x)
   mmScaleX <- attr(x,'minmax')
   if (classif) {
      y <- to_categorical(y,nClass)
      mmScaleY <- NULL
   } else {
      y <- mmscale(y)
      mmScaleY <- attr(y,'minmax')
   }
   
   # build model
   model <- keras_model_sequential()
   # input and first hidden layer
   layer_dense(model,units = hidden[1], activation = acts[1], 
      input_shape = ncol(x)) 
   # further hidden layers
   nHidden <- length(hidden)
   if (nHidden > 1) {
      for (i in 2:nHidden) {
         hi <- hidden[i]
         if (hi >= 1) {
            layer_dense(model,units = hidden[i], activation = acts[i])
         }
         else {
            layer_dropout(model,hi)
         }
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

   res <- list(model=model,fitOut=fitOut,classif=classif,
             mmScaleX=mmScaleX,mmScaleY=mmScaleY)
   class(res) <- 'krsFit'
   res
}

predict.krsFit <- function(krsFitOut,newx) 
{
   model <- krsFitOut$model
   mm <- krsFitOut$mmScaleX
   newx <- mmscale(newx,mm)
   preds <- predict(model,newx)
   if (krsFitOut$classif) {
      preds <- apply(preds,1,which.max) - 1
   } else {
      mm <- krsFitOut$mmScaleY
      preds <- mm[1] + preds * (mm[2] - mm[1])
   }
   preds
}

########################  krsFitImg()  ###################################

# arguments:

#    x: the pixel intensities, as a matrix of nImg rows 
#       and nr x nc columns, for images of size nr x nc
#    y: the vector of class labels, values 0,1,2,...
#    neurons: vector of units in each hidden layer
#    acts: vector activation function names, hidden layers; can be 
#       'relu','sigmoid','softmax','tanh', etc., or custom --
#       https://keras.io/api/layers/activations/
#    nClass: number of classes

krsFitImg <- function(x,y,neurons,acts,nClass,nEpoch) 
{
   y <- factorToDummies(as.factor(y),'pix',FALSE)
   x <- x / 255
   model <- keras_model_sequential()
   nrnc <- ncol(x)
   # input and first hidden layer
   layer_dense(model,units = neurons[1], activation = acts[1], 
      input_shape = c(nrnc)) 
   # further hidden layers
   nHidden <- length(neurons)
   if (nHidden > 1)
      for (i in 2:nHidden) {
         layer_dense(model,units = neurons[i], activation = acts[i])
      }
   # output layer
   layer_dense(model,units = nClass, activation = "softmax")
   # summary(model)

   compile(model,
     loss <- 'categorical_crossentropy',
     optimizer <- optimizer_rmsprop(),
     metrics <- c('accuracy')
   )

   # history <- model %>% fit(
   fitOut <- fit(model,
     x, y,
     epochs <- nEpoch, batch_size = 128,
     validation_split <- 0.2
   )

   list(model=model,fitOut=fitOut,x=x)
}

# fitOut: return value from krsFitImg()
multCol <- function(fitOut) 
{
   model <- fitOut$model
   modLayers <- model$layers
   modLayers[[length(modLayers)]] <- NULL  # output layer
   condNums <- NULL
   for (layer in modLayers) {
      layerOut <- keras_model(inputs = model$input, outputs = 
         layer$output)
      output <- predict(layerOut,fitOut$x)
      print(constCols(output))
      xpx <- t(output) %*% output
      eigs <- eigen(xpx)$values
      cN <- sqrt(eigs[1] / eigs[length(eigs)])
      condNums <- c(condNums,cN)
      print(sqrt(eigs[1] / eigs[length(eigs)]))
      # VIF comp
      z <- runif(nrow(output))
      zz <- cbind(z,output)
      zz <- as.data.frame(zz)
      lmout <- lm(z ~ .,data=zz)
      try(print(vif(lmout)))
   }
   condNums
}

