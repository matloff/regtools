
##############  8/28/21:  preparing to mv all this back to rectools

###  
###  # recommender systems tools; partially adapted from rectools pkg
###  
###  
###  ##################  ops to group recsys data  ############################
###  
###  # input raw data in standard format,
###  
###  #    (user ID, item ID, rating)
###  
###  # and form an R list for user data, with class 'usrData'; each element
###  # of the list will be of class 'usrDatum', representing one user's data,
###  # and will have components as seen in 'value' below 
###  
###  # arguments:
###  
###  #    ratingsIn: input data, whose first 3 cols are user ID, item ID
###  #               and rating 
###  
###  # value:
###  
###  #    object of class 'usrData': an R list with one element per user;
###  #    each such element is itself an R list, an object of class
###  #    'usrDatum', with these components:
###  #
###  #       userID:  the ID of this user
###  #       ratings:  vector of ratings made by this user
###  #       itms:  IDs of items rated by this user
###  
###  groupUserData <- function(ratingsIn) 
###  {
###  
###     # IMPORTANT NOTE: in order to work in cross-validation, etc. we need
###     # to abandon the idea of having the user IDs start at 1 and be
###     # consecutive; instead, we will just use the ID numbers as list
###     # indices; e.g. if we have users numbered 2,8,85 then retval below
###     # will consist of retval[[2]], retval[[8]] and retval[[85]]
###  
###     # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
###     rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
###     nusers <- length(rownums)
###     nitems <- length(unique(ratingsIn[,2]))
###  
###     # retval will ultimately be the return value, a list of lists as
###     # described above.
###     retval <- list()
###  
###     for (i in 1:nusers) {
###        whichrows <- rownums[[i]]  # row nums in ratingsIn for user i
###        userID <- chr(ratingsIn[whichrows[1],1])
###        # start building usrDatum object for this user
###        retval[[userID]] <- list()
###        retval[[userID]]$userID <- userID
###        retval[[userID]]$itms <- ratingsIn[whichrows,2]
###        retval[[userID]]$ratings <- ratingsIn[whichrows,3]
###        names(retval[[userID]]$ratings) <- chr(retval[[userID]]$itms) 
###  
###        class(retval[[userID]]) <- 'usrDatum'
###     }
###     class(retval) <- 'usrData'
###     retval
###  }
###  
###  # analog of groupUserData() for items
###  
###  groupItemData <- function(ratingsIn) 
###  {
###  
###     # IMPORTANT NOTE: in order to work in cross-validation, etc. we need
###     # to abandon the idea of having the item IDs start at 1 and be
###     # consecutive; instead, we will just use the ID numbers as list
###     # indices; e.g. if we have items numbered 2,8,85 then retval below
###     # will consist of retval[[2]], retval[[8]] and retval[[85]]
###  
###     # rownums[[i]] will be the row numbers in ratingsIn belonging to item i
###     rownums <- split(1:nrow(ratingsIn),ratingsIn[,2])
###     nitems <- length(rownums)
###     nusers <- length(unique(ratingsIn[,1]))
###  
###     # retval will ultimately be the return value, a list of lists as
###     # described above.
###     retval <- list()
###  
###     for (i in 1:nitems) {
###        whichrows <- rownums[[i]]  # row nums in ratingsIn for item i
###        itemID <- chr(ratingsIn[whichrows[1],2])
###        # start building itmDatum object for this user
###        retval[[itemID]] <- list()
###        retval[[itemID]]$itemID <- itemID
###        retval[[itemID]]$usrs <- ratingsIn[whichrows,1]
###        retval[[itemID]]$ratings <- ratingsIn[whichrows,3]
###        names(retval[[itemID]]$ratings) <- as.character(retval[[itemID]]$itms) 
###  
###        class(retval[[itemID]]) <- 'itmDatum'
###     }
###     class(retval) <- 'itmData'
###     retval
###  }
###  
###  #########################  knnRec()  ################################
###  
###  # arguments: ratings data in standard (user,item,rating) form
###  
###  # value: R list, user and item data, class 'knnRec'
###  
###  knnRec <- function(ratings) 
###  {
###     obj <- list()
###     obj$userData <- groupUserData(ratings)
###     obj$itemData <- groupItemData(ratings)
###     obj$overallMean <- mean(ratings[,3])
###     obj$users <- names(obj$userData)
###     obj$items <- names(obj$itemData)
###     class(obj) <- 'knnRec'
###     obj
###  }
###  
###  # note: prediction is from the user's point of view; we find users in
###  # our training data similar to the given user etc.
###  
###  # arguments:
###  
###  #   userData, itemData: outputs of groupUserData(), groupItemData()
###  #   user,item: the query pair
###  #   k: number of nearest neighbors
###  #   minMatch: 2 users won't be compared unless that have rated
###  #      at least this many items in common
###  
###  #   value: predicted rating
###  
###  predict.knnRec  <- function(object,user,item,k,minMatch=1)
###  {
###     userData <- object$userData
###     itemData <- object$itemData
###     if (minMatch > 1) stop('general minMatch not yet implemented')
###     charUser <- chr(user)
###     if (!(charUser %in% object$users)) {
###        warning('new user countered, returning overall mean')
###        return(object$overallMean)
###     }
###     charItem <- chr(item)
###     if (!(charItem %in% object$items)) {
###        warning('new item countered, returning overall mean')
###        return(object$overallMean)
###     }
###     uDatum <- userData[[chr(user)]]
###     udItems <- uDatum$itms
###     if (item %in% udItems) {
###        i <- which(udItems == item)
###        return(uDatum$ratings[i])
###     }
###  
###     haveRated <- itemData[[chr(item)]]$usrs
###     if (length(haveRated) == 0) warning(paste('no users rated item',item))
###     cd <- function(usrId) {
###        usrId <- chr(usrId)
###        dist <- cosDist(uDatum,userData[[usrId]])
###        c(usrId,dist)
###     }
###     dists <- sapply(haveRated,cd)
###     dists <- as.numeric(dists)
###     dists <- matrix(dists,nrow=2)
###     if (k > ncol(dists)) {
###        k <- ncol(dists)
###        warning('k reduced, too few neighbors')
###     }
###     tmp <- order(dists[2,])[1:k]
###     knear <- dists[1,][tmp]
###     getUij <- function(usr) 
###     {
###        ud <- userData[[chr(usr)]]
###        getUserRating(ud,item)
###     }
###     tmp <- sapply(knear,getUij)
###     mean(tmp,na.rm=TRUE)
###  }
###  
###  cosDist <- function(x,y)
###  {  
###     # rated items in common
###     commItms <- intersect(x$itms,y$itms)
###     if (length(commItms)==0) return(NaN)
###     # where are those common items in x and y?
###     xwhere <- which(!is.na(match(x$itms,commItms))) 
###     ywhere <- which(!is.na(match(y$itms,commItms))) 
###     xvec <- x$ratings[xwhere]
###     yvec <- y$ratings[ywhere]
###     xvec %*% yvec / (l2a(xvec) * l2a(yvec))
###  }
###  
###  #####################  anovaRec()  #################################
###  
###  # phrased in terms of ANOVA, but just common sense adjustments
###  # 
###  # Y_ij = mu + user_i + movie_j + generk_k + user.genre _ik
###  # 
###  # use the word "propensity" to mean differences in means
###  # 
###  # user_i is the extra propensity, beyond mu, for user i to like movies; 
###  # genre_k is the extra propensity, beyond mu, for genre k to be liked
###  # user.genre _ik is the further extra propensity for user i to like 
###  #    movies in genre k
###  
###  anovaRec <- function(ratingsDF,userCvrs=NULL,itemCvrs=NULL) 
###  {
###     res <- list()  # will ultimately be the return value
###     userID <- ratingsDF[,1]
###     itemID <- ratingsDF[,2]
###     ratings <- ratingsDF[,3]
###  
###     tmp <- getUsrItmMainEffects(ratings,userID,itemID)
###     ulist(tmp)  # userMainEffects,itemMainEffects, overallMean)
###     tmp <- getCvrXEffects(ratings,ratingsDF,userCvrs,itemCvrs,overallMean,
###        userMainEffects,itemMainEffects) 
###     ulist(tmp)  # cvrMainEffects,userCvrXEffects,itemCvrXEffects
###     res$overallMean <- overallMean
###     res$userMainEffects <- userMainEffects
###     res$itemMainEffects <- itemMainEffects
###     res$cvrMainEffects <- cvrMainEffects
###     res$userCvrXEffects <- userCvrXEffects
###     res$itemCvrXEffects <- itemCvrXEffects
###     class(res) <- 'anovaRec'
###     res
###  }
###  
###  getUsrItmMainEffects <- function(ratings,userID,itemID) 
###  {
###     overallMean <- mean(ratings)
###     usermeans <- tapply(ratings,userID,mean)
###     userMainEffects <- usermeans - overallMean 
###     itemmeans <- tapply(ratings,itemID,mean)
###     itemMainEffects <- itemmeans - overallMean 
###     list(userMainEffects=userMainEffects,itemMainEffects=itemMainEffects,
###        overallMean=overallMean)
###  }
###  
###  getCvrXEffects <- function(ratings,ratingsDF,userCvrs,itemCvrs,overallMean,
###        userMainEffects,itemMainEffects) 
###  {
###     cvrMainEffects <- list()
###     userCvrXEffects <- list()
###     itemCvrXEffects <- list()
###     usrcolname <- names(ratingsDF)[1]
###     for (usercvr in userCvrs) {
###        cvrmeans <- tapply(ratings,ratingsDF[[usercvr]],mean)
###        cvrMainEffects[[usercvr]] <- cvrmeans - overallMean
###        tmp <- 
###           tapply(ratings,list(ratingsDF[,usrcolname],ratingsDF[,usercvr]),mean)
###        tmp <- tmp - overallMean
###        for (i in 1:nrow(tmp)) {
###           usr <- rownames(tmp)[i]
###           tmp[i,] <- tmp[i,] - userMainEffects[[usr]]
###        }
###        for (k in 1:ncol(tmp)) {
###           cvr <- colnames(tmp)[k]
###           tmp[,k] <- tmp[,k] - cvrMainEffects[[usercvr]][cvr]
###        }
###        userCvrXEffects[[usercvr]] <- tmp
###     }
###     itmcolname <- names(ratingsDF)[2]
###     for (itemcvr in itemCvrs) {
###        cvrmeans <- tapply(ratings,ratingsDF[[itemcvr]],mean)
###        cvrMainEffects[[itemcvr]] <- cvrmeans - overallMean
###        tmp <- 
###           tapply(ratings,list(ratingsDF[,itmcolname],ratingsDF[,itemcvr]),mean)
###        tmp <- tmp + overallMean
###        for (i in 1:nrow(tmp)) {
###           itm <- rownames(tmp)[i]
###           tmp[i,] <- tmp[i,] - itemMainEffects[[itm]]
###        }
###        for (k in 1:ncol(tmp)) {
###           cvr <- colnames(tmp)[k]
###           tmp[,k] <- tmp[,k] - cvrMainEffects[[itemcvr]][cvr]
###        }
###        itemCvrXEffects[[itemcvr]] <- tmp
###     }
###     if (is.null(userCvrs)) userCvrXEffects <- NULL
###     if (is.null(itemCvrs)) itemCvrXEffects <- NULL
###     list(cvrMainEffects=cvrMainEffects,userCvrXEffects=userCvrXEffects,
###        itemCvrXEffects=itemCvrXEffects)
###  
###  }
###  
###  # predict a single (user,item) pair; cvrs is an R list with component
###  # names as in the ratingsDF input to anovaRec()
###  
###  predict.anovaRec <- function(object,user,item,userCvrVals=NULL,itemCvrVals=NULL) 
###  {
###     pred <- object$overallMean
###     pred <- pred + object$userMainEffects[chr(user)]
###     pred <- pred + object$itemMainEffects[chr(item)]
###     if (!is.null(userCvrVals)) {
###        if (!is.list(userCvrVals)) stop('userCvrVals must be a list')
###        nms <- names(userCvrVals)
###        for (nm in nms) {
###           col <- userCvrVals[[nm]]  # value of this covariate
###           pred <- pred + object$cvrMainEffects[[nm]][chr(col)]
###           mat <- object$userCvrXEffects[[nm]]
###           pred <- pred + mat[chr(user),chr(col)]
###        }
###     }
###     if (!is.null(itemCvrVals)) {
###        if (!is.list(itemCvrVals)) stop('itemCvrVals must be a list')
###        nms <- names(itemCvrVals)
###        for (nm in nms) {
###           col <- itemCvrVals[[nm]]   # value of this covariate
###           pred <- pred + object$cvrMainEffects[[nm]][chr(col)]
###           mat <- object$itemCvrXEffects[[nm]]
###           pred <- pred + mat[chr(item),chr(col)]
###        }
###     }
###     pred
###  }
###  
###  #########################  mfRec()  ################################### 
###  
###  # collaborative filtering based on matrix factorization; wrappers for 
###  # recosystem package
###  
###  # arguments:
###  
###  #    ratings: as in knnRec() above
###  #    rnk: desired matrix rank
###  #    niter: number of iterations
###  #    lambda: L1 regularization parameter
###  
###  # value:
###  
###  # object of class 'mfRec', with components P, Q, where A approx= P'Q
###  
###  mfRec <- function(ratings,rnk=10,nmf=FALSE,niter=20,lambda=0) 
###  {
###     require(recosystem)
###     # pkg assumes user, item IDs are 1,2,3,..., so need a lookup table,
###     # both here and in prediction
###     userIDs <- unique(ratings[,1])
###     ratings[,1] <- match(ratings[,1],userIDs)
###     itemIDs <- unique(ratings[,2])
###     ratings[,2] <- match(ratings[,2],itemIDs)
###     r <- Reco()
###     train_set <-
###        data_memory(ratings[,1],ratings[,2],ratings[,3],index1=TRUE)
###     r$train(train_set,opts = list(dim=rnk,nmf=nmf,niter=niter,costp_l1=lambda))
###     result <- r$output(out_memory(),out_memory())
###     result$overallMean <- mean(ratings[,3])
###     result$userIDs <- userIDs
###     result$itemIDs <- itemIDs
###     class(result) <- 'mfRec'
###     result$r <- r
###     # check overdetermined
###     nu <- length(userIDs)
###     ni <- length(itemIDs)
###     ncoeffs <- rnk * (nu+ni)
###     if (ncoeffs > nu * ni) 
###        warning('overdetermined system')
###     result
###  }
###  
###  # if item = -m, report the m top-rated items
###  predict.mfRec <- function(object,user,item) 
###  {
###     user <- match(user,object$userIDs)
###     if (is.na(user)) {
###        warning('no such user, returning mean')
###        return(object$overallMean)
###     }
###     m <- item
###     if (m > 0) {
###        item <- match(item,object$itemIDs)
###        if (is.na(item)) {
###           warning('no such item, returning mean')
###           return(object$overallMean)
###        }
###     }
###  
###     # set up classical A approx= WH
###     w <- object$P
###     h <- t(object$Q)    # classic H
###  
###     if (m > 0) {
###        pred <- w[user,] %*% h[,item]
###        if (is.nan(pred)) {
###           warning('NaN, predicting using overall mean')
###           pred <- object$overallMean
###        }
###     } else {
###        wrow <- w[user,]
###        if (any(is.nan(wrow)) || any(is.nan(h)))
###           stop("NaNs, can't compute")
###        preds <- as.vector(wrow %*% h)
###        tmp <- order(preds,decreasing=TRUE)
###        tmpM <- tmp[1:(-m)]
###        pred <- preds[tmpM]
###        names(pred) <- tmpM
###     }
###  
###     pred
###  
###  }
###  
###  ########################  utilities  ###################################
###  
###  # abbreviation for as.character()
###  chr <- function(i) as.character(i)
###  
###  # find row in matrix/df m for which m[,aCol] = aVal, m[,bCol] = bVal;
###  # aCol, bCol can be either numeric column numbers or column names
###  findRows <- function(m,aCol,aVal,bCol,bVal) {
###     aWhich <- which(m[,aCol] == aVal)
###     bWhich <- which(m[,bCol] == bVal)
###     intersect(aWhich,bWhich)
###  }
###  
###  # l2 norm
###  l2a <- function(x) sqrt(x %*% x)
###  
###  # get rating of item j from output of groupUserData()
###  getUserRating <- function(userData,j) 
###  {
###     itmList <- userData$itms
###     pos <- which(itmList == j)
###     if (length(pos) == 0) {
###        warning('no such item')
###        return(NA)
###     }
###     userData$ratings[pos]
###  }
###  
###  # the above predict.* do one (user,item) pair at a time (or for Reco,
###  # one user at a time); this function predicts a bunch of (user,item)
###  # pairs, no covariates
###  
###  # arguments:
###  
###  #    object: an object of class '*Rec', e.g. 'knnRec' 
###  #    newxs: a data frame containing user and item columns of the same 
###  #       names as the 'ratings' input that produced 'object'
###  
###  # value: a vector of predicted values
###  
###  predictMany <- function(object,newxs) 
###  {
###     prednewx <- function(i) predict(object,newxs[i,1],newxs[i,2])
###     res <- sapply(1:nrow(newxs),prednewx)
###     # if output of sapply() is a list instead of a vector, probably
###     # either the original fit was done incorrectly or there is a data
###     # problem
###     if (is.list(res))
###        stop('preds is a list instead of a vector, likely incorrect fit')
###     res
###  }
###  
###  ####################  misc. RecSys  ###################################
###  
###  # converts the adjacency matrix of a unidirectional bipartite graph to
###  # (user,item,rating) format, with rating = 0,1
###  
###  adjToUIR <- function(adj) 
###  {
###     if (is.data.frame(adj)) adj <- as.matrix(adj)
###     rws <- as.vector(row(adj))
###     cls <- as.vector(col(adj))
###     adjvec <- as.vector(adj)
###     cbind(rws,cls,adjvec)
###  }
###  
