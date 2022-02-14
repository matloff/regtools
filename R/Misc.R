
#######################################################################
#########################  scaling  #################################
#######################################################################

# undoes R 'scale()'

# reverses scaling on scaledx, dividing its columns by sds and adding
# ctrs; if either of the latter 2 is NULL, it is obtained via attr(), on
# the assumption that scaledx was produced from x by scale() or similar

# returns the original x; if scaledx 

unscale <- function(scaledx,ctrs=NULL,sds=NULL) {
   if (is.null(ctrs)) ctrs <- attr(scaledx,'scaled:center')
   if (is.null(sds)) sds <- attr(scaledx,'scaled:scale')
   origx <- scaledx
   for (j in 1:ncol(scaledx)) {
      origx[,j] <- origx[,j] * sds[j]
      origx[,j] <- origx[,j] + ctrs[j]
   }
   origx
}

# scale to [0,1]

# arguments:

#    m: a vector or matrix
#    scalePars: if not NULL, a 2-row matrix, with column i storing
#       the min and max values to be used in scaling column i of m;
#       typically, one has previously called mmscale() on a dataset and
#       saved the resulting scale parameters, and we want to use those
#       same scale parameters on new data
#    p: if m is a vector, specify the number of columns it should 
#       have as a matrix; code will try to take care of this by itself
#       if p is left at NULL

# value: a matrix, with column i consisting of the scaled version
#    of column i of m, and attribute as in scalePars (either copied from
#    the latter or if null, generated fresh

mmscale <- function (m,scalePars=NULL,p=NULL)
{
    # much of this code involves cases in which we have a vector but it
    # is to be treated as a matrix

    if (is.vector(m) && is.null(scalePars) && is.null(p))
       stop('specify argument p')
    if (is.null(p)) {
       if (!is.null(scalePars))
           p <- ncol(scalePars)
       else p <- ncol(m)
    }
    if (is.vector(m))
        m <- matrix(m, ncol = p)
    if (is.null(scalePars)) {
        rngs <- apply(m, 2, range)
        mins <- rngs[1, ]
        maxs <- rngs[2, ]
    }
    else {
        mins <- scalePars[1, ]
        maxs <- scalePars[2, ]
        rngs <- scalePars
    }
    ranges <- maxs - mins
    tmm <- function(i) m[, ] <- (m[, i] - mins[i])/ranges[i]
    m <- sapply(1:ncol(m), tmm)
    if (is.vector(m))
        m <- matrix(m, ncol = p)
    attr(m, "minmax") <- rngs
    m
}

#######################################################################
###################  factors and dummy variables  #####################
#######################################################################

# these routines are useful in that some regression packages insist that
# predictor be factors, while some require dummy variables

# for each column in dfr, if factor then replace by dummies,
# else just copy column; if omitLast, then dummy for last level of
# factor is not included in output

# a key point is that, for later prediction after fitting a model, one
# needs to use the same transformations; otherwise, the data to be
# predicted may be missing a level of some factor; this of course is
# especially true if one is predicting a single case

# thus the factor names and levels are saved in attributes, and can be
# used as input, via factorInfo and factorsInfo for factorToDummies()
# and factorsToDummies(): 

# factorToDummies() outputs and later inputs factorInfo
# factorsToDummies() outputs and later inputs factorsInfo

####################  factorsToDummies()  ######################

# inputs a data frame, outputs same but with all factor cols expanded to
# dummies

# arguments

#    dfr: a data frame
#    omitLast: if TRUE, make m-1 dummies for an m-level factor
#    factorsInfo: factor levels found earlier, R list, element nm
#       is levels of factor named nm
#    dfOut: if TRUE, output a data frame rather than a matrix

# if the input has cols not numeric or factor, ftn will quit

factorsToDummies <- function(dfr,omitLast=FALSE,factorsInfo=NULL,
   dfOut=FALSE)
{
   if (is.factor(dfr)) dfr <- as.data.frame(dfr)

   # for now, no input cols other than numeric, factor allowed
   ## nnf <- function(i)  (!is.numeric(dfr[,i]) && !is.factor(dfr[,i]))
   ## notnumfact <- sapply(1:ncol(dfr),nnf)
   ## if (any(notnumfact)) 
   ##    stop('non-numeric, non-factor columns encountered')

   outDF <- NULL
   nullFI <- is.null(factorsInfo)
   if (nullFI) factorsInfoOut <- list()
   for (i in 1:ncol(dfr)) {
      dfi <- dfr[,i]
      if (length(levels(dfi)) == 1 && length(dfi) > 1) {
         msg <- paste(names(dfr)[i],'constant column: ',i) 
         warning(msg)
      }
      colName <- names(dfr)[i]
      if (!is.factor(dfi)) {
         if (!is.numeric(dfi)) 
            dfi <- as.factor(dfi)
         outDF <- cbind(outDF,dfi) 
         colnames(outDF)[ncol(outDF)] <- colName
      } else {
         dumms <- factorToDummies(dfi,colName,omitLast=omitLast,
            factorInfo=factorsInfo[[colName]])
         if (nullFI) {
            factorInfo <- attr(dumms,'factorInfo')
            factorsInfoOut[[colName]] <- factorInfo
         }
         outDF <- cbind(outDF,dumms)
      }
   }
   res <- if (!dfOut) as.matrix(outDF) else outDF
   if (nullFI) {
      attr(res,'factorsInfo') <- factorsInfoOut
   }  else
      attr(res,'factorsInfo') <- factorsInfo
   res
}

####################  factorToDummies()  ######################

# converts just a single factor 

# def of omitLast is in comments above

# factorInfo is used if we are converting a factor that we've already
# converted on previous data; this argument is used to ensure that the
# conversion on the new data is consistent with the old, important for
# prediction settings

# easier to have both f, fname required

factorToDummies <- function (f,fname,omitLast=FALSE,factorInfo=NULL) 
{
    n <- length(f)
    fl <- levels(f)
    if (!is.null(factorInfo)) {
       fn <- factorInfo$fname
       if (fn != fname) stop('mismatched fname')
       ol <- factorInfo$omitLast
       if (ol != omitLast) stop('mismatChed omitLast')
       fullLevels <- factorInfo$fullLvls
       if (length(setdiff(fl,fullLevels))) 
          stop(paste('new factor level found'))
    } else fullLevels <- fl
    useLevels <- 
       if(omitLast) fullLevels[-length(fullLevels)] else fullLevels
    ndumms <- length(useLevels)
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == useLevels[i])
    colnames(dms) <- paste(fname,'.', useLevels, sep = "")
    tmp <- list()
    tmp$fname <- fname
    tmp$omitLast <- omitLast
    tmp$fullLvls <- fullLevels  # all levels even last
    attr(dms,'factorInfo') <- tmp
    dms
}

# in predicting after fitting a regression model, a factor that was fit
# may now encounter new levels, preventing prediction; this function
# reports which factors/levels, if any, are new; it should be called
# before calling predict()

# arguments:

#    info1: one of
#       the "old" data frame, used in fit (case A); 
#       an R list of levels for each factor in that frame
#          (case B); or
#       output of a qe*() call on that frame that
#          has a 'factorLevelsPresent' component
#          (future enhancement) (case C)
#    data2: "new" data frame, used in prediction

# value:

#    vector of row numbers in which a new factor level was found

checkNewLevels <- function(info1,data2) 
{ 
   tmp <- sapply(data2,is.factor)
   factorNames <- names(data2)[tmp]

   if (is.data.frame(info1)) {  # case A
     tmp <- sapply(info1,is.factor)
     tmp <- names(info1)[tmp]
     levelsPresent1 <- lapply(info1[tmp],function(t) unique(t))
   } else if (setequal(names(info1),names(data2))) {  # case B 
        levelsPresent1 <- info1  # case C
     } else {  # case C
        levelsPresent1 <- info1$factorLevelsPresent
     }

   res <- NULL
   for (nm in factorNames) {
      tmp <- setdiff(levels(data2[[nm]]),levelsPresent1[[nm]]) 
      if (length(tmp) > 0) {
         matches <- which(data2[[nm]] %in% tmp)
         res <- union(res,matches)
      }
   }
   res
}


####################  dummiesToFactor()  ######################

# makes a factor from a single related set of dummies dms; if the
# variable has k levels, inclLast = FALSE means there are only k-1
# dummies provided, so the k-th must be generated

dummiesToFactor <- function(dms,inclLast=FALSE) 
{
   dms <- as.matrix(dms)
   if (!inclLast) {
      lastCol <- 1 - apply(dms,1,sum)
      dms <- cbind(dms,lastCol)
   }
   where1s <- apply(dms,1,function(rw) which(rw == 1))
   colnames(dms) <- paste0('V',1:ncol(dms),sep='')
   nms <- colnames(dms)
   f <- nms[where1s]
   as.factor(f)
}

####################  dummiesToInt()  ######################

dummiesToInt <- function(dms,inclLast=FALSE) {
  as.numeric(dummiesToFactor(dms=dms,inclLast=inclLast))
}

# maps a factor to 0,1,2,...,m-1 where m is the number of levels of f;
# saves the levels in an attribute, e.g. for use in a later predict()
# setting; if using earlierLevels, then f will be a character vector
# with values in earlierLevels

factorTo012etc <- function(f,earlierLevels=NULL)  {
   if (!is.null(earlierLevels)) {
      checkOneValue <- function(val) which(val == earlierLevels) - 1
      return(sapply(f,checkOneValue))
   }
   tmp <- as.numeric(f)-1
   attr(tmp,'earlierLevels') <- levels(f)
   tmp
}

####################  intToDummies()  ######################

# inputs an integer vector x and creates dummies for the various values
intToDummies <- function(x,fname,omitLast=TRUE) 
{
   tmp <- as.factor(x)
   factorToDummies(tmp,fname,omitLast=omitLast)
}

####################  charsToFactors()  ######################

# inputs a data frame and converts all character columns to factors
charsToFactors <- function(dtaf) 
{
   for (i in 1:ncol(dtaf)) {
      cli <- dtaf[,i]
      if (is.character(cli)) {
         dtaf[,i] <- as.factor(cli)
      }
   }
   dtaf
}

####################  xyDataframeToMatrix()  ######################

# inputs a data frame intended for regression/classification, with X in
# the first cols and Y in the last; converts all factors to dummies, and
# outputs a matrix; in creating dummies, r-1 are retained for r levels,
# except for Y

# see also toAllNumeric() below

xyDataframeToMatrix <- function(xy) {
   p <- ncol(xy)
   x <- xy[,1:(p-1)]
   y <- xy[,p]
   xd <- factorsToDummies(x,omitLast=TRUE)
   yd <- factorToDummies(y,'y',omitLast=FALSE)
   as.matrix(cbind(xd,yd))
}

####################  hasFactors()  ######################

# x is a data frame; returns TRUE if at least one column is a factor
hasFactors <- function(x) 
{
   for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) return(TRUE)
   }
   FALSE
}

####################  hasCharacters()  ######################

# dfr is a data frame; returns TRUE if at least one column is in character mode
hasCharacters <- function(dfr) 
{
   for (i in 1:ncol(dfr)) {
      if (is.character(dfr[,i])) return(TRUE)
   }
   FALSE
}

####################  toSuperFactor()  ######################

# say we have a factor f1, then encounter f2, with levels a subset of
# those of f1; we want to change f2 to have the same levels as f1; seems
# that this can NOT be done via levels(f2) <- levels(f1); typically used
# in predict() functions
toSuperFactor <- function(inFactor,superLevels) 
{
   inFactorChars <- as.character(inFactor)
   extraChars <- setdiff(superLevels,inFactorChars)
   nExtra <- length(extraChars)
   if (nExtra == 0) return(inFactor)
   newInFactorChars <- c(inFactorChars,extraChars)
   nExtra <- length(extraChars)
   tmp <- as.factor(newInFactorChars)
   nTmp <- length(tmp)
   start <- nTmp - nExtra + 1
   end <- nTmp
   tmp[-(start:end)]
}

####################  toSubFactor()  ######################

# here we have a factor f with various levels, but want to lump all
# levelsl but the ones in saveLevels to a new level, lumpedLevel; the
# default for the latter is 'zzzOther', chosen to ensure that the lumped
# level is last
toSubFactor <- function(f,saveLevels,lumpedLevel='zzzOther') 
{
   lvls <- levels(f)
   fChar <- as.character(f)
   other <- setdiff(lvls,saveLevels)
   whichOther <- which(fChar %in% other)
   fChar[whichOther] <- lumpedLevel
   as.factor(fChar)
}

####################  toAllNumeric()  ######################

# change character variables to factors, then all factors to dummies,
# recording factorInfo for later use in prediction; put result in wm

# w: data frame
# factorsInfo: value found in a previous call 

toAllNumeric <- function(w,factorsInfo=NULL)
{
   if (hasCharacters(w)) {
      stop('character variables currently not supported')
      ## w <- charsToFactors(w)
   }
   if (hasFactors(w)) {
      wm <- factorsToDummies(w,omitLast=TRUE,factorsInfo=factorsInfo)
      attr(wm,'factorsInfo')
   } else {
      wm <- w
   }
   wm
} 

#######################################################################
###################  misc. data frame/matrix ops  ######################
#######################################################################

# multiply x[,cols] by vals, e.g. x[,cols[1]] * vals[1]
# code by Bochao Xin
multCols <- function(x,cols,vals) {
#     tx <- t(x[,cols])
#     x[,cols] <- t(tx*vals)
#    for (i in 1:length(cols)) {
#       cl <- cols[i]
#       x[,cl] <- x[,cl] * vals[1,i]
#    }
   tx <- t(x[,cols])
   x[,cols] <- t(tx*vals)
   x
}

# check for constant cols  

# d is a matrix or data frame; returns empty vector (i.e. length == 0)
# if no cols are constant, otherwise indices of those that are constant

constCols <- function(d) {
   if (is.matrix(d)) d <- as.data.frame(d)
   nDistinct <- sapply(lapply(d, unique), length)
   return(which(nDistinct == 1))
}

# print a data frame row

catDFRow <- function(dfRow) {
  for (i in 1:ncol(dfRow)) {
     cat(as.character(dfRow[1,i]),' ')
  }
}

# print the classes of a data frame

getDFclasses <- function(dframe) {
   tmp <- sapply(1:ncol(dframe),function(i) class(dframe[,i]))
   names(tmp) <- names(dframe)
   tmp
}

# check whether all elements of a list, including a data frame, are
# numeric

allNumeric <- function(lst) 
{
   tmp <- sapply(lst,is.numeric)
   all(tmp) 
}

#######################################################################
######################  misc. lm() routines  #######################
#######################################################################

# computes the standard error of the predicted Y for X = xnew

stdErrPred <- function(regObj,xnew) {
   xx <- c(1,xnew)  # the 1 accounts for the intercept term
   xx <- as.numeric(xx)  # in case xnew was a row in a data frame
   as.numeric(sqrt(xx %*% vcov(regObj) %*% xx))
}

#######################################################################
######################  misc. graphics ################################
#######################################################################

# "3-D" graphs of (x,y,z), where (x,y) points are plotted in 2-D for
# various values of z; pts connected by lines, with z values displayed
# at the connection points; grouping is allowed, specified via a 4th
# column if desired

# arguments:

#    xyz: matrix/df consisting of x, y and z above, possible a 4th; in
#    latter case, xyz must be a data frame
#    clrs: colors for the various lines; default uses heat.colors()
#    xlim, ylim: as in R plot(); default uses largest ranges
#    xlab,ylab: as in R plot()
#    legendPos: first argument to legend(), e.g. 'topright'

xyzPlot <- function(xyz,clrs=NULL,cexText=1.0,
   xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,legendPos=NULL,plotType='l') 
{
   if (is.null(xlim)) xlim <- range(xyz[,1])
   if (is.null(ylim)) ylim <- range(xyz[,2])
   if (is.null(xlab)) xlab <- 'x'
   if (is.null(ylab)) ylab <- 'y'
   oneLine <- (ncol(xyz) == 3)
   if (is.null(clrs)) {
      if (oneLine) clrs <- 'black'
      else clrs <- heat.colors(length(unique(xyz[,4]))) 
   }

   if (plotType == 'l')  
      # so that lines "move to the right," rather than a jumble
      xyz <- xyz[order(xyz[,1]),]

   nr <- nrow(xyz)
   lineGrps <- 
      if (oneLine) list(1:nr)
      else split(1:nr,xyz[,4])
   nGrps <- length(lineGrps)

   # plot(xyz[lineGrps[[1]],1:2],type=plotType,col=clrs[1],
   plot(1,
      xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,cex=0.1)
   ### if (nGrps > 1)
      for (i in 1:nGrps) {
         if (plotType == 'l') {
            lines(xyz[lineGrps[[i]],1:2],type='l',col=clrs[i])
         }
         else
            points(xyz[lineGrps[[i]],1:2],col=clrs[i],cex=0.1)
      }

   for (i in 1:nGrps) {
      lns <- xyz[lineGrps[[i]],]
      text(lns[,1],lns[,2],lns[,3],cex=cexText,col=clrs[i])
   }

   # add legend
   if (!oneLine && !is.null(legendPos)) {
      legend(legendPos,legend=unique(xyz[,4]),col=clrs,lty=1)
   }
}

# print current image to file
prToFile <- function (filename)
{
    origdev <- dev.cur()
    parts <- strsplit(filename,".",fixed=TRUE)
    nparts <- length(parts[[1]])
    suff <- parts[[1]][nparts]
    if (suff == "pdf") {
        pdf(filename)
    }
    else if (suff == "png") {
        png(filename,bg='white')
    }
    else jpeg(filename)
    devnum <- dev.cur()
    dev.set(origdev)
    dev.copy(which = devnum)
    dev.set(devnum)
    dev.off()
    dev.set(origdev)
}

#######################################################################
######################  PCA routines ##################################
#######################################################################

####################  PCAwithFactors()  ###############################

# allows use of prcomp() with data that may include R factors

# arguments:

#    x: a data frame
#    nComps: number of PC components to use

# value:  object of class 'PCAwithFactors', with components as follows

#    pcout: output of calling prcomp() on toAllNumeric(x)
#    factorsInfo: attr from call to toAllNumeric(), if any; needed for
#       predict.PCAwithFactors()
#    preds: PCA version of x

# note: scaling will be applied, using mmscale()

PCAwithFactors <- function(x,nComps=ncol(x)) 
{
   if (!is.data.frame(x)) stop('x must be a data frame')
   namesOrigX <- names(x)
   nColsOrigX <- ncol(x)
   factorIdxs <- which(sapply(x,is.factor))
   if (identical(factorIdxs,1:ncol(x)))
      stop('case of all-factor data not supported yet')
   xscale <- mmscale(x[,-factorIdxs])
   minmax <- attr(xscale,'minmax')
   x[,-factorIdxs] <- xscale
   if (length(factorIdxs) > 0) {
      x <- factorsToDummies(x)
      factorsInfo <- attr(x,'factorsInfo')
   }
   pcout <- prcomp(x)
   xpca <- predict(pcout,x)[,1:nComps]
   res <- list(pcout=pcout,xpca=xpca,factorsInfo=factorsInfo,
      namesOrigX=namesOrigX,nColsOrigX=nColsOrigX,factorIdxs=factorIdxs,
      minmax=minmax,nComps=nComps)
   class(res) <- 'PCAwithFactors'
   res
}

# find PCA rep of newx; newx in original scale, output in scale of 
# mmscale() + PCA

predict.PCAwithFactors <- function(object,newx) 
{
   if (!identical(names(newx),object$namesOrigX))
      stop('column names mismatch')
   factorIdxs <- object$factorIdxs
   tmp <- as.matrix(newx[,-factorIdxs])
   p <- object$nColsOrigX - length(factorIdxs)
   newxscale <- mmscale(tmp,object$minmax,p=p)
   if (nrow(newx) == 1) newxscale <- newxscale[1,]
   newx[,-factorIdxs] <- newxscale
   newx <- factorsToDummies(newx,factorsInfo=object$factorsInfo)
   # unfortunately, cannot use predict.prcomp(), as it scales
   preds <- as.matrix(newx) %*% object$pcout$rotation
   preds[,1:object$nComps]
}

#########################  doPCA()  ##########################

# call prcomp(x,pcaProp), transform x to PCs, with the number of the
# latter being set according to the top pcaProp proportion of variance

doPCA <- function(x,pcaProp) 
{
   pcaout <- prcomp(x,scale.=TRUE)
   xpca <- predict(pcaout,x)
   xNames <- colnames(xpca)
   pcVars <- pcaout$sdev^2
   ncx <- ncol(xpca)
   csums <- cumsum(pcVars)
   csums <- csums/csums[ncx]
   numPCs <- min(which(csums >= pcaProp))
   xpca <- xpca[,1:numPCs]
   newData <- as.data.frame(xpca)
   names(newData) <- xNames[1:numPCs]
   list(pcaout=pcaout,numPCs=numPCs,newData=newData)
}

#######################################################################
######################  misc. other #####################################
#######################################################################

######################  misc. list ops ################################

# assign list components to individual variables of the same names

# similar to unpack() in zeallot pkg

ulist <- function(lst) 
{
   nms <- names(lst)
   if (any(nms == '')) stop('missing list name')
   tmp <- substitute(for (nm in nms) assign(nm,lst[[nm]]))
   eval(tmp,parent.frame())
}


#######################  loss functions  ###############################

# mean absolute prediction error
MAPE <- function(yhat,y) 
{
   if (!is.vector(y) || !is.vector(yhat)) 
      stop('inputs must be vectors')
   mean(abs(yhat-y))
}



# overall error rate

# either 

#    y is a vector of 0s and 1s, 
#    yhat a vector of estimated probabilities of 1

# or

#    y is a vector of numeric class labels, starting at 1 or 0
#    yhat is a matrix, with y[i,j] = prob of Y = j+startAt1-1

probIncorrectClass <- function(yhat,y,startAt1=TRUE
) 
{
   if (is.factor(y) || is.factor(yhat)) 
      stop('vector/matrix inputs only')
   if (is.vector(yhat)) {
      yhat <- round(yhat)
      return(mean(yhat != y))
   }
   classPred <- apply(yhat,1,which.max) 
   classActual <- y - startAt1 + 1
   mean(classPred != classActual)
}

# proportion misclassified; deprecated in favor of probIncorrectClass
propMisclass <- function(y,yhat) 
{
   if (!is.vector(y) && !is.factor(y)) 
      stop('predicted classes must be a vector or factor')
   mean(y != yhat)
}

# included lossFtn choices are MAPE and probIncorrectClass; user may
# supply others
findOverallLoss <- function (regests, y, lossFtn = MAPE) 
{
   loss1row <- function(regestsRow) lossFtn(y, regestsRow)
   apply(regests, 1, loss1row)
}

#########################  other misc.  ################################

# convenience wrapper for replicate; finds the means and standard errors
# and the nrep replication; toReplic can be a vector 

# arguments:

#    nrep:  number of replications
#    toReplic:  expression to replicate; use braces and semicolons if
#       more than one statement; must return either a number or a vector
#       of numbers
#    timing:  if TRUE, apply system.time() to each replication, and
#       calculate the mean elapsed time

# value:

#   mean outcomes of the simulation, plus an attribute storing the 
#   associated standard errors

replicMeans <- function(nrep,toReplic,timing=FALSE) {
   if (timing) {
      tmp <- paste0('{tm <- system.time(z <- ',toReplic,'); ')
      tmp <- paste0(tmp,'c(tm[3],z)}')
      toReplic <- tmp
   }
   cmd <- paste0('replicate(',nrep,',',toReplic,')')
   cmdout <- eval.parent(parse(text=cmd))
   # cmdout <- eval(parse(text=cmd))
   # if toReplic returns a vector, cmdout will be a matrix; to handle
   # this, make it a matrix anyway
   if (!is.matrix(cmdout)) cmdout <- matrix(cmdout,ncol=nrep)
   meancmdout <- rowMeans(cmdout)
   attr(meancmdout,'stderr') <- apply(cmdout,1,sd) / sqrt(nrep)
   meancmdout
}

# convenience wrapper for cut() 

# arguments:

#    x: numeric vector
#    endpts: endpoints for the desired intervals, treated as open on the
#       left and closed on the right; to avoid NA values, make sure all
#       of x is accommodated

# value:

#    discrete version of x, with values 1,2,3,...; will have an R
#    attribute, 'endpts', so as to remember which ones we used

discretize <- function(x,endpts)
{
   xc <- cut(x,endpts,labels=1:(length(endpts)-1))
   attr(xc,'endpts') <- endpts
   xc
}

require(gtools)

# the problem with strsplit('a  b') is that it yields (in [[1]]
# component) 'a','','b'; the version below doesn't give any empty
# strings
pythonBlankSplit <- function(s)
{
   tmp <- strsplit(s,' ')[[1]]
   tmp[tmp != '']
}


# use this after doing error checking, giving the user the choice of
# leaving, or continuing in the debugger
stopBrowser <- defmacro(msg,expr=
   {
   cat(msg,'\n')
   d <- readline('hit Enter to leave, or d to enter debugger: ')
   if (d == '') stop('')
   browser()
   }
)

