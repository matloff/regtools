
\name{regtools-package}
\alias{regtools-package}
\alias{regtools}
\docType{package}         

\title{Overview and Package Reference Guide}
                                                                                \description{ 

This package provides a broad collection of functions useful for
regression and classification analysis, and machine learning.

}

\section{Function List}{

\bold{Parametric modeling:}

   \itemize{

   \item nonlinear regression:  nlshc

   \item ridge regression:  ridgelm, plot

   \item missing values (also see our \pkg{toweranNA} package):
   lmac,makeNA,coef.lmac,vcov.lmac,pcac

   }

\bold{Diagnostic plots:}

   \itemize{

   \item regression diagnostics:  parvsnonparplot, nonparvsxplot,
   nonparvarplot

   \item other: boundaryplot, pwplot, nonparvsxplot

   \item linear multiclass prediction: linClass

   }

\bold{Classification:}

   \itemize{

   \item unbalanced data: classadjust (see \bold{UnbalancedClasses.md})
   
   \item All vs. All: avalogtrn, avalogpred

   \item One vs. All wrappers: knnClass, linClass, logitClass

   \item k-NN reweighting: exploreExpVars, plotExpVars, knnFineTune

   \item quick convenience classification functions: logitClass,
   knnClass, linClass, rfClass, boostClass

   }

\bold{Machine learning:}

   \itemize{

   \item k-NN: kNN, kmin, knnest, knntrn, preprocessx, meany, vary, loclin,
   predict, kmin, pwplot, bestKperPoint, knnFineTune

   \item neural networks: krsFit,multCol

   \item advanced grid search: fineTuning, fineTuningPar, plot.tuner,
      knnFineTune

   \item dimension reduction: dimRed, dimRedNewX, reduceCols

   \item probability calibration: scoresToProbs

   \item loss: l1, l2, MAPE

   }

\bold{Quick-explore machine learning:}

   \itemize{

   \item qeLogit, qeKNN, qeLin, qeRF, qeGBoost, qeSVM, qeNeural,
      qeLASSO, qePoly

   }

\bold{Dummies and R factors:}

   \itemize{

   \item conversion between factors and dummies: dummiesToFactor,
   dummiesToInt, factorsToDummies, factorToDummies, factorTo012etc,
   dummiesToInt, hasFactors, charsToFactors

   \item dealing with superset and subsets of factors: toSuperFactor,
   toSubFactor

   }

\bold{Statistics:}

   \itemize{

   \item mm

   }

\bold{Matrix:}

   \itemize{

   \item multCols, constCols

   }

\bold{Time series:}

   \itemize{

   \item convert rectangular to TS: TStoMat, TStoX

   }

\bold{Image processing:} 

   \itemize{

   \item imgTo2D

   \item imgFilesToMatrix,imgFileToVector

   \item image data augmentation: augMatrix

   \item download data: getMNIST

   }

\bold{Text processing:}

   \itemize{

   \item textToXY

   }

\bold{Recommender systems:}

   \itemize{

   \item knnRec, anovaRec, mfRec

   \item predictMany

   \item adjToUIR

   }

\bold{Misc.:}

   \itemize{

   \item scaling:  mmscale, unscale

   \item data frames: catDFRow, tabletofakedf

   \item R: getNamedArgs, ulist

   \item discretize

   }


}

