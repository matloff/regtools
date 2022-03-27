
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

   \item other: boundaryplot, nonparvsxplot

   }

\bold{Classification:}

   \itemize{

   \item unbalanced data: classadjust (see \bold{UnbalancedClasses.md})
   
   \item All vs. All: avalogtrn, avalogpred

   \item k-NN reweighting: exploreExpVars, plotExpVars, knnFineTune

   }

\bold{Machine learning (also see qeML package):}

   \itemize{

   \item k-NN: kNN, kmin, knnest, knntrn, preprocessx, meany, vary, loclin,
   predict, kmin, pwplot, bestKperPoint, knnFineTune

   \item neural networks: krsFit,multCol

   \item advanced grid search: fineTuning, fineTuningPar, plot.tuner,
      knnFineTune

   \item loss: l1, l2, MAPE, ROC

   }


\bold{Dummies and R factors Utilities:}

   \itemize{

   \item conversion between factors and dummies: dummiesToFactor,
   dummiesToInt, factorsToDummies, factorToDummies, factorTo012etc,
   dummiesToInt, hasFactors, charsToFactors, makeAllNumeric

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

   \item convert rectangular to TS: TStoX

   }

\bold{Text processing:}

   \itemize{

   \item textToXY

   }

\bold{Misc.:}

   \itemize{

   \item scaling:  mmscale, unscale

   \item data frames: catDFRow, tabletofakedf

   \item R: getNamedArgs, ulist

   \item discretize

   }


}

