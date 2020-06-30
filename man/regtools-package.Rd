
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

(under construction)

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

   }

\bold{Classification:}

   \itemize{

   \item unbalanced data: classadjust (see \bold{UnbalancedClasses.md})
   
   \item One vs. All, All vs. All: avalogtrn, avalogpred, ovalogtrn,
   ovalogpred 

   }

\bold{Machine learning:}

   \itemize{

   \item k-NN: kNN, kmin, knnest, knntrn, preprocessx, meany, vary, loclin,
   predict, kmin, pwplot, bestKperPoint

   \item neural networks: krsFit,multCol

   \item advanced grid search: fineTuning, fineTuningPar, plot.tuner

   \item image data augmentation: augMatrix

   \item loss: l1, l2, MAPE

   }

\bold{Dummies and R factors:}

   \itemize{

   \item conversion between factors and dummies: dummiesToFactor,
   dummiesToInt, factorsToDummies, factorToDummies, factorTo012etc,
   dummiesToInt,hasFactors

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

   \item augMatrix

   }

\bold{Misc.:}

   \itemize{

   \item scaling:  mmscale, unscale

   \item data frames: catDFRow, tabletofakedf

   \item R: getNamedArgs

   }


}

