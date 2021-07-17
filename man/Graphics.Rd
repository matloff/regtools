\name{xyzPlot}
\alias{xyzPlot}

\title{Misc. Graphics}

\description{
Graphics utiliites.
}

\usage{
xyzPlot(xyz,clrs=NULL,cexText=1.0,xlim=NULL,ylim=NULL,
   xlab=NULL,ylab=NULL,legendPos=NULL,plotType='l') 
}

\arguments{
  \item{xyz}{A matrix or data frame of at least 3 columns, the first
     3 serving as 'x', 'y' and 'z' coordinates of points to be plotted. 
     Grouping, if any, is specified in column 4, in which case \code{xyz}
     must be a data frame.}
  \item{clrs}{Colors to be used in the grouped case.}
  \item{cexText}{Text size, proportional to standard.}
  \item{xlim}{As in \code{plot}.}
  \item{ylim}{As in \code{plot}.}
  \item{xlab}{As in \code{plot}.}
  \item{ylab}{As in \code{plot}.}
  \item{legendPos}{As in \code{legend}.}
  \item{plotType}{Coded 'l' for lines, 'p' for points.}
}

\details{
A way to display 3-dimensional data in 2 dimensions.  For each plotted
point (x,y), a z value is written in text over the point.  A grouping
variable is also allowed, with different colors used to plot different
groups.

A group (including the entire data in the case of one group) can be
displayed either as a polygonal line, or just as a point cloud.  The
user should experiment with different argument settings to get the most
visually impactful plot.
}

\examples{

\dontrun{

xyzPlot(mtcars[,c(3,6,1)],plotType='l',cexText=0.75)
xyzPlot(mtcars[,c(3,6,1)],plotType='p',cexText=0.75)
xyzPlot(mtcars[,c(3,6,1)],plotType='l',cexText=0.75)
xyzPlot(mtcars[,c(3,6,1,2)],clrs=c('red','darkgreen','blue'),plotType='l',cexText=0.75)

}

}

\author{
Norm Matloff
}

