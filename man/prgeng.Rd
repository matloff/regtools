\name{prgeng}
\alias{prgeng}

\title{
Silicon Valley programmers and engineers
}

\description{
This data set is adapted from the 2000 Census (5\% sample, person
records).  It is restricted to programmers and engineers in the 
Silicon Valley area.

The variable codes, e.g. occupational codes, are available from the Census
Bureau, at
\url{http://www.census.gov/prod/cen2000/doc/pums.pdf}.
(Short code lists are given in the record layout, but longer ones are in
the appendix Code Lists.)

The variables are:

   \itemize{

   \item{\code{age}, with a U(0,1) variate added for jitter}

   \item{\code{cit}, citizenship; 1-4 code various categories of
   citizens; 5 means noncitizen (including permanent residents}

   \item{\code{educ}: 01-09 code no college; 10-12 means some college;
   13 is a bachelor's degree, 14 a master's, 15 a professiona deal and
   16 is a doctorate}

   \item{\code{occ}, occupation}

   \item{\code{birth}, place of birth}

   \item{\code{wageinc}, wage income}

   \item{\code{wkswrkd}, number of weeks worked}

   \item{\code{yrentry}, year of entry to the U.S. (0 for natives)}

   \item{\code{powpuma}, location of work} 

   \item{\code{gender}, 1 for male, 2 for female}

   }
}

\usage{
data(prgeng); prgeng
}

