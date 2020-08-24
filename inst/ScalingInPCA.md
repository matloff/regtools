#  Clearing the Confusion: Scaling in PCA

Many resources on machine learning (ML) methodology recommend, or even
state as crucial, that one *scale* (or *standardize*) one's data, i.e.
divide each variable by its standard deviation (after subtracting the
mean), before applying Principal Component Analysis (PCA).  Here we will
show why that can be problematic, and provide alternatives.

## Overview

The recommendation to scale is common.  Here are some examples: 

* R **prcomp()** man page

    They say "scaling is advisable":

> scale.: a logical value indicating whether the variables should be
>           scaled to have unit variance before the analysis takes place.
>           The default is ‘FALSE’ for consistency with S, but in general
>           scaling is advisable.

* [Scikit-Learn](https://scikit-learn.org/stable/auto_examples/preprocessing/plot_scaling_importance.html):

    Actually, the mention of normal distributions is misleading and in
any case not relevant, but again there is a rather imperative statement
to scale:

> Feature scaling through standardization (or Z-score normalization) can
> be an important preprocessing step for many machine learning algorithms.
> Standardization involves rescaling the features such that they have the
> properties of a standard normal distribution with a mean of zero and a
> standard deviation of one.

* [DataCamp](https://www.datacamp.com/community/tutorials/pca-analysis-r)

Again, their phrasing is rather imperative:

> Note that the units used [in the **mtcars** dataset] vary and occupy
> different scales...You will also set two arguments, center and scale, to
> be TRUE. 

* [caret](https://cran.r-project.org/package=caret), **preProcess** man
  page

    Scaling done unless you say no:

> If PCA is requested but centering and scaling are not, the values will
> still be centered and scaled. 

* [Visually Enforced](https://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)

The word "must" is used here:

> Since most of the times the variables are measured in different scales,
> the PCA must be performed with standardized data (mean = 0, variance =
> 1).

## The perceived problem

As the DataCamp statement notes, some data may be "large" while other
data are "small."  There is a concern that, without scaling, the large
ones will artificially dominate.  This is especially an issue in light
of the variation in measurement systems -- should a variable measured in
kilometers be given more weight than one measured in miles?

## Motivating counterexample

Consider a setting with two independent variables, A and B, with means
100, and with Var(A) = 500 and Var(B) = 2.  Let A' and B' denote these
variables after centering and scaling.

PCA is all about removing variables with small variance, as they are
essentially constant.  If we work with A and B, we would of course use
only A.  **But if we work with A' and B', we would use both of them, as
they both have variance 1.0.**  Scaling has seriously misled us here.

## Alternatives

The real goal should be to make the variables *commensurate*.
Standardizing to mean 0, variance 1 is not the only way one can do this.
Consider the following alternatives.

* Do nothing.  In many data sets, the variables of interest are already
  commensurate.  Consider survey data, say, with each survey question
asking for a response on a scale of 1 to 5.  No need to transform the
data here, and worse, standardizing would have the distoritionary effect
of exaggerating rare values in items with small variance.

* Map each variable to the interval [0,1], i.e. t -> (t-m)/(M-m), where
  m and M are the minimum and maximum values of the given variable.
This is typically better than standardizing, but it does have some
problems.  First, it is sensitive to outliers.  This might be
ameliorated with a modified form of the transformation, but a second
problem is that new data -- new data in prediction applications, say --
may stray from this [0,1] world.

* Instead of changing the *standard deviation* of a variable to 1.0,
  change its *mean* to 1.0.  This addresses the miles-vs.-kilometers
concern more directly, without inducing the distortions I described
above.  And if one is worried about outliers, then divide the variable
by the median or other trimmed mean.


