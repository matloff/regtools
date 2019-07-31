# regtools 

## Novel tools tools for linear, nonlinear and nonparametric regression.

These tools are associated with my book, <i>From Linear
Models to Machine Learning: Modern Statistical Regression and
Classification</i>, N. Matloff, CRC, 2017 (recipient of the
*Technometrics* Eric Ziegel Award for Best Book Reviewed in 2017).

*The tools are useful in general, independently of the book*.

## FEATURES:

* Tools for multiclass classification, parametric and nonparametric, any
  number of classes.  One vs. All and All vs. All paradigms.  Novel
adjustment for artificially balanced (or undesirably imbalanced) data.

* Innovative graphical tools for assessing fit in linear and nonlinear
  parametric models, via nonparametric methods.  Model evaluation,
examination of quadratic effects, investigation of nonhomogeneity of
variance.

* Nonparametric regression for general dimensions in predictor and
  response variables, using k-Nearest Neighbors (k-NN.  Local-linear
option to deal with edge aliasing.  Allows for user-specified smoothing
method.  Allows for accelerated exploration of multiple values of **k**
at once.  Tool to aid in choosing **k**.

* Nicer implementation of ridge regression, with more meaningful scaling
and better plotting.

* Extension to nonlinear parametric regression of Eicker-White
technique to handle heteroscedasticity.

* Misc. tools, e.g. Method of Moments estimation (including for
nonregression settings).

* Utilities for conversion of time series data to rectangular form,
  enabling lagged prediction by **lm** or other regression model.

* Linear regression, PCA and log-linear model estimation in missing-data
setting, via the Available Cases method.

* Utilities for conversion between factor and dummy variable forms,
  useful since among various regression packages, some use factors while
some others use dummies.

## EXAMPLE:  PARAMETRIC MODEL FIT ASSESSMENT

The fit assessment techniques in **regtools** gauge the fit of
parametric models by comparing to nonparametric ones.  Since the latter
are free of model bias, they are very useful in assessing the parametric
models.

Let's take a look at the included dataset **prgeng**, some Census data
for California engineers and programmers in the year 2000. The response
variable in this example is wage income, and the predictors are age,
gender, number of weeks worked, and dummy variables for MS and PhD
degrees.  You can read the details of the data by typing

``` r
> ?prgeng
```

One of the graphical functions for model fit assessment plots the
parametric (e.g. **lm()**) values against the nonparametric fit via
k-NN.  Let's try this on the Census data.

The package includes three versions of the dataset:  The original; a
version with categorical variables in dummy form; and a version with
categorical variables in R factor form.  Since the k-NN routines require
dummies, we'll use that second version, **peDumms**.

We need to generate the parametric and nonparametric fits, then call
**parvsnonparplot()**:

``` r
data(peDumms)
pe1 <- peDumms[c('age','educ.14','educ.16','sex.1','wageinc','wkswrkd')]
lmout <- lm(wageinc ~ .,data=pe1)
xd <- preprocessx(pe1[,-5],10)  # prep for k-NN, k <= 10
knnout <- knnest(pe1$wageinc,xd,10)
parvsnonparplot(lmout,knnout)
```

![result](inst/images/ParVsNonpar.png)

There is quite a bit suggested in this picture:

* There seems to be some overfitting near the low end, and quite
substantial underfitting at the high end.  

* There are intriguing "streaks" or "tails" of points, suggesting the
  possible existence of small but important subpopulations.  Moreover,
one can imagine seeing two separate large subpopulations

* There appear to be a number of people with 0 wage income. Depending on
the goals of our analysis, we might consider removing them.

The package includes various other graphical diagnostic functions.

Finally, let's check the classical assumption of homoscedasticity,
meaning that the conditional variance of Y given X is constant.  The
function <b>nonparvarplot()</b> plots the estimated conditional variance
against the estimated conditional mean, both computed nonparametrically:

![result](inst/images/PrgengVar.png)

Though we ran the plot thinking of the homoscedasticity assumption, this
is much more remarkable, confirming that there are interesting
subpopulations within this data.

By the way, violation of the homoscedasticity assumption won't
invalidate our estimates; they still will be *statistically consistent*.
But the standard errors we compute, and thus the statistical inference
we perform, will be affected.  This is correctible using the
Eicker-White procedure, which for linear models is available in the
**car** and **sandwich** packagers.  Our package here also extends this
to nonlinear parametric models, in our function <b>nlshc()</b> (the
validity of this extension is shown in the book).

## EXAMPLE; OVA VS. AVA IN MULTICLASS PROBLEMS

A very popular prediction method in 2-class problems is to use logistic
(logit) regression. In analyzing click-through patterns of Web users,
for instance, we have 2 classes, Click and Nonclick.  We might fit a
logistic model for Click, given user Web history, demographics and so
on.  Note that logit actually models probabilities, e.g. the probability
of Click given the predictor variables.

But the situation is much less simple in multiclass settings. Suppose
our application is recognition of hand-written digits (a famous machine
learning example). The predictor variables are pixel patterns in images.
There are two schools of thought on this:

* <i>One vs. All (OVA):</i>  We would run 26 logistic regression models,
  one for predicting '0' vs. non-'0', one for '1' vs. non-'1', and so
on.  For a particular image, we would thus obtain 26 estimated
probabilities.  Let i<sub>max</sub> be the image that yields the largest
probability; we would then guess the digit for the image to be 'i'.

* <i>All vs. All (AVA):</i>  Here we would run C(10,2) = 45 logit
analyses, one for each pair of digits.  There would be one for '0' vs.
'1', one for '0' vs. '2', etc., all the way up through '8' vs. '9'.
Many in the machine learning literature recommend AVA over OVA, on the
grounds that might be linearly separable (in the statistical sense) in
pairs but not otherwise.  My book counters by positing that such a
situation could be remedied under OVA by adding quadratic terms to the
logit models.

At any rate, the <strong>regtools</strong> package gives you a choice,
OVA or AVA, for both parametric and nonparametric methods.  For example,
<strong>avalogtrn()</strong> and <strong>avalogpred()</strong> do
training and prediction operations for logit with AVA.

Another feature concerns adjustment of class probabilities.  In many
multiclass data sets, the numbers of points in each class is the same,
or least not reflective of the population class probabilities. In
<strong>regtools</strong>, the user can specify estimates of the latter,
for logit and nonparametric methods.

So, let's look at an example, using the UCI Letter Recognition data set,
another image recognition example.  Again, the code below was preceded
by some data wrangling, which changed the letter data from character to
numeric, and which divided the data set into training and test sets.
Here is the OVA run:

```{r}
> ologout <- ovalogtrn(26,lrtrn[,c(2:17,1)]) 
> ypred <- ovalogpred(ologout,lrtest[,-1]) 
> mean(ypred == lrtest[,1]) 
[1] 0.7193333 
```

So, we get about a 72% rate of correct classification.  Now let's try
AVA:

```{r}
> alogout <- avalogtrn(26,lrtrn[,c(2:17,1)])
> ypred <- avalogpred(26,alogout,lrtest[,-1])
> mean(ypred == lrtest[,1])
[1] 0.8355
```

AVA did considerably better, 84%.  So, apparently AVA fixed a poor
model. But of course, it’s better to make a good model in the first
place. Based on our previous observation that the boundaries may be
better approximated by curves than lines, let's try a quadratic model.

There were 16 predictors, thus 16 possible quadratic terms, and C(16,2)
= 120 possible interaction terms.  Inclusion of all such variables would
probably produce too rich a model for the 14000 points in our training
set.  We'll settle for adding just the squared terms (not shown):

```{r}
> ologout <- ovalogtrn(26,lrtrn[,c(2:33,1)])
> ypred <- ovalogpred(ologout,lrtest[,-1])
> mean(ypred == lrtest[,1])
[1] 0.8086667
```

Ah, much better, though still not quite as good as AVA.

